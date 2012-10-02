package Finance::Bank::NFCU;

use 5.010001;
use strict;
use warnings;
use base qw( WWW::Mechanize );
{
    use Carp;
    use Data::Dumper;
    use DateTime;
    use Env qw( %ENV );
    use Readonly;
    use File::Spec;
    use File::Slurp;
    use Date::Manip::TZ;
    use English qw( -no_match_vars $EVAL_ERROR );
}

our $VERSION = 0.21;

my ( %URL_FOR, $AUTHENTICATED_REGEX, $OFFLINE_REGEX, $ACCT_SUMMARY_REGEX,
     $ACCT_ROW_REGEX, $PAYMENT_REGEX, $ESTATEMENT_URL_REGEX, $ESTATEMENT_ROW_REGEX,
     $ESTATEMENT_PERIOD_REGEX, $TIME_ZONE );
{
    Readonly %URL_FOR => (
        login        => 'https://myaccounts.navyfcu.org/cgi-bin/ifsewwwc?Logon',
        main         => 'https://myaccountsaws.navyfcu.org/nfoaa/main',
        summary      => 'https://myaccountsaws.navyfcu.org/nfoaa/accounts/summary',
        estatements  => 'https://myaccountsaws.navyfcu.org/eStmts/stmtList.do',
        billpay      => 'https://myaccountsaws.navyfcu.org/WBP/wbp.nfo?page=wbpstart',
    );

    Readonly $AUTHENTICATED_REGEX => qr{
        <a \s href= [^>]+ > Log \s off </a>
    }xms;

    Readonly $OFFLINE_REGEX => qr{
        inconvenience
    }xms;

    Readonly $ACCT_SUMMARY_REGEX => qr{
      <tr \s id \s* = \s* "accountTable \w+ ">                         \s*
        <td \s class \s* = \s* "tdText" \s* > \s* ( [^<]+? ) \s* </td> \s*
        <td \s class \s* = \s* "tdText" \s* >                          \s*
            <a .+? href=" ( [^"]+? ) " \s* >  \s* ( [^<]+? ) \s* </a>  \s*
            <a [^>]+ > .+? </a>                                        \s*
        </td>                                                          \s*
        <td \s class \s* = \s* "tdAmt" \s* >  \s* ( [^<]+? ) \s* </td> \s*
      </tr>
    }xms;

    Readonly $ACCT_ROW_REGEX => qr{
        <tr \s id="sort \w+ [^"]+ " [^>]* > \s* ( .+? ) \s* </tr>
    }xms;

    Readonly $ESTATEMENT_URL_REGEX => qr{
        statementType=A
    }xms;

    Readonly $ESTATEMENT_ROW_REGEX => qr/
        ( \d+ - \d+ .+? (?: \n [ ]{6} [^\n]+ )? [.] \d{2} ) \n
    /xms;

    #  STATEMENT PERIOD
    #  From    11-21-08
    #  Through 12-20-08
    #   ACCOUNT NUMBER
    #   00001491962005

    #   STATEMENT PERIOD
    #  05/21/11 - 06/20/11
    #    ACCOUNT NUMBER
    #    00001491962005

    Readonly $ESTATEMENT_PERIOD_REGEX => qr{
        STATEMENT \s+ PERIOD   \s+
        (?: From \s* )?
        ( \d+ \D \d+ \D \d+ ) \s+
        (?: - \s* )?
        (?: Through \s* )?
        ( \d+ \D \d+ \D \d+ )
    }xms;

    Readonly $PAYMENT_REGEX => qr/
        try \s+ {                            \s+
            BizObjRegistry[.]add\('payment', \s+
                ( .+? )                      \s+
            \);                              \s+
        }
    /xms;

    my $tz = Date::Manip::TZ->new();

    Readonly $TIME_ZONE => $tz->zone();
}

# internal helper functions
{
    $Carp::Internal{ (__PACKAGE__) }++;

    my $error_level = 'non-fatal';
    my $now         = time;
    my ( %epoch_for, %date_for, %criterion_for, %item_for );

    sub _set_error_level {
        return $error_level = $_[0];
    }

    sub _parse_config {
        my ( $config_rh, $config_spec_rh ) = @_;

        return
            if !$config_rh;

        my $calling_func = ( caller 1 )[3];

        for my $key (keys %{ $config_rh }) {

            croak "$key is not a supported config option for $calling_func"
                if not exists $config_spec_rh->{$key};
        }

        my @values;

        my @keys = sort keys %{ $config_spec_rh };

        KEY:
        for my $key (@keys) {

            if ( defined $config_rh->{$key} ) {

                my $spec  = $config_spec_rh->{$key};
                my $value = $config_rh->{$key};
                my $type  = ref $spec;

                if ( $type ne 'Regexp' ) {

                    if ( ref $value eq $type ) {

                        push @values, $value;
                        next KEY;
                    }
                    elsif ( defined $value ) {

                        croak "$key is expected to have a ", ( ref $spec ),
                            " type reference";
                    }

                    push @values, $spec;
                    next KEY;
                }

                if ( $value =~ $spec ) {

                    $value = $1;

                    push @values, $value;
                    next KEY;
                }

                croak "$value is not a valid value for $key for $calling_func";
            }
            else {

                push @values, undef;
            }
        }

        return @values
            if wantarray;

        return
            if !@values;

        return $values[0]
            if @keys == 1;

        return \@values;
    }

    sub _trans_cmp {
        my ( $m, $n ) = @_;

        return $m->{epoch} <=> $n->{epoch}
            if $m->{epoch} != $n->{epoch};

        if ( exists $m->{balance} && exists $n->{balance} ) {

            return -1
                if $m->{balance} + $n->{amount} == $n->{balance};

            return 1
                if $n->{balance} + $m->{amount} == $m->{balance};
        }

        return $n->{_sequence} <=> $m->{_sequence}
            if exists $m->{_sequence} && exists $n->{_sequence};

        return $n->{_list_number} <=> $m->{_list_number}
            if exists $m->{_list_number} && exists $n->{_list_number};

        return 0;
    }

    sub _as_dollars {
        for my $rs (@_) {

            my ( $dollars, $cents, $is_negative ) = _parse_money( $rs );

            if ( $dollars ) {

                $dollars = reverse $dollars;
                $dollars ||= '0';
                $dollars =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
                $dollars = scalar reverse $dollars;
            }

            ${$rs} = sprintf '$%s.%02u%s', $dollars,
                ( substr $cents . '00', 0, 2 ),
                ( $is_negative ? '-' : "" );
        }
        return;
    }

    sub _as_cents {
        for my $rs (@_) {

            my ( $dollars, $cents, $is_negative ) = _parse_money( $rs );

            if ( defined $cents ) {

                $cents =~ s{\D}{}xmsg;

                $cents .= '00';

                $cents =
                      $dollars
                    . ( substr $cents, 0, 2 )
                    . '.'
                    . ( substr $cents, 2 );

                $cents *= $is_negative ? -1 : 1;

                ${$rs} = sprintf '%.10f', $cents;
                ${$rs} =~ s{ 0+ \z}{}xmsg;
                ${$rs} =~ s{ \D \z}{}xmsg;
            }
            else {

                ${$rs} = "";
            }
        }
        return;
    }

    sub _parse_money {
        my ( $rs ) = @_;

        if ( !$rs || !defined ${$rs} ) {
            carp 'no value passed to _parse_money';
            return;
        }

        my ($is_dollars)  = ${$rs} =~ s{(\$)}{}xms;
        my ($is_negative) = ${$rs} =~ s{(-)}{}xms;

        my ( $dollars, $cents ) = ( 0 ) x 2;

        if ( ${$rs} =~ m{\A ( [,\d]+ ) ( [.] \d+ ) \z}xms ) {

            ( $dollars, $cents ) = ( $1, $2 );
            $dollars =~ s{,}{}xmsg;

            if ( $is_dollars ) {

                $cents = substr $cents, 1;
            }
            else {

                $cents   = ( substr $dollars, -2 ) . $cents;
                $dollars = substr $dollars, 0, -2;
            }
        }
        elsif ( ${$rs} =~ m{\A ( [,\d]+ ) \z}xms ) {

            if ( $is_dollars ) {

                ( $dollars, $cents ) = ( $1, 0 );

                $dollars =~ s{,}{}xmsg;
            }
            else {

                ( $dollars, $cents ) = ( 0, '0' . $1 . '00' );

                $cents =~ s{,}{}xmsg;

                $dollars = substr $cents, 0, -4;
                $cents   = substr $cents, ( length $dollars );
            }
        }
        else {

            my $line = ( caller 1 )[2];

            warn "line[$line]: unable to parse: ", ${$rs};
            return;
        }

        $dollars =~ s{\A 0+ }{}xmsg;
        $cents   =~ s{ 0+ \z}{}xmsg;

        return ( $dollars, $cents, $is_negative );
    }

    sub _lookahead {
        my ( $seek_rh ) = @_;

        my $epoch       = $seek_rh->{epoch};
        my $amount      = $seek_rh->{amount};
        my $category    = $seek_rh->{category};
        my $list_ra     = $seek_rh->{list_ra};
        my $list_number = $seek_rh->{list_number};

        LIST:
        for my $i ( 0 .. $#{ $list_ra } ) {

            for my $transaction_rh (@{ $list_ra->[$i] }) {

                next LIST
                    if $transaction_rh->{_list_number} == $list_number;

                next LIST
                    if $transaction_rh->{epoch} < $epoch;

                my $amt_cat = join ',', @{ $transaction_rh }{qw( amount category )};

                return 1
                    if "$amount,$category" eq $amt_cat;
            }
        }

        return;
    }

    sub _merge {
        my @transaction_lists = @_;

        my ( @master, @merger );

        my $list_number = 1;

        for my $transaction_ra ( reverse @transaction_lists ) {

            for my $transaction_rh ( reverse @{ $transaction_ra || [] } ) {

                $transaction_rh->{_list_number} = $list_number;

                push @master, $transaction_rh;
            }

            $list_number++;
        }

        @master = sort { _trans_cmp( $a, $b ) } @master;

        my %is_seen;

        TRANS:
        for my $transaction_rh (@master) {

            my ( $epoch, $status, $item, $amount, $category, $list_number ) =
                @{$transaction_rh}
                {qw( epoch status item amount category _list_number )};

            my $key_a = "$amount,$category";
            my $key_b = "$amount,$epoch";

            if ( exists $is_seen{$key_a} || exists $is_seen{$key_b} ) {

                my $key = exists $is_seen{$key_a} ? $key_a : $key_b;

                my $seen_list_number = $is_seen{$key}->{list_number};
                my $seen_epoch       = $is_seen{$key}->{epoch};

                my $same_epoch = 86_401 > abs $epoch - $seen_epoch;
                my $diff_list  = $list_number != $seen_list_number;

                next TRANS
                    if $same_epoch && $diff_list;
            }

            if ( @merger && $status ne 'confirmed' ) {

                my %seek = (
                    epoch       => $epoch,
                    amount      => $amount,
                    category    => $category,
                    list_number => $list_number,
                    list_ra     => \@transaction_lists
                );
                next TRANS
                    if _lookahead( \%seek ); # prevent overlap

                my $last_transaction_rh = $merger[0];

                my $balance = $last_transaction_rh->{balance};

                die "no balance found:", Dumper( $last_transaction_rh )
                    if not defined $balance;

                $balance += $amount;

                $transaction_rh->{balance}     = $balance;
                $transaction_rh->{balance_str} = $balance;

                _as_dollars( \$transaction_rh->{balance_str} );
            }

            $is_seen{$key_a} = {
                list_number => $list_number,
                epoch       => $epoch,
            };

            $is_seen{$key_b} = {
                list_number => $list_number,
                epoch       => $epoch,
            };

            unshift @merger, $transaction_rh;
        }

        return _audit_transaction_list( \@merger, 0 );
    }

    sub _audit_transaction_list {
        my ( $transaction_ra, $reverse_flag ) = @_;

        return $transaction_ra
            if !@{ $transaction_ra || [] };

        if ($reverse_flag) {

            @{$transaction_ra} = reverse @{$transaction_ra};
        }

        my $balance = $transaction_ra->[0]->{balance};

        for my $i ( 0 .. $#{$transaction_ra} ) {

            my $transaction_rh = $transaction_ra->[$i];

            my $balance_str          = sprintf '%0.2f', $balance;
            my $expected_balance_str = sprintf '%0.2f', $transaction_rh->{balance};

            if ( $balance_str != $expected_balance_str ) {

                my $from_i = $i > 2                         ? $i - 3 : 0;
                my $to_i   = $i < $#{ $transaction_ra } - 2 ? $i + 3 : $#{ $transaction_ra };

                my $snippet = Dumper( [ @{ $transaction_ra }[ $from_i .. $to_i ] ] );
                $snippet =~ s/\A .+? ( \s* [{] ) /$1/xms;
                $snippet =~ s/ [}] [^}]+? \z/}/xms;

                my $message =
                    sprintf "transaction discrepancy '%s' (%s != %s) in:\n...%s\n...\n%s\n",
                            $transaction_rh->{item},
                            $balance_str,
                            $expected_balance_str,
                            $snippet,
                            "Perhaps your categorize function need improvement.";

                croak $message
                    if $error_level eq 'fatal';

                warn $message;
            }

            $balance += ( -1 * $transaction_rh->{amount} );
        }

        return $transaction_ra;
    }

    sub _formatted_date {
        my $epoch = defined $_[0] ? $_[0] : $now;

        if ( !exists $date_for{$epoch} ) {

            my $dt;

            if ( $epoch =~ m{\A ( \d{2} ) \D ( \d{2} ) \D ( \d+ ) \z}xms ) {

                my ( $m, $d, $y ) = ( $1, $2, $3 );

                $dt = DateTime->new(
                    year      => $y,
                    month     => $m,
                    day       => $d,
                    hour      => 0,
                    minute    => 0,
                    second    => 0,
                    time_zone => $TIME_ZONE,
                );
            }
            else {

                $dt = DateTime->from_epoch( epoch => $epoch );
            }

            $date_for{$epoch}
                = [ ( split /\D/, $dt->mdy('/') ), lc $dt->day_abbr(), ];
        }

        return @{ $date_for{$epoch} }
            if wantarray;

        return join '/', @{ $date_for{$epoch} }[ 0 .. 2 ];
    }

    sub _compute_epoch {
        my ( $date, $default_epoch ) = @_;

        if ( !$date ) {

            return $default_epoch
                if defined $default_epoch;

            $date = _formatted_date();
        }

        return $epoch_for{$date}
            if exists $epoch_for{$date};

        my ( $m, $d, $y ) = split /\D/, $date;

        if ( $m && !$d && !$y ) {

            my $dt = DateTime->from_epoch( epoch => $m );

            ( $m, $d, $y ) = split /\D/, $dt->mdy('/');
        }

        return
            if !$m || !$d || !$y;

        my $dt = DateTime->new(
            year      => $y,
            month     => $m,
            day       => $d,
            hour      => 0,
            minute    => 0,
            second    => 0,
            time_zone => $TIME_ZONE,
        );

        $epoch_for{$date} = $dt->epoch();

        return $epoch_for{$date};
    }

    sub _tidy_item {
        my $item = $_[0] || "";

        return
            if !$item;

        return $item_for{$item}
            if exists $item_for{$item};

        my $dash_regex = qr{ \s* (?: - \s+ )+ }xms;

        my @debris = (
            qr{ <[^>]+> }xms,
            qr{ POS \s Credit \s Adjustment \s Transaction \s }xmsi,
            qr{ ACH \s Transaction $dash_regex }xmsi,
            qr{ POS \s Debit \s }xmsi,
            qr{ Cucard \s \d+ $dash_regex }xmsi,
            qr{ Cucard \s \d+ \s Transaction }xmsi,
            qr{ Visa \s Check \s Card \s \d+ \s }xmsi,
            qr{ Paid \s to $dash_regex }xmsi,
            qr{ [A-Z]{2} US (?: \b | \z )}xms,
            qr{ \s Chk \s \d+ \z}xms,
            qr{ \s 0jqw \s \d+ \z}xms,
            qr{ \A \s* \d+ - \d+ - \d+ \s }xms,
            qr{ \A $dash_regex }xmsi,
            qr{ (?: \A \s* | \s* \z ) }xms,
        );

        for my $regex (@debris) {

            $item =~ s{ $regex }{}xmsig;
        }

        $item =~ s{&[#]0?39;}{'}xmsig;
        $item =~ s{&amp;}{&}xmsig;
        $item =~ s{ \s+ }{ }xmsig;

        $item_for{ $_[0] } = $item;

        return $item_for{ $_[0] };
    }

    sub _categorize {
        my ( $item, $amount ) = @_;

        if ( !keys %criterion_for ) {

            %criterion_for = (
                'administrative' => [
                    [ qr{ \b dividend \b }xmsi, ],
                ],
                'health' => [
                    [ qr{ \s pharmacy \s }xmsi, ],
                ],
                'charity' => [
                    [ qr{ \s democrats \s }xmsi, ],
                    [ qr{ \s cancer \s }xmsi, ],
                    [ qr{ \s moveon[.]org \s }xmsi, ],
                    [ qr{ \s komen \s }xmsi, ],
                    [ qr{ \s barackobama[.]c }xmsi, ],
                    [ qr{ \s rob \s miller \s }xmsi, ],
                ],
                'entertainment' => [
                    [ qr{ bigstar \W tv }xmsi, ],
                    [ qr{ \W netflix \W }xmsi, ],
                    [ qr{ \s liquor \s }xmsi, ],
                ],
                'gasoline' => [
                    [ qr{ \s shell \s service }xmsi, ],
                    [ qr{ \s exxon (?: mobile )? \s }xmsi, ],
                    [ qr{ \s oil \s }xmsi, ],
                    [ qr{ \s arco \W }xmsi, ],
                    [ qr{ \s costco \s gas \s }xmsi, ],
                ],
                'income' => [
                    [ qr{ \s deposit \s }xmsi, ],
                ],
                'insurance' => [
                    [ qr{ nationwide \W* allied }xmsi, ],
                    [ qr{ \s geico \s }xmsi, ],
                    [ qr{ \s all \W* state \s }xmsi, ],
                ],
                'education' => [
                    [ qr{ \s u[.]s[.] \s dep(?: t[.] | artment ) \s of \s ed }xmsi, ],
                    [ qr{ \s sdccd \s }xmsi, ],
                    [ qr{ \s college \s }xmsi, ],
                ],
                'housing' => [
                    [ qr{ \s mortgage \s }xmsi, ],
                    [ qr{ home \s* loan }xmsi, ],
                ],
                'cell phones' => [
                    [ qr{ \s verizon \s }xmsi, ],
                    [ qr{ \s nextel \s }xmsi, ],
                    [ qr{ \s sprint \s }xmsi, ],
                ],
                'cars' => [
                    [ qr{ \s dmv \W }xmsi, ],
                    [ qr{ \s smog \s }xmsi, ],
                    [ qr{ \s autozone \s }xmsi, ],
                    [ qr{ napa \s store \s }xmsi, ],
                    [ qr{ jiffy \s lube \s }xmsi, ],
                    [ qr{ toyota/lexus \s }xmsi, ],
                ],
                'insurance' => [
                    [ qr{ \s auto \s club \s }xmsi, ],
                    [ qr{ (?: \b | \s ) allied \s insurance }xmsi, ],
                ],
                'internet' => [
                    [ qr{ (?: \b | \s ) cox \s cable \s }xmsi, ],
                    [ qr{ (?: \b | \s ) cox \s communications }xmsi, ],
                ],
                'utilities' => [
                    [ qr{ \s gas \s & (?: amp; )? \s electric \b }xmsi, ],
                ],
                'shopping' => [
                    [ qr{ \W newegg \W }xmsi, ],
                    [ qr{ (?: \b | \s ) home \s depot \s }xmsi, ],
                    [ qr{ \s tech \s 4 \s less \s }xmsi, ],
                    [ qr{ \s best \s buy \s }xmsi, ],
                    [ qr{ \s ikea \s }xmsi, ],
                    [ qr{ \s dixieline \s }xmsi, ],
                    [ qr{ \s costco \s }xmsi, ],
                    [ qr{ \s target \s }xmsi, ],
                    [ qr{ \s amazon (?: [.] com )? \s }xmsi, ],
                ],
                'fast food' => [
                    [ qr{ \s sammy &[#]039; s \s }xmsi, ],
                    [ qr{ \s noodles \s & }xmsi, ],
                    [ qr{ \s jack \s in \s the \s box }xmsi, ],
                    [ qr{ \s islands \s }xmsi, ],
                    [ qr{ \s juice \s blend \s }xmsi, ],
                    [ qr{ \s sushi \s }xmsi, ],
                    [ qr{ \s bistro \s }xmsi, ],
                    [ qr{ \s carljr \d }xmsi, ],
                    [ qr{ \s espresso \s }xmsi, ],
                    [ qr{ \s las \s brasas \s }xmsi, ],
                    [ qr{ \s pizza \s }xmsi, ],
                    [ qr{ \s greek \s }xmsi, ],
                    [ qr{ \s yogurt \s }xmsi, ],
                    [ qr{ \s chipotle \s }xmsi, ],
                    [ qr{ \s cafe \s }xmsi, ],
                    [ qr{ \s pho \s }xmsi, ],
                    [ qr{ \s submarina \s }xmsi, ],
                    [ qr{ \s subway \s }xmsi, ],
                    [ qr{ \s city \s wok \s }xmsi, ],
                    [ qr{ \s starbucks \s }xmsi, ],
                    [ qr{ \s sbux \s }xmsi, ],
                ],
                'groceries' => [
                    [ qr{ \s 99 \s ranch \s }xmsi, ],
                    [ qr{ \s ranch \s market \s }xmsi, ],
                    [ qr{ \s seafood \s city \s  }xmsi, ],
                    [ qr{ \s supermarket \s }xmsi, ],
                    [ qr{ \s marukai \s }xmsi, ],
                    [ qr{ \s ralph (?: &[#]039; )? s \s }xmsi, ],
                    [ qr{ \s mitsuwa \s }xmsi, ],
                    [ qr{ \s fresh \s &amp; \s easy \s }xmsi, ],
                    [ qr{ \s nijiya \s }xmsi, ],
                    [ qr{ \s henrys \s }xmsi, ],
                    [ qr{ \s vons \s }xmsi, ],
                    [ qr{ \s albertsons \s }xmsi, ],
                ],
            );
        }

        for my $category (keys %criterion_for) {

            CRIT:
            for my $criterion_ra (@{ $criterion_for{$category} }) {

                my ( $regex, $range_ra ) = @{ $criterion_ra };

                if ( $range_ra ) {

                    my ( $min, $max ) = sort { $a <=> $b } @{ $range_ra };

                    next CRIT
                        if $amount < $min || $max < $amount;
                }

                return $category
                    if $item =~ $regex;
            }
        }

        return 'uncategorized';
    }

    sub _cache_filename {
        my ( $cache_dir, $url, $persistence ) = @_;

        return
            if !$cache_dir;

        $url =~ s{\A https?:// [^/]+ /}{}xms;
        $url =~ s{\W}{.}xmsg;

        $persistence ||= 'hour';

        if ( $persistence eq 'indefinite' ) {

            $url .= '.ind';
        }
        else {

            $url .= '.';

            if ( $persistence eq 'hour' ) {

                $url .= join '.', (localtime)[ 5, 7, 2 ];
            }
            elsif ( $persistence eq 'month' ) {

                $url .= join '.', (localtime)[ 5, 7 ];
            }
        }

        $url .= '.html';

        return File::Spec->catfile( $cache_dir, $url );
    }

    sub _fetch_cache {
        my ( $filename ) = @_;

        return
            if !$filename || !stat $filename;

        return File::Slurp::slurp( $filename );
    }
}

sub new {
    my ( $class, $option_rh ) = @_;

    croak "no option hash given"
        if !$option_rh || ref $option_rh ne 'HASH';

    for my $cred (qw( access_number user_id password )) {

        croak "the $cred parameter is missing"
            if !$option_rh->{$cred};
    }

    my $access_number = delete $option_rh->{access_number};
    my $user_id       = delete $option_rh->{user_id};
    my $password      = delete $option_rh->{password};

    my $cookie_file
        = exists $option_rh->{cookie_file}
        ? delete $option_rh->{cookie_file}
        : "";

    my %default_for = (
        stack_depth    => 1, # lower memory consumption
        agent          => __PACKAGE__ . '/' . $VERSION,
    );
    for my $key (keys %default_for) {

        if ( !exists $option_rh->{$key} ) {

            $option_rh->{$key} = $default_for{$key};
        }
    }

    my $self = bless $class->SUPER::new( %{ $option_rh } ), $class;

    if ( $cookie_file ) {

        $self->cookie_jar( {
            file           => $cookie_file,
            autosave       => 1,
            ignore_discard => 1,
        } );
    }

    $self->get( $URL_FOR{main} );

    if (  !$self->success()
        || $self->content() !~ $AUTHENTICATED_REGEX )
    {

        $self->get( $URL_FOR{login} );

        croak "failed to get: ", $URL_FOR{login}
            if !$self->success();

        my %form = (
            'Submit'           => 'Sign+on',
            'passwrd'          => $password,
            'userid'           => $user_id,
            'prevcmd'          => '',
            'token'            => '0',
            'comboLogonNumber' => $access_number,
            'nls'              => 'EN'
        );
        $self->post( $URL_FOR{login}, \%form );

        return
            if !$self->success();

        croak "NFCU online banking is off line\n"
            if $self->content() =~ $OFFLINE_REGEX;

        $self->get( $URL_FOR{main} );

        return
            if !$self->success();

        return
            if $self->content() !~ $AUTHENTICATED_REGEX;
    }

    return $self;
}

sub config {
    my ( $self, $config_rh ) = @_;

    my @options = qw(
        cache_override
        cache_dir
        categorize_rc
        tidy_rc
        error_level
    );

    for my $option (@options) {

        if ( exists $config_rh->{$option} ) {

            $self->{"__$option"} = delete $config_rh->{$option};
        }
    }

    my $error_level = $self->{__error_level} || 'non-fatal';

    carp "error_level should be 'fatal' or 'non-fatal'"
        if $error_level !~ m{\A (?: non- )? fatal \z}xms;

    _set_error_level( $error_level );

    my $cache_dir = $self->{__cache_dir};

    if ($cache_dir) {

        if ( !-d $cache_dir ) {

            carp "not a directory: $cache_dir";
            $cache_dir = 0;
        }
        elsif ( !-w $cache_dir ) {

            carp "no write permission: $cache_dir";
            $cache_dir = 0;
        }
    }

    for my $option ( keys %{$config_rh} ) {

        carp "$option is not a supported config option";
    }

    return;
}

sub get_balances {
    my ($self) = @_;

    my @balances;

    TRY:
    for ( 1 .. 3 ) {

        $self->get( $URL_FOR{summary} );

        if ( $self->success() ) {

            my $html = $self->content();

            $self->{__account_url_rh} = {};

            while ( $html =~ m{ $ACCT_SUMMARY_REGEX }xmsg ) {

                my %balance = (
                    account_number => $1,
                    detail_url     => $2,
                    account        => $3,
                    balance        => $4,
                    balance_str    => $4,
                );

                _as_cents( \$balance{balance} );

                push @balances, \%balance;

                $self->{__nfcu_account_url_rh}->{ lc $balance{account} } = \%balance;
            }

            last TRY
                if @balances;
        }
        else {

            $self->get( $URL_FOR{main} );

            return
                if !$self->success()
                    || $self->content() !~ $AUTHENTICATED_REGEX;
        }
    }

    return \@balances;
}

sub get_transactions {
    my ( $self, $config_rh ) = @_;

    my $billpay_ra = $self->get_billpay_transactions(
        {    status_ra => [qw( pending paid )],
             account   => $config_rh->{account},
        }
    );

    my $recent_ra     = $self->get_recent_transactions( $config_rh );
    my $estatement_ra = $self->get_estatement_transactions( $config_rh );

    return _merge( $billpay_ra, $recent_ra, $estatement_ra );
}

sub get_recent_transactions {
    my ( $self, $config_rh ) = @_;

    my ( $account, $from_date, $from_epoch, $to_date, $to_epoch )
        = _parse_config(
        $config_rh,
        {   account    => qr{\A ( \w+ \s \w+ ) \z}xms,
            from_date  => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            from_epoch => qr{\A ( \d+ ) \z}xms,
            to_date    => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            to_epoch   => qr{\A ( \d+ ) \z}xms,
        }
        );

    if ( $from_date && !$from_epoch ) {

        $from_epoch = _compute_epoch($from_date);
    }

    if ( $to_date && !$to_epoch ) {

        $to_epoch = _compute_epoch($to_date);
    }

    $account    ||= 'EveryDay Checking';
    $from_epoch ||= 0;
    $to_epoch   ||= time + 7 * 86_400;

    my $cache_override = $self->{__cache_override};
    my $cache_dir      = $self->{__cache_dir};
    my $categorize_rc  = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc        = $self->{__tidy_rc} || \&_tidy_item;

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $account_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    if ( !$account_rh ) {

        my @accounts = keys %{ $self->{__nfcu_account_url_rh} };

        croak "account '$account' is not found among ",
            ( join ', ', @accounts );
    }

    my $account_number = $account_rh->{account_number};
    my $account_url    = $account_rh->{detail_url};

    my ( $filename, $html ) = ( "" ) x 2;

    if ( !$cache_override ) {

        $filename = _cache_filename( $cache_dir, $account_url, 'hour' );

        $html = _fetch_cache($filename);
    }

    if ( !$html ) {

        $self->get($account_url);

        return
            if !$self->success();

        $filename ||= _cache_filename( $cache_dir, $account_url, 'hour' );

        if ($filename) {

            $self->save_content($filename);
        }

        $html = $self->content();
    }

    my $transaction_ra
        = _parse_recent( $html, $to_epoch, $from_epoch, $categorize_rc,
        $tidy_rc );

    return _audit_transaction_list( $transaction_ra, 0 );
}

sub get_estatement_transactions {
    my ( $self, $config_rh ) = @_;

    my ( $account ) = _parse_config(
        $config_rh,
        {   account    => qr{\A ( \w+ \s \w+ ) \z}xms,
        }
    );

    $account ||= 'EveryDay Checking';

    my $cache_dir     = $self->{__cache_dir};
    my $categorize_rc = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc       = $self->{__tidy_rc} || \&_tidy_item;

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $account_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    if ( !$account_rh ) {

        my @accounts = keys %{ $self->{__nfcu_account_url_rh} };

        croak "account '$account' is not found among ",
            ( join ', ', @accounts );
    }

    my $account_number = $account_rh->{account_number};

    $self->get( $URL_FOR{estatements} );

    return
        if !$self->success();

    my $date_cmp_rc = sub {
        my ( $a, $b ) = @_;
        my ( $ma, $da, $ya ) = split /\D/, $a;
        my ( $mb, $db, $yb ) = split /\D/, $b;

        if (   !defined $da
            || !defined $db
            || !defined $ma
            || !defined $mb
            || !defined $ya
            || !defined $yb )
        {
            warn "estatement link text comparison ${a}::${b} looks weird";

            for my $rs ( \$da, \$db, \$ma, \$mb, \$ya, \$yb ) {

                ${$rs} ||= 0;
            }
        }

        return $ya <=> $yb
            if $ya != $yb;
        return $ma <=> $mb
            if $ma != $mb;
        return $da <=> $db;
    };

    my @links =
        sort { $date_cmp_rc->( $a->text(), $b->text() ) }
        grep { $_->text() }
        $self->find_all_links( url_regex => $ESTATEMENT_URL_REGEX );

    my $account_regex = qr{
        \s* -+ \s* $account_number \s+
        ( .+? )
        \d+ - \d+ \s+ Ending \s Balance
    }xmsi;

    my @transactions;

    my $sequence = 0;

    LINK:
    for my $link (@links) {

        my ( $m, $d, $y ) = $link->text() =~ m{( \d+ )\D( \d+ )\D( \d+ )}xms;

        my $filename = _cache_filename( $cache_dir, "estatement_$m.$d.$y",
            'indefinite' );

        my $html = _fetch_cache($filename);

        if ( !$html ) {

            $self->get($link);

            die "failed to get ", $link->url()
                if !$self->success();

            if ($filename) {

                $self->save_content($filename);
            }

            $html = $self->content();
        }

        my @period_dates = $html =~ $ESTATEMENT_PERIOD_REGEX;

        die "failed to parse period dates from: $filename:\n",
            $link->url(), " ($filename)"
            if !@period_dates;

        my ($body) = $html =~ $account_regex;

        die "failed to parse $account - $account_number body from ",
            $link->url(), " ($filename)"
            if !$body;

        my %year_for;

        for my $period_date (@period_dates) {

            my ( $m, $d, $y ) = split /\D/, $period_date;

            $year_for{$m} = $y < 100 ? $y + 2000 : $y;
        }

        $body =~ s{ \s+? \n}{\n}xmsg;

        ROW:
        while ( $body =~ m{ $ESTATEMENT_ROW_REGEX }xmsg ) {

            my $row = $1;

            next ROW
                if $row =~ m{ beginning \s+ balance }xmsi;

            $row =~ s{ \s*? \n \s* }{ }xmsg;

            my ( $month_day_item, $amount_str, $balance_str )
                = $row =~ m/\A ( .+? ) \s{3,} ( \S+ ) \s{3,} ( \S+ ) \z/xms;

            die "ROW:$row:\n"
                if not defined $amount_str;

            my ( $month, $day, $item )
                = $month_day_item =~ m{\A ( \d+ )-( \d+ ) \s ( .+ ) }xms;

            $item =~ s{(?: \A \s* | \s* \z )}{}xmsg;

            my $date = sprintf '%0.2d/%0.2d/%d', $month, $day,
                $year_for{$month};
            my $epoch = _compute_epoch($date);

            ( $amount_str, $balance_str )
                = ( '$' . $amount_str, '$' . $balance_str );

            my ( $amount, $balance ) = ( $amount_str, $balance_str );

            _as_cents( \$amount, \$balance );

            my $category = $categorize_rc->( $item, $amount );

            croak
                "categorize function didn't return a category for: $item ($amount)"
                if !defined $category;

            my %transaction = (
                amount      => $amount,
                amount_str  => $amount_str,
                balance     => $balance,
                balance_str => $balance_str,
                category    => $category,
                date        => $date,
                epoch       => $epoch,
                item        => $tidy_rc->($item),
                status      => 'confirmed',
            );
            push @transactions, \%transaction;
        }
    }

    return _audit_transaction_list( \@transactions, 'reverse' );
}

sub get_billpay_transactions {
    my ( $self, $config_rh ) = @_;

    $config_rh ||= {};

    my ( @transactions, %target );

    if ( defined $config_rh->{status} ) {

        $target{ lc $config_rh->{status} } = 1;
    }
    elsif ( defined $config_rh->{status_rh} ) {

        %target = map { ( lc $_ => 1 ) } keys %{ $config_rh->{status_rh} };
    }
    elsif ( defined $config_rh->{status_ra} ) {

        %target = map { ( lc $_ => 1 ) } @{ $config_rh->{status_ra} };
    }
    else {

        $target{pending} = 1;
        $target{paid}    = 1;
    }

    my $account        = $config_rh->{account} || 'EveryDay Checking';
    my $cache_override = $self->{__cache_override};
    my $cache_dir      = $self->{__cache_dir};
    my $categorize_rc  = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc        = $self->{__tidy_rc} || \&_tidy_item;
    my $billpay_url    = $URL_FOR{billpay};

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $balance_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    if ( !$balance_rh ) {

        my @accounts = keys %{ $self->{__nfcu_account_url_rh} };

        croak "account '$account' is not found among ",
            ( join ', ', @accounts );
    }

    $account = $balance_rh->{account};

    my ( $filename, $html ) = ( "" ) x 2;

    if ( !$cache_override ) {

        $filename = _cache_filename( $cache_dir, $billpay_url, 'hour' );

        $html = _fetch_cache($filename);
    }

    if ( !$html ) {

        $self->get( $billpay_url );

        return
            if !$self->success();

        $filename ||= _cache_filename( $cache_dir, $billpay_url, 'hour' );

        if ($filename) {

            $self->save_content($filename);
        }

        $html = $self->content();
    }

    PAYMENT:
    while ( $html =~ m{ $PAYMENT_REGEX }xmsg ) {

        my $hash_str = $1;

        # {
        #     upperName              => 'GE MONEY BANK',
        #     amount                 => '$270.00',
        #     feeamount              => '',
        #     MMDD_dateToPay         => '01/05/12',
        #     MMDDYYYY_dateToPay     => '01/05/2012',
        #     status                 => 'Pending',
        #     numericStatus          => '1',
        #     id                     => '000000000000027',
        #     pmtId                  => '20111203025714384119',
        #     mobilePaymentIndicator => 'N',
        #     mobilePaymentAltText   => ''
        # }
        $hash_str =~ s{ \s ( \w+ ) : \s '}{ $1 => '}xmsg;

        my $payment_rh = eval $hash_str;

        if ( $EVAL_ERROR ) {

            warn $EVAL_ERROR;
            next PAYMENT;
        }

        my $status = lc $payment_rh->{status};

        next PAYMENT
            if not $target{$status};

        $payment_rh->{amount} .= '-';

        my $amount     = $payment_rh->{amount};
        my $amount_str = $payment_rh->{amount};
        my $date       = $payment_rh->{MMDDYYYY_dateToPay};
        my $epoch      = _compute_epoch( $date );
        my $item       = $payment_rh->{upperName};

        if ( $amount =~ m{\A \D+ \z}xms ) {

            $amount     = 0;
            $amount_str = '$0.00';
        }
        else {

            _as_cents( \$amount );
        }

        my $category = $categorize_rc->( $item, $amount );

        croak "categorize function didn't return a category for: $item ($amount)"
            if !defined $category;

        $status = $status eq 'paid' ? 'pending' : $status;    # work-around

        my %transaction = (
            amount      => $amount,
            amount_str  => $amount_str,
            category    => $category,
            date        => $date,
            epoch       => $epoch,
            item        => $tidy_rc->( $item ),
            status      => $status,
        );
        push @transactions, \%transaction;
    }

    return \@transactions;
}

sub get_expenditure_report {
    my ( $self, $config_rh ) = @_;

    my ( $account, $from_date, $from_epoch, $to_date, $to_epoch ) = _parse_config(
        $config_rh,
        {   account    => qr{\A ( \w+ (?: \s \w+ )* ) \z}xms,
            from_date  => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            from_epoch => qr{\A ( \d+ ) \z}xms,
            to_date    => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            to_epoch   => qr{\A ( \d+ ) \z}xms,
        }
    );

    if ( $from_date && !$from_epoch ) {

        $from_epoch = _compute_epoch( $from_date );
    }

    if ( $to_date && !$to_epoch ) {

        $to_epoch = _compute_epoch( $to_date );
    }

    $account    ||= 'EveryDay Checking';
    $from_epoch ||= 0;
    $to_epoch   ||= time + 7 * 86_400;

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %total_for;
    {
        my $transaction_ra = $self->get_transactions( { account => $account } );

        TRANS:
        for my $transaction_rh (@{ $transaction_ra }) {

            my $amount   = $transaction_rh->{amount};
            my $category = $transaction_rh->{category};
            my $epoch    = $transaction_rh->{epoch};

            next TRANS
                if $epoch < $from_epoch;

            next TRANS
                if $to_epoch && $epoch > $to_epoch;

            $first_epoch ||= $epoch;
            $last_epoch = $epoch;

            $total_for{$category} += $amount;
        }
    }

    my $total_time = $last_epoch - $first_epoch;

    my $days   = $total_time / 86_400;
    my $weeks  = $total_time / ( 7 * 86_400 );
    my $months = $total_time / ( 30 * 86_400 );

    my @report = ([qw(
        category
        total
        monthly_average
        weekly_average
    )]);

    my @categories = sort { $total_for{$a} <=> $total_for{$b} } keys %total_for;

    for my $category (@categories) {

        my $polarity = $category =~ m{ income }xms ? 1 : -1;

        my $total             = $total_for{$category};
        my $monthly_average   = $total / $months;
        my $weekly_average    = $total / $weeks;

         _as_dollars( \$total, \$monthly_average, \$weekly_average );

        push @report, [
            $category,
            $total,
            $monthly_average,
            $weekly_average,
        ];
    }

    return \@report;
}

sub get_transaction_schedule {
    my ( $self, $config_rh ) = @_;

    my ( $from_date, $from_epoch, $to_date, $to_epoch ) = _parse_config(
        $config_rh,
        {   from_date  => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            from_epoch => qr{\A ( \d+ ) \z}xms,
            to_date    => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            to_epoch   => qr{\A ( \d+ ) \z}xms,
        }
    );

    if ( $from_date && !$from_epoch ) {

        $from_epoch = _compute_epoch( $from_date );
    }

    if ( $to_date && !$to_epoch ) {

        $to_epoch = _compute_epoch( $to_date );
    }

    $from_epoch ||= 0;
    $to_epoch   ||= time + 7 * 86_400;

    my $tidy_rc = $self->{__tidy_rc} || \&_tidy_item;

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %schedule;
    {
        my $transaction_ra = $self->get_transactions();

        TRANS:
        for my $transaction_rh (@{ $transaction_ra }) {

            my $amount   = $transaction_rh->{amount};
            my $category = $transaction_rh->{category};
            my $epoch    = $transaction_rh->{epoch};
            my $item     = $tidy_rc->( $transaction_rh->{item} );

            next TRANS
                if $epoch < $from_epoch;

            next TRANS
                if $to_epoch && $epoch > $to_epoch;

            $first_epoch = $epoch;
            $last_epoch ||= $epoch;

            my $last_occur_epoch = $schedule{$category}->{last_occur_epoch};

            if ( $epoch > time ) {

                $schedule{$category}->{next_occur_epoch} = $epoch;
            }
            else {

                if ( $schedule{$category}->{last_occur_epoch}
                    && !$schedule{$category}->{interval} )
                {
                    $schedule{$category}->{interval}
                        = ( $schedule{$category}->{last_occur_epoch} - $epoch ) / 86_400;
                }

                $schedule{$category}->{last_occur_epoch} ||= $epoch;
            }

            my ( $m, $d ) = _formatted_date( $epoch );

            $schedule{$category}->{total} += $amount;
            $schedule{$category}->{occurrence_count}++;
            $schedule{$category}->{day_of_month_rh}->{ int $d }++;
            $schedule{$category}->{item_rh}->{ lc $item }++;
        }
    }

    my $total_time = $last_epoch - $first_epoch;

    my $days   = $total_time / 86_400;
    my $weeks  = $total_time / ( 7 * 86_400 );
    my $months = $total_time / ( 30 * 86_400 );

    for my $category ( keys %schedule ) {

        my $event_rh = $schedule{$category};

        my $count      = $event_rh->{occurrence_count} || 0;
        my $last_epoch = $event_rh->{last_occur_epoch} || 0;
        my $total      = $event_rh->{total} || 0;
        my $interval   = $event_rh->{interval} || 0;

        my $ave_amount   = $total / $count;
        my $ave_interval = sprintf '%d', $days / $count;
        my $monthly_ave  = $total / $months;
        my $weekly_ave   = $total / $weeks;

        my $day_of_month_rh = delete $event_rh->{day_of_month_rh};

        DOM:
        {
            my @doms = keys %{ $day_of_month_rh };

            if ( @doms == 1 ) {

                ( $event_rh->{day_of_month} ) = values %{ $day_of_month_rh };
            }
            elsif ( @doms ) {

                for my $dom (@doms) {

                    if ( $count == $day_of_month_rh->{$dom} ) {

                        $event_rh->{day_of_month} = $dom;

                        last DOM;
                    }
                }

                @doms =
                    map { int $_ }
                    sort { int $day_of_month_rh->{$b} <=> int $day_of_month_rh->{$a} }
                    @doms;

                my $range = $doms[0] - $doms[ $#doms ];

                if ( $range < 4 ) {

                    $event_rh->{day_of_month} = $doms[0];
                }
                elsif ( $ave_interval > 27 && $ave_interval < 34 ) {

                    my $sum = 0;
                    map { $sum += $_ } @doms;

                    $event_rh->{day_of_month} = int $sum / ( $#doms + 1 );
                }
                else {

                    $event_rh->{day_of_month_ra} = \@doms;
                }
            }
        }

        if ( exists $event_rh->{next_occur_epoch} ) {

            $event_rh->{next_occur_date} = _formatted_date( $event_rh->{next_occur_epoch} );
        }

        $event_rh->{last_occur_epoch} = $last_epoch;
        $event_rh->{last_occur_date}  = _formatted_date($last_epoch);
        $event_rh->{ave_amount}       = $ave_amount;
        $event_rh->{ave_interval}     = $ave_interval;
        $event_rh->{interval}         = int $interval;
        $event_rh->{monthly_ave}      = $monthly_ave;
        $event_rh->{weekly_ave}       = $weekly_ave;
    }

    return \%schedule;
}

sub get_future_transactions {
    my ( $self, $config_rh ) = @_;

    my ( $account, $from_date, $from_epoch, $schedule_rh, $to_date, $to_epoch ) = _parse_config(
        $config_rh,
        {   account     => qr{\A ( \w+ \s \w+ ) \z}xms,
            from_date   => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            from_epoch  => qr{\A ( \d+ ) \z}xms,
            schedule_rh => {},
            to_date     => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            to_epoch    => qr{\A ( \d+ ) \z}xms,
        }
    );

    if ( $from_date && !$from_epoch ) {

        $from_epoch = _compute_epoch( $from_date );
    }

    if ( $to_date && !$to_epoch ) {

        $to_epoch = _compute_epoch( $to_date );
    }

    $account    ||= 'EveryDay Checking';
    $from_epoch ||= 0;
    $to_epoch   ||= time + ( 365 * 86_400 );

    croak "no schedule data given"
        if not keys %{ $schedule_rh };

    my $transaction_ra = $self->get_transactions( { account => $account } );

    my %last_occur_for;

    TRANS:
    for my $transaction_rh ( @{$transaction_ra} ) {

        my $category = $transaction_rh->{category};
        my $epoch    = $transaction_rh->{epoch};

        $last_occur_for{$category} ||= $epoch;

        for my $category ( keys %{$schedule_rh} ) {

            next TRANS
                if !exists $last_occur_for{$category};
        }

        last TRANS;
    }

    my @transactions;

    my $epoch = _compute_epoch( $from_epoch );

    while ( $epoch < $to_epoch ) {

        my ( $m, $d, $y, $dow ) = _formatted_date($epoch);

        my $date = "$m/$d/$y";

        CAT:
        for my $category ( keys %{$schedule_rh} ) {

            my $last_occur_epoch = $last_occur_for{$category};

            my $days_elapsed = 0;

            if ( defined $last_occur_epoch ) {

                $days_elapsed = ( $epoch - $last_occur_epoch ) / 86_400;
            }

            my $desc_rh = $schedule_rh->{$category};

            my $amount
                = exists $desc_rh->{amount}
                ? $desc_rh->{amount}
                : $desc_rh->{ave_amount};

            if ( !defined $amount ) {
                carp "skipping $category -- no amount value given";
                next CAT;
            }

            _as_cents( \$amount );

            my $amount_str = $amount;

            _as_dollars( \$amount_str );

            my $item = sprintf '%s (%d days since %s)',
                $category, $days_elapsed,
                scalar _formatted_date( $last_occur_epoch || $epoch );

            $desc_rh->{has_day} ||= {};

            if ( exists $desc_rh->{on} ) {

                if ( ref $desc_rh->{on} ne 'ARRAY' ) {

                    $desc_rh->{on} = [ $desc_rh->{on} ];
                }

                ON:
                for my $on (@{ $desc_rh->{on} }) {

                    if ( $date eq _formatted_date( $on ) ) {

                        $desc_rh->{has_day}->{$date} = 1;
                        $last_occur_for{$category} = $epoch;

                        my %transaction = (
                            amount     => $amount,
                            amount_str => $amount_str,
                            category   => $category,
                            date       => $date,
                            epoch      => $epoch,
                            item       => $item,
                            status     => 'predicted',
                        );
                        push @transactions, \%transaction;

                        last ON;
                    }
                }
            }
            elsif ( exists $desc_rh->{day_of_month} ) {

                if ( ref $desc_rh->{day_of_month} ne 'ARRAY' ) {

                    $desc_rh->{day_of_month} = [ $desc_rh->{day_of_month} ];
                }

                my $is_significant = 0;

                my $frequency = @{ $desc_rh->{day_of_month} };

                # [ 1 ]     => 31 + 1   => 32
                # [ 1, 15 ] => 15.5 + 2 => 17.5
                my $max_days = ( 31 / $frequency ) + $frequency;

                # [ 1 ]     => 31 - 1   => 30
                # [ 1, 15 ] => 15.5 - 2 => 13.5
                my $min_days = ( 31 / $frequency ) - $frequency;

                DAY:
                for my $day ( @{ $desc_rh->{day_of_month} } ) {

                    next DAY
                        if exists $desc_rh->{pre_weekend}
                            && ( $dow eq 'sat' || $dow eq 'sun' );

                    if ($day == $d
                        || (   exists $desc_rh->{pre_weekend}
                            && $dow eq 'fri'
                            && ( $day - 1 == $d || $day - 2 == $d ) )
                        )
                    {
                        next DAY
                            if $days_elapsed
                                && $days_elapsed < $min_days;

                        $desc_rh->{has_day}->{$date} = 1;
                        $last_occur_for{$category} = $epoch;

                        my %transaction = (
                            amount     => $amount,
                            amount_str => $amount_str,
                            category   => $category,
                            date       => $date,
                            epoch      => $epoch,
                            item       => $item,
                            status     => 'predicted',
                        );
                        push @transactions, \%transaction;

                        $is_significant = 1;

                        last DAY;
                    }
                }

                if ( !$is_significant ) {

                    if (   $days_elapsed
                        && $days_elapsed > $max_days )
                    {
                        $desc_rh->{has_day}->{$date} = 1;
                        $last_occur_for{$category} = $epoch;

                        $days_elapsed ||= 0;

                        my %transaction = (
                            amount     => $amount,
                            amount_str => $amount_str,
                            category   => $category,
                            date       => $date,
                            epoch      => $epoch,
                            item       => $item,
                            status     => 'predicted',
                        );
                        push @transactions, \%transaction;

                        $is_significant = 1;
                    }
                }
            }
            elsif ( exists $desc_rh->{interval} ) {

                next CAT
                    if !$days_elapsed && $desc_rh->{interval} > 1;

                next CAT
                    if exists $desc_rh->{pre_weekend}
                        && ( $dow eq 'sat' || $dow eq 'sun' );

                if ($days_elapsed >= $desc_rh->{interval}
                    || (   exists $desc_rh->{pre_weekend}
                        && $dow eq 'fri'
                        && (   $days_elapsed + 1 == $desc_rh->{interval}
                            || $days_elapsed + 2 == $desc_rh->{interval} )
                    )
                    )
                {
                    $desc_rh->{has_day}->{$date} = 1;
                    $last_occur_for{$category} = $epoch;

                    my %transaction = (
                        amount     => $amount,
                        amount_str => $amount_str,
                        category   => $category,
                        date       => $date,
                        epoch      => $epoch,
                        item       => $item,
                        status     => 'predicted',
                    );
                    push @transactions, \%transaction;
                }
            }
        }

        $epoch += 86_400;
    }

    return _merge( $transaction_ra, \@transactions );
}

sub _parse_recent {
    my ( $html, $to_epoch, $from_epoch, $categorize_rc, $tidy_rc ) = @_;

    my @transactions;

    ROW:
    while ( $html =~ m{ $ACCT_ROW_REGEX }xmsg ) {

        my $row = $1;

        my @data = $row =~ m{<td [^>]* > \s* ( .+? ) \s* </td>}xmsg;

        die "couldn't parse row:$row\n"
            if !@data;

        next ROW
            if !defined $data[1] || !defined $data[3];

        for my $i ( 0 .. $#data ) {

            $data[$i] =~ s{<[^>]+>}{}xmsg;
        }

        $data[0] =~ tr/A-Z/a-z/;

        my $status = $data[0] eq 'pending' ? 'pending'         : 'confirmed';
        my $date   = $data[0] eq 'pending' ? _formatted_date() : $data[0];
        my $item   = $data[1];
        my $amount_str  = $data[2];
        my $balance_str = $data[3];
        my $epoch       = _compute_epoch($date);

        next ROW
            if !$amount_str || $amount_str =~ m{\A \d+ \z}xms;

        next ROW
            if $to_epoch && $epoch > $to_epoch;

        last ROW
            if $from_epoch && $epoch <= $from_epoch;

        my ( $amount, $balance ) = ( $amount_str, $balance_str );

        _as_cents( \$amount, \$balance );

        my $category = $categorize_rc->( $item, $amount );

        croak "categorize function didn't return a category for: $item ($amount)"
            if !defined $category;

        my %transaction = (
            amount      => $amount,
            amount_str  => $amount_str,
            balance     => $balance,
            balance_str => $balance_str,
            category    => $category,
            date        => $date,
            epoch       => $epoch,
            item        => $tidy_rc->($item),
            status      => $status,
        );

        push @transactions, \%transaction;
    }

    return \@transactions;
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU - Module for accessing data in your Navy Federal Credit
Union accounts.

=head1 SYNOPSIS

  use Finance::Bank::NFCU;

  my %credentials = (
      access_number => '111111111111',
      user_id       => '1234',
      password      => '*********',
  );
  my $ncfu = Finance::Bank::NFCU->new( \%credentials )
      || die "failed to authenticate";

  $nfcu->config(
      {   cache_dir     => '/var/cache/nfcu',
          tidy_rc       => \&tidy_function,
          categorize_rc => \&categorize_function,
          error_level   => 'non-fatal', # fatal or non-fatal
      }
  );

  my $balances_ra = $nfcu->get_balances();

  die "Your session has (probably) expired."
      if !defined $balances_ra;

  for my $balance_rh (@{ $balances_ra }) {

      my $number  = $balance_rh->{account_number};
      my $desc    = $balance_rh->{description};
      my $dollars = $balance_rh->{balance};

      print "$number, $desc -- $dollars\n";
  }

  my $transaction_ra = $nfcu->get_transactions(
      {   account    => 'EveryDay Checking',
          from_epoch => ( time - 90 * 86_400 ),
      }
  );

  for my $transaction_rh (@{ $transaction_ra }) {

      my ( $date, $item, $amount ) = @{ $transaction_rh }{qw( date item amount )};

      print "$date -- $item -- $amount\n";
  }

  my $report_ra = $nfcu->get_expenditure_report();

  my $tb = Text::Table->new( @{ shift @{ $report_ra } } );

  $tb->load( @{ $report_ra } );

  print $tb;

=head1 ADVISORY

This module is designed to interact with your online
banking service at Navy Federal Credit Union. You are fully
responsible for the actions taken with this module and you are
expected to audit the code yourself to be sure that the
security, accuracy and quality is up to your expectations.

The author cannot assume responsibility for any untoward or
nefarious activities attempted with this software.

Don't leave your financial passwords, access numbers or
user IDs hard coded in your programs.

=head1 DESCRIPTION

This is an OO interface to the NFCU online banking interface at:
  https://www.navyfederal.org/

The goal is to provide a convenient read-only interface for your
financial data. This module also gives you the ability to make
projections based on known pending transactions and scheduled
transactions. This can come in handy when contemplating the impact
of a purchase or a new monthly expense.

Use the sample program in the example directory to get started.

=head1 METHODS

=over

=item new

Creates a new instance of this class which has used the given
credentials to authenticate on the NFCU web interface. If the login
event fails on the web interface then undef is returned.

Expects a hash ref of credentials with the following keys:

=over

=item access_number

Same as the field labeled 'Access Number' on the website.

=item user_id

Same as the field labeled 'User ID' on the website.

=item password

Same as the 'Password' field on the website.

=back

None of the above values are cached or retained by this module
in any form. You need to make yourself aware of, and assume full
responsibility for, any potential security risks that may be
involved with passing your online banking credentials via this module.

=item config

Use this function to configure various internal parameters including:

=over

=item cache_dir

The directory where you'd like to cache the bank statement source data.
Caching the data will allow eStatement information to be available
after it is no longer on the website. This directory will contain your
financial data in plain text and you should choose it carefully.

=item cache_override

Use this if you want to override any caching behavior for recent
transations. This may apply if you're expecting a new transaction to appear
which has occurred since the currently cached data was stored.

=item tidy_rc

This is a code reference for the callback function that you'd like
to use for tidying up the transaction labels. The transaction label
will be passed as a string and the tidied version should be returned.
If not given then the default tidy function is used which is optimal for the
author's purposes.

=item categorize_rc

This is a code reference for the callback function that you'd like to use for
determining which category label applies for a given transaction description.

The categorize callback receives two arguments:

=over

=item *

the transaction description

=item *

the transaction amount (positive or negative) in cents.

=back

If no category is identified then the function ought to return something
like 'uncategorized'. If undef is returned as a category then this module
will croak. (You could do that on purpose to help identify transactions which
are failing to be properly categorized.)

The categorize function ought to recognize a payment in varied formats.

For example my trash collection payments:

=over

=item eStatement format

  Paid To - Nationwideallied Online Pmt Chk 6410085

=item recent transactions format

  ACH Transaction - NATIONWIDEALLIED ONLINE PMT

=item billpay format

  Allied Waste

=back

If no categorize_rc is given then the default categorize function is used
which is not likely to be exactly what you need. A good categorize function is
essential because there are several functions which operate by consolidating
all the known transaction data to get a complete history. If the categorize
function doesn't correctly resolve to a consistent category then there will be
discrepencies for cases where there is overlap. This overlap will be evident
by warning or die messages like "transaction discrepancy ...".

=item error_level

As the transactions are read from the web content they're validated for
accuracy by double checking the amounts and their affects on the balance.
If the math doesn't work out between two adjacent transactions then this
module will emit a warning. Set this config parameter to 'fatal' to
upgrade this situation to a fatal error. This situation is unlikely if
you've written a thorough categorize function. (See categorize_rc above.)

=back

=item get_balances

Returns a hash of the accounts and the current balances as indicated on
the main account page.

=item get_recent_transactions

Returns an array ref of transaction hash refs. The transactions are drawn
from the list of recent activity for the current statement period.

=item get_estatement_transactions

Returns an array ref of transaction hash refs. The transactions are drawn
from the eStatement section and includes all available history.

=item get_billpay_transactions

Returns an array ref of transaction hash refs. The transactions are drawn
from the billpay section of the website and includes pending status
transactions.

=item get_transactions

Returns an array ref of transaction hash refs. The transactions are an
audited aggregation of the transactions returned from the current statement,
the eStatements, and pending transactions in the billpay section. The
pending billpay transactions suggest a predicted balance history which
is based on the current balance as of the most recent confirmed
transaction.

=item get_expenditure_report

This analyzes all the accessible transaction data and computes stats on the
amount spent on the various categories (as determined by the categorize_rc
function).

Accepts a configuration hash ref with two keys:

=over

=item *

from_date -- Date to start expenditure analysis (MM/DD/YYYY).

=item *

to_date -- Date to terminate expenditure analysis (MM/DD/YYYY).

=back

Example:

  my %dates = (
      from_date => '04/03/2011',
      to_date   => '07/03/2011',
  );
  my $report_ra = $nfcu->get_expenditure_report( \%dates );

  print "Expenditure Report:\n";

  my $tb = Text::Table->new( @{ shift @{ $report_ra } } );
  $tb->load( @{ $report_ra } );

  print "\n$tb\n\n";

You may also pass dates in epoch values with they keys from_epoch, to_epoch.

  my %dates = (
      from_epoch => 0,
      to_epoch   => time,
  );
  my $report_ra = $nfcu->get_expenditure_report( \%dates );

=item get_transaction_schedule

This function analyzes your expenditures and composes a hash of categories and
the apparent schedule of transaction activity. This could be useful in making
predictions about future spending habits.

The output may look something like this:

  {
      'groceries' => {
          'ave_amount'       => '-5083.95',
          'ave_interval'     => '4',
          'day_of_month'     => 11,
          'interval'         => 6,
          'item_rh' => {
              'vons spring valley ca'                => 1,
              'vons san diego ca'                    => 1,
              'vons store 2118 san diego'            => 1,
              'vons chula visa ca'                   => 1,
              'seafood city market. national cit ca' => 1,
              '0124 henrys la mesa ca'               => 2,
              'vons la mesa ca'                      => 1,
          },
          'last_occur_date'  => '07/03/2011',
          'last_occur_epoch' => 1309676400,
          'monthly_ave'      => '-33893',
          'occurrence_count' => 20,
          'total'            => -101679,
          'weekly_ave'       => '-7908.36666666667',
      },
      'housing' => {
          'ave_amount'       => '-252023',
          'ave_interval'     => '30',
          'day_of_month'     => 1,
          'interval'         => 30,
          'item_rh' => {
              'bac home loans online pmt'   => 1,
              'bachomeloansvclp online pmt' => 1,
              'bank of america online pmt'  => 1
          },
          'last_occur_date'  => '07/01/2011',
          'last_occur_epoch' => 1309503600,
          'monthly_ave'      => '-252023',
          'occurrence_count' => 3,
          'total'            => -396069,
          'weekly_ave'       => '-30805.3666666667',
      },
  }

This result indicates grocery and housing payments made
over a three month period. The program doesn't know the difference
between a regularly scheduled expense (such as housing) vs.
a not-so regular expense like groceries. So it's up to you to
decide the difference as indicated in keys like 'day_of_month'
and 'ave_interval'.

Accepts an optional configuration hash ref with two keys:

=over

=item *

from_date -- Date to start expenditure analysis (MM/DD/YYYY).

=item *

to_date -- Date to terminate expenditure analysis (MM/DD/YYYY).

=back

Example:

  my %dates = (
      from_date => '04/03/2011',
      to_date   => '07/03/2011',
  );
  my $schedule_rh = $nfcu->get_transaction_schedule( \%dates );

You may also pass dates in epoch values with they keys from_epoch, to_epoch.

  # this is the default if the date hash is not given.
  my %dates = (
      from_epoch => 0,
      to_epoch   => time,
  );
  my $schedule_rh = $nfcu->get_transaction_schedule( \%dates );


=item get_future_transactions

Projects future expenditures by extrapolating from current transaction
history according to the given transaction schedule. The schedule is
a hash of categories (as defined by the categorize function).

Example:

  my %schedule = (
      'housing' => {
          'ave_amount'   => '-250000', # cents
          'day_of_month' => 1,
      },
      'income' => {
          amount   => 50000,           # cents
          'day_of_month' => [ 1, 16 ], # may be a list
      },
      'groceries' => {
          'interval'   => 13,
          'ave_amount' => '$50.95-',   # dollars
      },
      'one_time_expense' => {
          'on'     => '01/05/2012', # mm/dd/yyyy
          'amount' => '$42.00-',    # dollars
      },
      'three_time_expense' => {
          'on'     => [
            '01/05/2012',
            '02/05/2012',
            '03/05/2012',
          ],
          'amount' => '$42.00-',    # dollars
      },
  );
  my %config = (
    account     => 'EveryDay Checking',
    from_epoch  => 0,
    to_epoch    => ( time + 86_400 * 90 ),
    schedule_rh => \%schedule,
  );
  my $transaction_ra = $nfcu->get_future_transactions( \%config );

The frequency description must be either a day_of_month or an interval key.
You might use the result of the get_transaction_schedule function as a basis
for this schedule. But you'll need to clean it up because the generated
schedule is the result of some guess-work. Maybe in a future release ...

=back

=head1 BUGS

Pending billpay transactions go to 'paid' status before going to 'confirmed'
status. Sometimes these don't get merged with the current transactions
correctly.

=head1 AUTHOR

Dylan Doxey, E<lt>dylan.doxey@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Dylan Doxey

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
