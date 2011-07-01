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

our $VERSION = 0.01;

my ( %URL_FOR, $AUTHENTICATED_REGEX, $OFFLINE_REGEX, $ACCT_SUMMARY_REGEX, $ACCT_ROW_REGEX, $PAYMENT_REGEX,
    $ESTATEMENT_URL_REGEX, $ESTATEMENT_ROW_REGEX, $ESTATEMENT_PERIOD_REGEX, $TIME_ZONE );
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
      <tr \s id="accountTable \w+ ">                                \s*
        <td \s class="tdText">         \s* ( [^<]+? ) \s* </td>     \s*
        <td \s class="tdText"><a \s href=" ( [^"]+? ) ">
                                       \s* ( [^<]+? ) \s* </a></td> \s*
        <td \s class="tdAmt">          \s* ( [^<]+? ) \s* </td>     \s*
      </tr>
    }xms;

    Readonly $ACCT_ROW_REGEX => qr{
        <tr \s id="sortall [^"]+ "> \s* ( .+? ) \s* </tr>
    }xms;

    Readonly $ESTATEMENT_URL_REGEX => qr{
        statementType=A
    }xms;

    Readonly $ESTATEMENT_ROW_REGEX => qr{
        ( \d+ - \d+ .+? \d+ [.] \d{2} ) \n
    }xms;

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
    my $now = time;
    my ( %epoch_for, %date_for, %regexes_for, %item_for );

    sub _trans_cmp {
        my ( $m, $n ) = @_;

        return $m->{epoch} <=> $n->{epoch}
            if $m->{epoch} != $n->{epoch};

        return -1
            if $m->{balance} + $n->{amount} == $n->{balance};

        return 1
            if $n->{balance} + $m->{amount} == $m->{balance};

        return $n->{_sequence} <=> $m->{_sequence};
    }

    sub _as_dollars {
        for my $rs (@_) {

            if ( ${$rs} =~ m{\A -? ( \d+ ) [.]? ( \d* ) \z}xms ) {

                my $dollars    = $1;
                my $cents      = $2 || '00';
                my $multiplier = ${$rs} < 0 ? '-' : "";

                $cents = ( substr $dollars, -2 ) . $cents;
                $cents = sprintf '%02u', ( substr $cents, 0, 6 );
                $cents = substr $cents, 0, 2;

                $dollars = reverse substr $dollars, 0, -2;
                $dollars ||= '0';
                $dollars =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
                $dollars = scalar reverse $dollars;

                ${$rs} = sprintf '$%s%s', "$dollars.$cents", $multiplier;
            }
            else {

                warn "unable to parse: ", ${$rs};
            }
        }
        return;
    }

    sub _as_cents {
        for my $rs (@_) {

            if ( ${$rs} =~ m{\A [\$]? ( [,\d]+ ) [.] ( \d+ ) ( \D )? \z}xms ) {

                my $dollars    = $1;
                my $cents      = $2;
                my $multiplier = $3 ? -1 : 1;

                $dollars =~ s{\D}{}xmsg;

                ${$rs} = sprintf '%d', $cents + 100 * $dollars;
                ${$rs} *= $multiplier;
            }
            else {

                warn "unable to parse: ", ${$rs};
            }
        }
        return;
    }

    sub _merge {
        my @transaction_lists = @_;

        my $list_number = 1;

        my %is_seen;

        my @merger;

        for my $transaction_ra ( reverse @transaction_lists ) {

            TRANS:
            for my $transaction_rh ( reverse @{ $transaction_ra } ) {

                my $key = join ',', @{ $transaction_rh }{qw( epoch amount category )};

                my $item = $transaction_rh->{item};

                next TRANS
                    if $is_seen{$key} && $list_number ne $is_seen{$key};

                $is_seen{$key} = $list_number;

                if ( @merger ) {

                    my $last_transaction_rh = $merger[0];

                    my $last_epoch = $last_transaction_rh->{epoch};
                    my $epoch      = $transaction_rh->{epoch};

                    next TRANS
                        if $epoch < $last_epoch; # overlap

                    if ( not defined $transaction_rh->{balance} ) {

                        my $balance = $last_transaction_rh->{balance};
                        my $amount  = $transaction_rh->{amount};

                        die "no balance found:", Dumper( $last_transaction_rh )
                            if not defined $balance;

                        $balance += $amount;

                        $transaction_rh->{balance}     = $balance;
                        $transaction_rh->{balance_str} = $balance;

                        _as_dollars( \$transaction_rh->{balance_str} );
                    }
                }

                $transaction_rh->{_list_number} = $list_number;

                unshift @merger, $transaction_rh;
            }

            $list_number++;
        }

        map { delete $_->{_list_number} } @merger;

        return _audit_transaction_list( \@merger );
    }

    sub _audit_transaction_list {
        my ( $transaction_ra, $reverse_flag, $error_level ) = @_;

        return $transaction_ra
            if !@{ $transaction_ra || [] };

        if ($reverse_flag) {

            @{$transaction_ra} = reverse @{$transaction_ra};
        }

        my $balance = $transaction_ra->[0]->{balance};

        for my $transaction_rh ( @{$transaction_ra} ) {

            my $balance_str          = sprintf '%0.2f', $balance;
            my $expected_balance_str = sprintf '%0.2f', $transaction_rh->{balance};

            if ( $balance_str != $expected_balance_str ) {

                my $message =
                    sprintf 'transaction discrepancy (%s != %s) at: %s',
                    $balance_str,
                    $expected_balance_str,
                    Dumper($transaction_rh);

                die $message
                    if $error_level eq 'fatal';

                warn $message;
            }

            $balance += ( -1 * $transaction_rh->{amount} );
        }

        return $transaction_ra;
    }

    sub _formatted_date {
        my $epoch = defined $_[0] ? $_[0] : $now;

        return $date_for{$epoch}
            if exists $date_for{$epoch};

        my $dt = DateTime->from_epoch( epoch => $epoch );

        $date_for{$epoch} = $dt->mdy('/');

        return $date_for{$epoch}
    }

    sub _compute_epoch {
        my ( $date, $default_epoch ) = @_;

        return $default_epoch
            if !$date;

        return $epoch_for{$date}
            if exists $epoch_for{$date};

        my ( $m, $d, $y ) = split /\D/, $date;

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
        my ( $item ) = @_;

        if ( !keys %regexes_for ) {

            %regexes_for = (
                'administrative' => [
                    qr{ (?: \b | \s ) transfer \s }xmsi,
                    qr{ \b dividend \b }xmsi,
                    qr{ \s u \S? haul (?: \b | \s ) }xmsi,
                    qr{ \s fedex \s }xmsi,
                    qr{ american \s inspectio }xmsi,
                ],
                'yuki' => [
                    qr{ transfer .+? yuki }xmsi,
                ],
                'health' => [
                    qr{ \s fertlity \s }xmsi,
                    qr{ \s quest \s }xmsi,
                    qr{ \s cvs (?: \s* pharmacy )? \s }xmsi,
                    qr{ \s unoptical \s }xmsi,
                    qr{ \s william \s v \s carlo \s }xmsi,
                ],
                'charity' => [
                    qr{ \W democrats \W }xmsi,
                    qr{ \s cancer \s }xmsi,
                    qr{ \s moveon[.]org \s }xmsi,
                    qr{ \s komen \s }xmsi,
                    qr{ \s barackobama[.]c }xmsi,
                    qr{ \s rob \s miller \s }xmsi,
                ],
                'entertainment' => [
                    qr{ bigstar \W tv }xmsi,
                    qr{ \W netflix \W }xmsi,
                    qr{ south \s sun \s products }xmsi,
                    qr{ book \s off \s }xmsi,
                    qr{ old \s town \s liquor \s }xmsi,
                    qr{ hortensia \s liquor \s }xmsi,
                    qr{ \s barclays \s }xmsi,
                ],
                'gasoline' => [
                    qr{ \s shell \s service }xmsi,
                    qr{ \s oil \s }xmsi,
                    qr{ \s arco \W }xmsi,
                    qr{ \s costco \s gas \s }xmsi,
                    qr{ \s lemon \s grove \s 76 \s }xmsi,
                ],
                'income' => [
                    qr{ \s tierranet \s }xmsi,
                    qr{ \A (?: atm \s )? deposit (?: \s | \b ) }xmsi,
                ],
                'education' => [
                    qr{ \s u[.]s[.] \s dept[.] \s of \s ed \s }xmsi,
                    qr{ \s sdccd \s }xmsi,
                    qr{ \s mesa \s college \s }xmsi,
                ],
                'travel' => [
                    qr{ \s olancha \s }xmsi,
                    qr{ \s ontario \s }xmsi,
                    qr{ \s kennewick \s }xmsi,
                    qr{ \s spokane \s }xmsi,
                    qr{ \s winnemucca \s }xmsi,
                    qr{ \s maverik \s }xmsi,
                    qr{ \s supershuttle \s }xmsi,
                    qr{ topaz \s lodge \s }xmsi,
                    qr{ \s portland \s }xmsi,
                    qr{ \s hesperia \s }xmsi,
                    qr{ \s alaska \s air \s }xmsi,
                    qr{ \s lone \s pine? \s ca }xmsi,
                ],
                'fraud' => [
                    qr{ \s norton \s }xmsi,
                ],
                'government' => [
                    qr{ check \s 1622 }xmsi,
                ],
                'housing' => [
                    qr{ \s tierra \s prop }xmsi,
                    qr{ \s bank \s of \s america \s }xmsi,
                    qr{ (?: \b | \s ) bac \s* home \s* loan }xmsi,
                    qr{ \W nationwide/all }xmsi,
                    qr{ (?: \b | \s ) nationwide \s* allied \s }xmsi,
                ],
                'business' => [
                    qr{ \s totalchoice \s }xmsi,
                    qr{ \s usps \s }xmsi,
                    qr{ \s office \s depot \s }xmsi,
                ],
                'cell phones' => [
                    qr{ (?: \b | \s ) verizon \s }xmsi,
                ],
                'cars' => [
                    qr{ \s dmv \W }xmsi,
                    qr{ \s smog \s }xmsi,
                    qr{ \s autozone \s }xmsi,
                    qr{ \s advantec \s }xmsi,
                    qr{ (?: \b | \s ) napa \s store \s }xmsi,
                    qr{ (?: \b | \s ) jiffy \s lube \s }xmsi,
                    qr{ (?: \b | \s ) toyota/lexus \s }xmsi,
                ],
                'insurance' => [
                    qr{ \s auto \s club \s }xmsi,
                    qr{ (?: \b | \s ) allied \s insurance }xmsi,
                ],
                'cats' => [
                    qr{ \s abc \s vet \s }xmsi,
                    qr{ \s petco \s }xmsi,
                ],
                'unaccounted' => [
                    qr{ \s paypal \s }xmsi,
                    qr{ atm \s withdrawal }xmsi,
                    qr{ check \s 1650 }xmsi,
                    qr{ check \s 1626 }xmsi,
                    qr{ check \s 1623 }xmsi,
                    qr{ check \s 1624 }xmsi,
                    qr{ check \s 1625 }xmsi,
                    qr{ check \s 5860 }xmsi,
                    qr{ check \s 5837 }xmsi,
                    qr{ check \s 5835 }xmsi,
                    qr{ check \s 5836 }xmsi,
                ],
                'internet' => [
                    qr{ (?: \b | \s ) cox \s cable \s }xmsi,
                    qr{ (?: \b | \s ) cox \s communications }xmsi,
                ],
                'water' => [
                    qr{ (?: \b | \s ) otay \s water \s }xmsi,
                ],
                'trash' => [
                    qr{ (?: \b | \s ) republic \s service \s }xmsi,
                ],
                'utilities' => [
                    qr{ \s gas \s & (?: amp; )? \s electric \b }xmsi,
                    qr{ \s sdg \s & (?: amp; )? e \s }xmsi,
                ],
                'shopping' => [
                    qr{ \W newegg \W }xmsi,
                    qr{ (?: \b | \s ) home \s depot \s }xmsi,
                    qr{ \s tech \s 4 \s less \s }xmsi,
                    qr{ \s best \s buy \s }xmsi,
                    qr{ \s ikea \s }xmsi,
                    qr{ \s dixieline \s }xmsi,
                    qr{ \s groupon \s }xmsi,
                    qr{ \s mega \s liquidation \s }xmsi,
                    qr{ \s outlet \s }xmsi,
                    qr{ \s costco \s }xmsi,
                    qr{ \s target \s }xmsi,
                    qr{ \s amazon (?: [.] com )? \s }xmsi,
                    qr{ s \s electronics \s }xmsi,
                ],
                'fast food' => [
                    qr{ \s chin &[#]039; s \s }xmsi,
                    qr{ \s sammy &[#]039; s \s }xmsi,
                    qr{ \s noodles \s & }xmsi,
                    qr{ \s jack \s in \s the \s box }xmsi,
                    qr{ \s islands \s }xmsi,
                    qr{ \s juice \s blend \s }xmsi,
                    qr{ \s mediterran \s }xmsi,
                    qr{ \s chai \s tian \s }xmsi,
                    qr{ \s sushi \s }xmsi,
                    qr{ \s tartine \s }xmsi,
                    qr{ \s saffron \s }xmsi,
                    qr{ \s bistro \s }xmsi,
                    qr{ \s sichuan \s }xmsi,
                    qr{ \s carljr \d }xmsi,
                    qr{ \s country \s kabob \s }xmsi,
                    qr{ \s espresso \s }xmsi,
                    qr{ \s izumi \s }xmsi,
                    qr{ \s las \s brasas \s }xmsi,
                    qr{ \s pizza \s }xmsi,
                    qr{ \s greek \s }xmsi,
                    qr{ \s yogurt \s }xmsi,
                    qr{ \s niban \s }xmsi,
                    qr{ \s brioche \s }xmsi,
                    qr{ \s chipotle \s }xmsi,
                    qr{ \s cafe \s }xmsi,
                    qr{ \s pho \s }xmsi,
                    qr{ \s pappalecco \s }xmsi,
                    qr{ \s submarina \s }xmsi,
                    qr{ \s subway \s }xmsi,
                    qr{ \s city \s wok \s }xmsi,
                    qr{ \s starbucks \s }xmsi,
                    qr{ \s sbux \s }xmsi,
                ],
                'groceries' => [
                    qr{ \s 99 \s ranch \s }xmsi,
                    qr{ \s ranch \s market \s }xmsi,
                    qr{ \s seafood \s city \s  }xmsi,
                    qr{ \s supermarket \s }xmsi,
                    qr{ \s marukai \s }xmsi,
                    qr{ \s ralph (?: &[#]039; )? s \s }xmsi,
                    qr{ \s mitsuwa \s }xmsi,
                    qr{ \s fresh \s &amp; \s easy \s }xmsi,
                    qr{ \s nijiya \s }xmsi,
                    qr{ \s henrys \s }xmsi,
                    qr{ \s vons \s }xmsi,
                    qr{ \s albertsons \s }xmsi,
                ],
            );
        }

        for my $category (keys %regexes_for) {

            for my $regex (@{ $regexes_for{$category} }) {

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

    my %default_for = (
        stack_depth => 1, # lower memory consumption
        agent       => __PACKAGE__ . '/' . $VERSION,
    );
    for my $key (keys %default_for) {

        if ( !exists $option_rh->{$key} ) {

            $option_rh->{$key} = $default_for{$key};
        }
    }

    my $self = bless $class->SUPER::new( %{ $option_rh } ), $class;

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

    $self->get( $URL_FOR{main} );

    return
        if !$self->success();

    croak "NFCU online banking is off line\n"
        if $self->content() =~ $OFFLINE_REGEX;

    return
        if $self->content() !~ $AUTHENTICATED_REGEX;

    return $self;
}

sub config {
    my ( $self, $config_rh ) = @_;

    for my $option (qw( cache_dir categorize_rc tidy_rc error_level )) {

        if ( exists $config_rh->{$option} ) {

            $self->{"__$option"} = delete $config_rh->{$option};
        }
    }

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
                );
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
    my ( $self ) = @_;

    my $billpay_ra    = $self->get_billpay_transactions();
    my $recent_ra     = $self->get_recent_transactions();
    my $estatement_ra = $self->get_estatement_transactions();

    return _merge( $billpay_ra, $recent_ra, $estatement_ra );
}

sub get_recent_transactions {
    my ( $self, $config_rh ) = @_;

    $config_rh ||= {};

    my $account = $config_rh->{account} || 'EveryDay Checking';
    my $from    = _compute_epoch( $config_rh->{from}, 0 );
    my $to      = _compute_epoch( $config_rh->{to},   time );

    my $cache_dir     = $self->{__cache_dir};
    my $categorize_rc = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc       = $self->{__tidy_rc} || \&_tidy_item;

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $account_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    croak "account '$account' is not found"
        if !$account_rh;

    my $account_number = $account_rh->{account_number};
    my $account_url    = $account_rh->{detail_url};

    my $cache_filename = _cache_filename( $cache_dir, $account_url, 'hour' );
    my $html = _fetch_cache($cache_filename);

    if ( !$html ) {

        $self->get($account_url);

        return
            if !$self->success();

        if ($cache_filename) {

            $self->save_content($cache_filename);
        }

        $html = $self->content();
    }

    my @transactions;

    ROW:
    while ( $html =~ m{ $ACCT_ROW_REGEX }xmsg ) {

        my $row = $1;

        my @data = $row =~ m{<td [^>]* > \s* ( .+? ) \s* </td>}xmsg;

        die "couldn't parse row:$row\n"
            if !@data;

        $data[0] =~ s{<[^>]+>}{}xmsg;

        my $status = $data[0] eq 'Pending' ? 'Pending'         : 'Confirmed';
        my $date   = $data[0] eq 'Pending' ? _formatted_date() : $data[0];
        my $item   = $data[1];
        my $amount_str  = $data[2];
        my $balance_str = $data[3];
        my $epoch       = _compute_epoch($date);
        my $category    = $categorize_rc->( $data[1] );

        next ROW
            if $epoch > $to;

        last ROW
            if $epoch <= $from;

        my ( $amount, $balance ) = ( $amount_str, $balance_str );

        _as_cents( \$amount, \$balance );

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

#    my $calling_package = ( caller 1 )[3] || "";
#    return \@transactions
#        if -1 != index $calling_package, __PACKAGE__;

    return _audit_transaction_list( \@transactions, 0, $self->{__error_level} );
}

sub get_estatement_transactions {
    my ( $self, $config_rh ) = @_;

    $config_rh ||= {};

    my $account       = $config_rh->{account} || 'EveryDay Checking';
    my $cache_dir     = $self->{__cache_dir};
    my $categorize_rc = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc       = $self->{__tidy_rc} || \&_tidy_item;

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $account_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    croak "account '$account' is not found"
        if !$account_rh;

    my $account_number = $account_rh->{account_number};

    $self->get( $URL_FOR{estatements} );

    return
        if !$self->success();

    my $date_cmp_rc = sub {
        my ( $a, $b ) = @_;
        my ( $ma, $da, $ya ) = split /\D/, $a;
        my ( $mb, $db, $yb ) = split /\D/, $b;
        return $ya <=> $yb
            if $ya != $yb;
        return $ma <=> $mb
            if $ma != $mb;
        return $da <=> $db;
    };
    my @links =
        sort { $date_cmp_rc->( $a->text(), $b->text() ) }
        $self->find_all_links( url_regex => $ESTATEMENT_URL_REGEX );

    my $account_regex = qr{
        \w+ \s \w+ \s+ - \s+ $account_number \s+
        ( .+? )
        \d+ - \d+ \s+ Ending \s Balance
    }xms;

    my @transactions;

    my $sequence = 0;

    LINK:
    for my $link (@links) {

        my ( $m, $d, $y ) = $link->text() =~ m{( \d+ ) \D ( \d+ ) \D ( \d+ )}xms;

        my $cache_filename = _cache_filename( $cache_dir, "estatement_$m.$d.$y", 'indefinite' );
        my $html = _fetch_cache($cache_filename);

        if ( !$html ) {

            $self->get($link);

            die "failed to get ", $link->url()
                if !$self->success();

            if ($cache_filename) {

                $self->save_content($cache_filename);
            }

            $html = $self->content();
        }

        my @period_dates = $html =~ $ESTATEMENT_PERIOD_REGEX;

        die "failed to parse period dates from: $cache_filename:\n",
            $link->url()
            if !@period_dates;

        my ($body) = $html =~ $account_regex;

        die "failed to parse $account - $account_number body from ",
            $link->url()
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

            $row =~ s{ \n \s* }{ }xmsg;

            next ROW
                if $row =~ m{ beginning \s+ balance }xmsi;

            my ( $month_day_item, $amount_str, $balance_str ) = split /\s{3,}/, $row;

            my ( $month, $day, $item ) = $month_day_item =~ m{\A ( \d+ )-( \d+ ) \s ( .+ ) }xms;

            my $date = sprintf '%0.2d/%0.2d/%d', $month, $day, $year_for{$month};
            my $epoch = _compute_epoch($date);

            my ( $amount, $balance ) = ( $amount_str, $balance_str );

            _as_cents( \$amount, \$balance );

            my %transaction = (
                amount      => $amount,
                amount_str  => $amount_str,
                balance     => $balance,
                balance_str => $balance_str,
                category    => $categorize_rc->($item),
                date        => $date,
                epoch       => $epoch,
                item        => $tidy_rc->($item),
                status      => 'Confirmed',
            );

            push @transactions, \%transaction;
        }
    }

#    my $calling_package = ( caller 1 )[3] || "";
#    return \@transactions
#        if -1 != index $calling_package, __PACKAGE__;

    return _audit_transaction_list( \@transactions, 'reverse', $self->{__error_level} );
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
    }

    my $account       = $config_rh->{account} || 'EveryDay Checking';
    my $cache_dir     = $self->{__cache_dir};
    my $categorize_rc = $self->{__categorize_rc} || \&_categorize;
    my $tidy_rc       = $self->{__tidy_rc} || \&_tidy_item;

    return
        if !exists $self->{__nfcu_account_url_rh}
            && !$self->get_balances();

    my $balance_rh = $self->{__nfcu_account_url_rh}->{ lc $account };

    if ( !$balance_rh ) {
        carp "account '$account' is not found";
        return;
    }

    $account = $balance_rh->{account};

    my $cache_filename = _cache_filename( $cache_dir, $URL_FOR{billpay}, 'hour' );
    my $html           = _fetch_cache( $cache_filename );

    if ( !$html ) {

        $self->get( $URL_FOR{billpay} );

        return
            if !$self->success();

        if ( $cache_filename ) {

            $self->save_content( $cache_filename );
        }

        $html = $self->content();
    }

    PAYMENT:
    while ( $html =~ m{ $PAYMENT_REGEX }xmsg ) {

        my $hash_str = $1;

        # }
        #    upperName              : 'U.S. Department of Education',
        #    amount                 : 'Canceled',
        #    feeamount              : '',
        #    MMDD_dateToPay         : '03/30/11',
        #    MMDDYYYY_dateToPay     : '03/30/2011',
        #    status                 : 'Canceled',
        #    numericStatus          : '4',
        #    id                     : '000000000000002',
        #    pmtId                  : '20110226034246960182',
        #    mobilePaymentIndicator : 'N',
        #    mobilePaymentAltText   : ''
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

        my %transaction = (
            amount      => $amount,
            amount_str  => $amount_str,
            category    => $categorize_rc->( $item ),
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
    my ( $self ) = @_;

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %total_for;
    {
        my $billpay_ra    = $self->get_billpay_transactions();
        my $recent_ra     = $self->get_recent_transactions();
        my $estatement_ra = $self->get_estatement_transactions();

        my $transaction_ra = _merge( $billpay_ra, $recent_ra, $estatement_ra );

        for my $transaction_rh (@{ $transaction_ra }) {

            my $amount   = $transaction_rh->{amount};
            my $category = $transaction_rh->{category};
            my $epoch    = $transaction_rh->{epoch};

            $first_epoch = $epoch;
            $last_epoch ||= $epoch;

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
    my ( $self ) = @_;

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %schedule;
    {
        my $billpay_ra    = $self->get_billpay_transactions();
        my $recent_ra     = $self->get_recent_transactions();
        my $estatement_ra = $self->get_estatement_transactions();

        my $transaction_ra = _merge( $billpay_ra, $recent_ra, $estatement_ra );

        for my $transaction_rh (@{ $transaction_ra }) {

            my $amount   = $transaction_rh->{amount};
            my $category = $transaction_rh->{category};
            my $epoch    = $transaction_rh->{epoch};

            $first_epoch ||= $epoch;
            $last_epoch = $epoch;

            my $last_occur_epoch = $schedule{$category}->{last_occur_epoch};

            if ( $epoch > time ) {

                $schedule{$category}->{next_occur_epoch} = $epoch;
            }
            else {

                $schedule{$category}->{last_occur_epoch} = $epoch;
            }

            $schedule{$category}->{total} += $amount;
            $schedule{$category}->{occurrence_count}++;
        }
    }

    my $total_time = $last_epoch - $first_epoch;

    my $days   = $total_time / 86_400;
    my $weeks  = $total_time / ( 7 * 86_400 );
    my $months = $total_time / ( 30 * 86_400 );

    for my $category ( keys %schedule ) {

        my $event_rh = $schedule{$category};

        my $count            = $event_rh->{occurrence_count};
        my $last_epoch       = $event_rh->{last_occur_epoch} || 0;
        my $next_epoch       = $event_rh->{next_occur_epoch} || 0;
        my $total            = $event_rh->{total};

        my $ave_amount   = $total / $count;
        my $ave_interval = sprintf '%d', $days / $count;
        my $monthly_ave  = $total / $months;
        my $weekly_ave   = $total / $weeks;

        my $interval;
        {
            $interval = $next_epoch ? $days / $count : ( time - $last_epoch ) / 86_400;
            $interval = sprintf '%d', ( $interval < 1 ? $ave_interval : $interval );
        }

        if ($last_epoch) {

            DAY:
            for my $i ( 1 .. 364 ) {

                $next_epoch = sprintf '%d', $last_epoch + $i * $interval * 86_400;

                last DAY
                    if $next_epoch > time;
            }
        }
        elsif ($next_epoch) {

            $last_epoch = sprintf '%d', $next_epoch - $interval * 86_400;
        }

        $event_rh->{next_occur_epoch} = $next_epoch;
        $event_rh->{next_occur_date}  = _formatted_date($next_epoch);
        $event_rh->{last_occur_epoch} = $last_epoch;
        $event_rh->{last_occur_date}  = _formatted_date($last_epoch);
        $event_rh->{ave_amount}       = $ave_amount;
        $event_rh->{ave_interval}     = $ave_interval;
        $event_rh->{interval}         = $interval;
        $event_rh->{monthly_ave}      = $monthly_ave;
        $event_rh->{weekly_ave}       = $weekly_ave;
    }

    return \%schedule;
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

  my $transaction_ra = $nfcu->get_transactions();

  for my $transaction_rh (@{ $transaction_ra }) {

      my ( $date, $item, $amount ) = @{ $transaction_rh }{qw( date item amount )};

      print "$date -- $item -- $amount\n";
  }

  my $report_ra = $nfcu->get_expenditure_report();

  for my $category_rh (@{ $report_ra }) {

      my $category    = $category_rh->{category};
      my $total       = $category_rh->{total};
      my $weekly_ave  = $category_rh->{weekly_ave};
      my $monthly_ave = $category_rh->{monthly_ave};

      print "$category: $total, $weekly_ave, $monthly_ave\n";
  }

=head1 WARNING

This module is designed to interact with your online
banking service at Navy Federal Credit Union. You are fully
responsible for the actions taken with this module and you are
expected to audit the code yourself to be sure that the
security, accuracy and quality is up to your expectations.

The author cannot assume responsibility for any untoward or
nefarious activities attempted with this software.

You are advised to never leave passwords, access numbers or
user IDs hard coded in your programs.

=head1 DESCRIPTION

This is an OO interface to the NFCU online banking interface at:
  https://www.navyfederal.org/

The goal is to provide a convenient read-only interface for your
financial data.

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

=item tidy_rc

This is a code reference for the function that you'd like to use for
tidying up the transaction labels. The transaction lable will be passed
as a string and the tidied version should be returned. If not given
then the default tidy function is used which is optimal for the
author's purposes.

=item categorize_rc

This is a code reference for the function that you'd like to use for
determining which category label applies for a given transaction label.
The transaction label will be passed as a string and the category label
should be returned, or some sensible default category such
as 'uncategorized'. If not given then the default tidy function is used
which is optimal for the author's purposes.

=item error_level

As the transactions are read from the web content they're validated for
accuracy by double checking the amounts and their affects on the balance.
If the math doesn't work out between two adjacent transactions then this
module will emit a warning. Set this config parameter to 'fatal' to
upgrade this situation to a fatal error. Although the author considers
this possibility to be unlikely, you might want to run this in fatal mode
just to be certain.

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
from the billpay section of the website and includes Pending status
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

=back

=head1 TODO

This is currently "EveryDay Checking" oriented. There are some optional
(and currently undocumented and untested) parameters which could potentially
be used to focus on particular accounts such as a credit card, loan, savings
or any other accounts accessible. A future revision will support alternate
accounts of interest.

This is a working first draft which is useful to the author. It comes with no
guarantee as to the correctness or suitability for any particular purpose.

=head1 AUTHOR

Dylan Doxey, E<lt>dylan.doxey@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Dylan Doxey

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
