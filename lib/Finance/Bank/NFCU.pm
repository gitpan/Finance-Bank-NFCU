package Finance::Bank::NFCU;

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
    use Finance::Bank::NFCU::Category qw( categorize );
    use Finance::Bank::NFCU::HTML::Recent;
    use Finance::Bank::NFCU::HTML::Estatement;
    use Finance::Bank::NFCU::HTML::BillPay;
}

our $VERSION = 0.24;

my (%URL_FOR,                 $AUTHENTICATED_REGEX,
    $OFFLINE_REGEX,           $ACCT_SUMMARY_REGEX,
    $ACCT_ROW_REGEX,          $PAYMENT_REGEX,
    $ESTATEMENT_URL_REGEX,    $ESTATEMENT_ROW_REGEX,
    $ESTATEMENT_PERIOD_REGEX, $TIME_ZONE,
    $MAX_POST_DATE_DRIFT,
);
{
    Readonly %URL_FOR => (

        login => "https://myaccounts.navyfcu.org"
            . "/cgi-bin/ifsewwwc?Logon",

        main  => "https://myaccountsaws.navyfcu.org"
            . "/nfoaa/main",

        summary => "https://myaccountsaws.navyfcu.org"
            . "/nfoaa/accounts/summary",

        estatements => "https://myaccountsaws.navyfcu.org"
            . "/eStmts/stmtList.do",

        billpay => "https://myaccountsaws.navyfcu.org"
            . "/WBP/wbp.nfo?page=wbpstart",
    );

    Readonly $AUTHENTICATED_REGEX => qr{
        <a \s href= [^>]+ > Log \s off </a>
    }xms;

    Readonly $OFFLINE_REGEX => qr{ inconvenience }xms;

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

    Readonly $MAX_POST_DATE_DRIFT => 2 * 86_400;
}

# internal helper functions
{
    $Carp::Internal{ (__PACKAGE__) }++;

    my $default_account = 'EveryDay Checking';
    my $error_level = 'non-fatal';
    my $now         = time;
    my ( %epoch_for, %date_for, %criterion_for, %item_for );

    sub _default_account {
        my ($account_rh) = @_;

        if ($account_rh) {

            ($default_account) = map { $_->{account} } grep {
                $_->{detail_url} =~ m{ \W account=2 (?: \W | \z ) }xms
            } values %{$account_rh};
        }

        return $default_account;
    }

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

                croak "$value not a valid value for $key for $calling_func";
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

            for my $trans_hr (@{ $list_ra->[$i] }) {

                next LIST
                    if $trans_hr->{_list_number} == $list_number;

                next LIST
                    if $trans_hr->{epoch} < $epoch;

                my $amt_cat = join ',', @{ $trans_hr }{qw( amount category )};

                return 1
                    if "$amount,$category" eq $amt_cat;
            }
        }

        return;
    }

    sub _match {
        my ( $key_ar, $trans_a_hr, $trans_b_hr ) = @_;

        my %trans_a = %{$trans_a_hr};
        my %trans_b = %{$trans_b_hr};

        if ( grep { $_ eq 'epoch' } @{$key_ar} ) {

            my $epoch_diff = abs $trans_a{epoch} - $trans_b{epoch};

            return
                if $epoch_diff > $MAX_POST_DATE_DRIFT;

            @{$key_ar} = grep { $_ ne 'epoch' } @{$key_ar};
        }

        if ( !exists $trans_b{balance_str} || $trans_b{balance_str} eq "" ) {

            @trans_a{qw( balance balance_str )} = ("") x 2;
            @trans_b{qw( balance balance_str )} = ("") x 2;
        }

        my $a = join '|',
            map { defined $_ ? lc $_ : "" } @trans_a{ @{$key_ar} };

        my $b = join '|',
            map { defined $_ ? lc $_ : "" } @trans_b{ @{$key_ar} };

        return $a eq $b;
    }

    sub _merge {
        my @transaction_lists = @_;

        my @merger;

        my @match_keys = (
            [qw( epoch amount balance item category )],
            [qw( epoch amount balance category )],
            [qw( amount balance item category )],
            [qw( amount balance category )],
        );

        LIST:
        for my $list_idx ( 0 .. $#transaction_lists ) {

            my $trans_ar = $transaction_lists[$list_idx];

            next LIST
                if !$trans_ar || !@{$trans_ar};

            @{ $trans_ar } = reverse @{$trans_ar};

            if ( $list_idx > 0 && @merger ) {

                my $last_trans_hr = $merger[-1];

                KEYSET:
                for my $key_ar (@match_keys) {

                    for my $trans_idx ( 0 .. $#{$trans_ar} ) {

                        my $trans_hr = $trans_ar->[$trans_idx];

                        if ( _match( $key_ar, $last_trans_hr, $trans_hr ) ) {

                            splice @{$trans_ar}, 0, $trans_idx + 1;

                            last KEYSET;
                        }
                    }
                }
            }

            push @merger, @{ $trans_ar };
        }

        @merger = reverse @merger;

        return _audit_transaction_list( \@merger );
    }

    sub _audit_transaction_list {
        my ($trans_ar) = @_;

        return $trans_ar
            if !@{ $trans_ar || [] };

        my $date = $trans_ar->[0]->{date};

        my $balance
            = $trans_ar->[0]->{balance_str} eq ""
            ? ""
            : $trans_ar->[0]->{balance};

        my @transactions;

        TRANS:
        for my $i ( 0 .. $#{$trans_ar} ) {

            my $trans_hr = $trans_ar->[$i];

            $trans_hr->{epoch} ||= _compute_epoch( $trans_hr->{date} );

            if ( $trans_hr->{balance_str} eq "" ) {

                push @transactions, $trans_hr;

                next TRANS;
            }
            elsif ( $balance eq "" ) {

                $balance = $trans_hr->{balance};
            }

            $date = $trans_hr->{date};

            my $expected_balance_str = $balance;
            my $balance_str          = $trans_hr->{balance};

            _as_dollars( \$balance_str, \$expected_balance_str );

            if ( $balance_str eq $expected_balance_str ) {

                push @transactions, $trans_hr;

                $balance = $trans_hr->{balance} - ( 0 + $trans_hr->{amount} );

                next TRANS;
            }

            warn "[$i] $date: $balance_str ne $expected_balance_str\n";
            warn "[" . ( $i - 1 ) . "] ", Dumper( $trans_ar->[ $i - 1 ] )
                if $i != 0;
            die "[$i] ", Dumper($trans_hr);
        }

        return \@transactions;
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

        if ( !$date || $date =~ m{ pending }xmsi ) {

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

        my ($filename) = File::Spec->catfile( $cache_dir, $url ) =~ m{( .+ )}xms;

        return $filename;
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

    if ( !$self->success() ) {

        my $html = $self->content() || "";

        croak "failed to get: ", $URL_FOR{login}, "\n$html\n",
            "conisder setting: PERL_LWP_SSL_VERIFY_HOSTNAME=0"
            if $html =~ m{ certificate \s verify \s failed }xms;

        croak "failed to get: ", $URL_FOR{login};
    }

    if ( $self->content() !~ $AUTHENTICATED_REGEX ) {

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

                $self->{__nfcu_account_url_rh}->{ lc $balance{account} }
                    = \%balance;
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

    if ( $self->{__nfcu_account_url_rh}
        && %{ $self->{__nfcu_account_url_rh} } )
    {
        _default_account( $self->{__nfcu_account_url_rh} );
    }

    return \@balances;
}

sub get_transactions {
    my ( $self, $config_rh ) = @_;

    my $estatement_ra = $self->get_estatement_transactions($config_rh);

    my $recent_ra = $self->get_recent_transactions($config_rh);

    my $balance = $recent_ra->[0]->{balance};

    my $billpay_ra = $self->get_billpay_transactions(
        {   status  => 'pending',
            account => $config_rh->{account},
        }
    );

    for my $trans_hr ( reverse @{$billpay_ra} ) {

        $balance += $trans_hr->{amount};

        $trans_hr->{balance}     = $balance;
        $trans_hr->{balance_str} = $balance;

        _as_dollars( \$trans_hr->{balance_str} );
    }

    return _merge(
        $estatement_ra,
        $recent_ra,
        $billpay_ra,
    );
}

sub get_recent_transactions {
    my ( $self, $config_rh ) = @_;

    my ($account) = _parse_config( $config_rh,
        { account => qr{\A ( \w+ \s \w+ ) \z}xms }
    );

    $account ||= _default_account();

    my $cache_override = $self->{__cache_override};
    my $cache_dir      = $self->{__cache_dir};
    my $categorize_rc  = $self->{__categorize_rc} || \&categorize;

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

    my $parser = Finance::Bank::NFCU::HTML::Recent->new(
        {   html           => $html,
            categorize     => $categorize_rc,
            compute_epoch  => \&_compute_epoch,
            cache_dir      => $cache_dir,
            sequence_start => 1,
        }
    );

    my $trans_ar = $parser->get_transactions();

    for my $trans_hr ( @{$trans_ar} ) {

        if ( $trans_hr->{date} eq 'pending' ) {

            $trans_hr->{date} = _formatted_date();
        }
    }

    return $trans_ar;
}

sub get_estatement_transactions {
    my ( $self, $config_rh ) = @_;

    my ($account) = _parse_config( $config_rh,
        { account => qr{\A ( \w+ \s \w+ ) \z}xms }
    );

    $account ||= _default_account();

    my $cache_dir     = $self->{__cache_dir};
    my $categorize_rc = $self->{__categorize_rc} || \&categorize;

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

    my %mdy_for;

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
            warn "estatement link text '${a}' <=> '${b}' looks weird";

            for my $rs ( \$da, \$db, \$ma, \$mb, \$ya, \$yb ) {

                ${$rs} ||= 0;
            }
        }

        $mdy_for{$a} //= [ $ma, $da, $ya ];
        $mdy_for{$b} //= [ $mb, $db, $yb ];

        return $ya <=> $yb
            if $ya != $yb;
        return $ma <=> $mb
            if $ma != $mb;
        return $da <=> $db;
    };

    # sorted new to old
    my @links =
        sort { $date_cmp_rc->( $b->text(), $a->text() ) }
        grep { $_->text() }
        $self->find_all_links( url_regex => $ESTATEMENT_URL_REGEX );

    my @transactions;

    LINK:
    for my $link (@links) {

        my ( $m, $d, $y ) = @{ $mdy_for{ $link->text() } };

        my $filename = _cache_filename(
            $cache_dir,
            "estatement_$m.$d.$y",
            'indefinite'
        );

        my $html = _fetch_cache($filename);

        if ( !$html ) {

            $self->get($link);

            die 'failed to get ', $link->url()
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

        my $parser = Finance::Bank::NFCU::HTML::Estatement->new(
            {   html           => $html,
                categorize     => $categorize_rc,
                compute_epoch  => \&_compute_epoch,
                cache_dir      => $cache_dir,
                sequence_start => 1 + $#transactions,
            }
        );

        push @transactions,
            @{ $parser->get_transactions(
                { account_number => $account_number }
            ) };
    }

    return _audit_transaction_list( \@transactions );
}

sub get_billpay_transactions {
    my ( $self, $config_rh ) = @_;

    $config_rh ||= {};

    my ( @transactions, %where );

    if ( defined $config_rh->{status} ) {

        $where{status} = lc $config_rh->{status};
    }
    elsif ( defined $config_rh->{status_rh} ) {

        $where{status} = { '$in' => [
            map { ( lc $_ => 1 ) } keys %{ $config_rh->{status_rh} }
        ] };
    }
    elsif ( defined $config_rh->{status_ra} ) {

        $where{status} = { '$in' => [
            map { ( lc $_ => 1 ) } @{ $config_rh->{status_ra} }
        ] };
    }
    else {

        $where{status} = { '$in' => [qw( pending paid )] };
    }

    my $account        = $config_rh->{account} || _default_account();
    my $cache_override = $self->{__cache_override};
    my $cache_dir      = $self->{__cache_dir};
    my $categorize_rc  = $self->{__categorize_rc} || \&categorize;
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

    my $account_number = $balance_rh->{account_number};

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

    my $parser = Finance::Bank::NFCU::HTML::BillPay->new(
        {   html           => $html,
            categorize     => $categorize_rc,
            compute_epoch  => \&_compute_epoch,
            cache_dir      => $cache_dir,
            account_hr     => { $account_number => $account },
            sequence_start => 1,
        }
    );

    my $trans_ar = $parser->get_transactions( \%where );

    for my $trans_hr ( @{$trans_ar} ) {

        if ( $trans_hr->{date} eq 'pending' ) {

            $trans_hr->{date} = _formatted_date();
        }
    }

    return $trans_ar;
}

sub get_future_transactions {
    my ( $self, $config_rh ) = @_;

    my ( $account, $schedule_rh, $to_date ) = _parse_config(
        $config_rh,
        {   account     => qr{\A ( \w+ \s \w+ ) \z}xms,
            schedule_rh => {},
            to_date     => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
        }
    );

    my $account_number;

    my $to_epoch
        = $to_date
        ? _compute_epoch($to_date)
        : 90 * 86_400 + _compute_epoch();

    $account ||= _default_account();

    croak "no schedule data given"
        if not keys %{$schedule_rh};

    my $trans_ar = $self->get_transactions( { account => $account } );

    my ( $from_epoch, %last_occur_for );

    TRANS:
    for my $trans_hr ( @{$trans_ar} ) {

        $account_number ||=
              $trans_hr->{account_number}
            ? $trans_hr->{account_number}
            : $account_number;

        my $category = $trans_hr->{category};
        my $epoch    = $trans_hr->{epoch};
        my $date     = $trans_hr->{date};

        next TRANS
            if $trans_hr->{status} ne 'confirmed'
            && $trans_hr->{status} ne 'pending';

        next TRANS
            if !exists $schedule_rh->{$category};

        next TRANS
            if !defined $epoch;

        if ( !exists $last_occur_for{$category} ) {

            $from_epoch = $epoch;

            $last_occur_for{$category} = $epoch;
        }

        my $last_count  = keys %last_occur_for;
        my $sched_count = keys %{$schedule_rh};

        last TRANS
            if $last_count == $sched_count;
    }

    my @transactions;

    my $epoch = $from_epoch;

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

            next CAT
                if $days_elapsed <= 0;

## print "$category $date days elapsed: ( $epoch - $last_occur_epoch ) / 86_400 => $days_elapsed\n"
##     if $days_elapsed == 31 && $category eq 'housing';

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
                            account_number => $account_number,
                            amount         => $amount,
                            amount_str     => $amount_str,
                            category       => $category,
                            date           => $date,
                            epoch          => $epoch,
                            item           => $item,
                            source         => 'schedule',
                            status         => 'predicted',
                            _sequence      => 0,
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
                            account_number => $account_number,
                            amount         => $amount,
                            amount_str     => $amount_str,
                            category       => $category,
                            date           => $date,
                            epoch          => $epoch,
                            item           => $item,
                            source         => 'schedule',
                            status         => 'predicted',
                            _sequence      => 0,
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
                            account_number => $account_number,
                            amount         => $amount,
                            amount_str     => $amount_str,
                            category       => $category,
                            date           => $date,
                            epoch          => $epoch,
                            item           => $item,
                            source         => 'schedule',
                            status         => 'predicted',
                            _sequence      => 0,
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
                        account_number => $account_number,
                        amount         => $amount,
                        amount_str     => $amount_str,
                        category       => $category,
                        date           => $date,
                        epoch          => $epoch,
                        item           => $item,
                        source         => 'schedule',
                        status         => 'predicted',
                        _sequence      => 0,
                    );
                    push @transactions, \%transaction;
                }
            }
        }

        $epoch += 86_400;
    }

    my $pending_boundary;

    PENDING:
    for my $i ( 0 .. $#{ $trans_ar }) {
        if ( $trans_ar->[$i]->{status} eq 'confirmed' ) {
            $pending_boundary = $i - 1;
            last PENDING;
        }
    }

    push @transactions, splice @{$trans_ar}, 0, $pending_boundary;

    @transactions = sort { $b->{epoch} <=> $a->{epoch} } @transactions;

    my $balance = $trans_ar->[0]->{balance};

    for my $trans_hr ( reverse @transactions ) {

        $balance += $trans_hr->{amount};

        $trans_hr->{balance}     = $balance;
        $trans_hr->{balance_str} = $balance;

        _as_dollars( \$trans_hr->{balance_str} );
    }

    return _merge( $trans_ar, \@transactions );
}

sub get_expenditure_report {
    my ( $self, $config_rh ) = @_;

    my ( $account, $from_date, $to_date ) = _parse_config(
        $config_rh,
        {   account    => qr{\A ( \w+ (?: \s \w+ )* ) \z}xms,
            from_date  => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
            to_date    => qr{\A ( \d{1,2} / \d{1,2} / \d{4} ) \z}xms,
        }
    );

    my $from_epoch = _compute_epoch($from_date);
    my $to_epoch   = _compute_epoch($to_date);

    $account ||= _default_account();

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %total_for;
    {
        my $trans_ar = $self->get_transactions( { account => $account } );

        TRANS:
        for my $trans_hr (@{ $trans_ar }) {

            my $amount   = $trans_hr->{amount};
            my $category = $trans_hr->{category};
            my $epoch    = $trans_hr->{epoch};

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
        my $monthly_average   = $total / ( $months || 1 );
        my $weekly_average    = $total / ( $weeks || 1 );

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

    my ( $first_epoch, $last_epoch ) = ( 0, 0 );

    my %schedule;
    {
        my $trans_ar = $self->get_transactions();

        TRANS:
        for my $trans_hr (@{ $trans_ar }) {

            my $amount   = $trans_hr->{amount};
            my $category = $trans_hr->{category};
            my $epoch    = $trans_hr->{epoch};
            my $item     = $trans_hr->{item};

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

1;
