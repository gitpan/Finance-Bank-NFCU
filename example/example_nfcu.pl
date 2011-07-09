#!/usr/bin/perl -Tw

use strict;
use warnings;
{
    use IO::Prompt;
    use Text::Table;
    use Finance::Bank::NFCU;
}

my ( $access_number, $user_id, $password );
{
    $access_number = prompt( -integer => 'Account: ' );
    $user_id       = prompt( -integer => 'User ID: ' );
    local $SIG{__WARN__} = sub { return; };
    $password      = prompt( 'Password: ', -echo => '*' );
}

die "password, userid and account are required\n"
    if !$password || !$user_id || !$access_number;

my %credentials = (
    access_number => $access_number,
    user_id       => $user_id,
    password      => $password,
);
my $nfcu = Finance::Bank::NFCU->new( \%credentials )
    || die "failed to authenticate";

$nfcu->config( {
    cache_dir     => '/var/cache/nfcu',
    error_level   => 'fatal',
    categorize_rc => \&categorize,
} );

my $balances_ra = $nfcu->get_balances();

die "Your session has (probably) expired."
    if !defined $balances_ra;

print "Balances:\n\n";
for my $balance_rh (@{ $balances_ra }) {

    my $number  = $balance_rh->{account_number};
    my $account = $balance_rh->{account};
    my $dollars = $balance_rh->{balance_str};

    print "$number, $account -- $dollars\n";
}
print "\n";

my $transaction_ra = $nfcu->get_transactions();

print "All Transactions:\n\n";
for my $transaction_rh ( reverse @{ $transaction_ra } ) {

    my ( $date, $item, $amount_str, $balance_str, $status )
        = @{ $transaction_rh }{qw( date item amount_str balance_str status )};

    printf "%s\t% 50s\t% 10s\t% 10s\t% 10s\n", $date, $item, $amount_str, $balance_str, $status;
}
print "\n";

my $report_ra = $nfcu->get_expenditure_report();

print "Expenditure Report:\n";

my $tb = Text::Table->new( @{ shift @{ $report_ra } } );
$tb->load( @{ $report_ra } );

print "\n$tb\n\n";

sub categorize {
    my ( $item, $amount ) = @_;

    my %criterion_for = (
        'administrative' => [ [ qr{ \b dividend \b }xmsi, ], ],
        'health'         => [ [ qr{ \s pharmacy \s }xmsi, ], ],
        'charity'        => [
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
        'income' => [ [ qr{ \s deposit \s }xmsi, ], ],
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
        'housing' =>
            [ [ qr{ \s mortgage \s }xmsi, ], [ qr{ home \s* loan }xmsi, ], ],
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
        'utilities' =>
            [ [ qr{ \s gas \s & (?: amp; )? \s electric \b }xmsi, ], ],
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

    for my $category ( keys %criterion_for ) {

        CRIT:
        for my $criterion_ra ( @{ $criterion_for{$category} } ) {

            my ( $regex, $range_ra ) = @{$criterion_ra};

            if ($range_ra) {

                my ( $min, $max ) = sort { $a <=> $b } @{$range_ra};

                next CRIT
                    if $amount < $min || $max < $amount;
            }

            return $category
                if $item =~ $regex;
        }
    }

    return 'uncategorized';    # not fatal
}

1;
