#!/usr/bin/perl -Tw

# NFCU appears to use a cert which fails verification.
# Setting this environment var is a convenient work-around.
# Please email the author if you know better.
$ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;

use strict;
use warnings;
{
    use IO::Prompter;
    use Text::Table;
    use Finance::Bank::NFCU;
    use Term::ANSIColor::Print;
}

my ( $access_number, $user_id, $password );
{
    $access_number = IO::Prompter::prompt( -integer => 'Account: ' );
    $user_id       = IO::Prompter::prompt( -integer => 'User ID: ' );
    $password      = IO::Prompter::prompt( 'Password: ', -echo => '*' );
}

die "password, userid and account are required\n"
    if !$password || !$user_id || !$access_number;

my $print = Term::ANSIColor::Print->new(
    alias => {
        balance   => 'white_on_dark_green',
        confirmed => 'white_on_dark_green',
        paid      => 'white_on_dark_green',
        pending   => 'yellow_on_dark_green',
        predicted => 'white_on_dark_blue',
        text      => 'white_on_black',
    },
);

$print->text("\t");

my %credentials = (
    access_number => $access_number,
    user_id       => $user_id,
    password      => $password,
);
my $nfcu = Finance::Bank::NFCU->new( \%credentials )
    || die "failed to authenticate";

$nfcu->config(
    {   cache_dir   => '/var/cache/nfcu',
        error_level => 'fatal',
    }
);

my $balances_ra = $nfcu->get_balances();

die "Your session has (probably) expired."
    if !defined $balances_ra;

$print->text('Balances:');

for my $balance_rh ( @{$balances_ra} ) {

    $print->balance( sprintf "\t% 18s\t% 18s\t% 10s\t",
        @{$balance_rh}{qw( account_number account balance_str )} );
}

$print->text("\t");

my $transaction_ra = $nfcu->get_transactions();

$print->text('All Transactions:');

for my $transaction_rh ( reverse @{$transaction_ra} ) {

    my $status = $transaction_rh->{status};

    $print->$status(
        sprintf "\t%s\t% 60s\t% 13s\t% 10s\t% 10s\t% 10s\t",
        @{$transaction_rh}{
            qw(
                date
                item
                category
                amount_str
                balance_str
                status
                )
            }
    );
}
$print->text("\t");

my $report_ra = $nfcu->get_expenditure_report();

my $tb = Text::Table->new( @{ shift @{$report_ra} } );
$tb->load( @{$report_ra} );

$print->text('Expenditure Report:');
$print->confirmed($tb);

1;
