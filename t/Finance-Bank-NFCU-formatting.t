# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Finance-Bank-NFCU.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More 'no_plan';
BEGIN {
    use_ok('Finance::Bank::NFCU');
    can_ok( 'Finance::Bank::NFCU', '_as_cents', '_as_dollars' );
};

#########################

my %cents_for = (
    '-1,000'            => -1000,
    '-1000'             => -1000,
    '1000-'             => -1000,
    '-1000.00'          => -1000,
    '1000.00-'          => -1000,
    '-$10'              => -1000,
    '$10-'              => -1000,
    '-$10.00'           => -1000,
    '$10.00-'           => -1000,
    '-$10'              => -1000,
    '$10-'              => -1000,
    '-$10.00'           => -1000,
    '$10.00-'           => -1000,
    '$1,000-'           => -100000,
    '100000.00'         => 100000,
    '-3369.42857142857' => -3369.4285714286,
);
for my $money (keys %cents_for) {

    my $got = $money;

    Finance::Bank::NFCU::_as_cents( \$got );

    is( $got, $cents_for{$money}, "_as_cents: $money => " . $cents_for{$money} );
}

my %dollars_for = (
    '-1000'             => '$10.00-',
    '1000-'             => '$10.00-',
    '-1000.00'          => '$10.00-',
    '1000.00-'          => '$10.00-',
    '-$10'              => '$10.00-',
    '$10-'              => '$10.00-',
    '-$10.00'           => '$10.00-',
    '$10.00-'           => '$10.00-',
    '-$10'              => '$10.00-',
    '$10-'              => '$10.00-',
    '-$10.00'           => '$10.00-',
    '$10.00-'           => '$10.00-',
    '-3369.42857142857' => '$33.69-',
);
for my $money (keys %dollars_for) {

    my $got = $money;

    Finance::Bank::NFCU::_as_dollars( \$got );

    is( $got, $dollars_for{$money}, "_as_dollars: $money => " . $dollars_for{$money} );
}

