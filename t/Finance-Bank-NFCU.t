# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Finance-Bank-NFCU.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 1;
BEGIN { use_ok('Finance::Bank::NFCU') };

#########################
