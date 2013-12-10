# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Finance-Bank-NFCU.t'

#########################

use Test::More 'no_plan';

BEGIN {
    use_ok('Finance::Bank::NFCU::Category');
    can_ok( 'Finance::Bank::NFCU::Category', '_build_regex' );
}

#########################

my @items = (
    'ACH Transaction - MTG+ HOME LOANS   ONLINE PMT',
    'ACH Transaction - TWI AZBLE        ONLINE PMT',
    'ACH Transaction - GATEWAYONELENDIN ONLINE PMT',
    'ACH Transaction - GE MONEY BANK    ONLINE PMT',
    'ACH Transaction - NATIONWIDEALLIED ONLINE PMT',
    'ACH Transaction - L.A. WATER DIST  ONLINE PMT',
    'ACH Transaction - PAYPAL           INST XFER',
    'ACH Transaction - REPUBLIC SERVICE ONLINE PMT',
    'ACH Transaction - PG&amp;E            ONLINE PMT',
    'ACH Transaction - TOYOTA/LEXUS     ONLINE PMT',
    'ACH Transaction - AT&amp;T WIRELESS ONLINE PMT',
    'ATM Deposit',
    'ATM Withdrawal - CT 7-11                CULVER VALLEYORUS',
    'ATM Withdrawal - Hollywood County CU    Poway        ORUS',
    'Deposit - FRANCHISE TAX BDTAX-REFUND',
    'Deposit - N768 Google DIR DEP',
    'Deposit - PAYPAL          TRANSFER',
    'Deposit - US TREASURY 332   TAX REF',
    'Dividend',
    'POS Adjustment - THE HOME DEPOT 464     PORTLAND  AZ',
    'POS Debit - CUCARD 1111 - #06707 ALBERTSONS      PORTLAND  ORUS',
    'POS Debit - CUCARD 1111 - 7-ELEVEN               TUSCON    ORUS',
    'POS Debit - CUCARD 1111 - ARCO PAYPOINT          CULVER CITY      ORUS',
    'POS Debit - CUCARD 1111 - COSTCO GAS #75       PARRIS        ORUS',
    'POS Debit - CUCARD 1111 - NIJIYA MARKET # 30 C TUSCON    ORUS',
    'POS Debit - CUCARD 1111 - PETCO ANIMAL SUPPLIES  CULVER CITY      ORUS',
    'POS Debit - CUCARD 1111 - SPROUTS FARMERS MARKET CULVER CITY      ORUS',
    'POS Debit - CUCARD 1111 - THE HOME DEPOT 6     PORTLAND  ORUS',
    'POS Debit - CUCARD 1111 - THE HOME DEPOT #8949   PARRIS        ORUS',
    'POS Debit - CUCARD 1111 - VONS 203              CULVER CITY      ORUS',
    'POS Debit - CUCARD 1111 - VONS     Store  203   CULVER CITY      ORUS',
    'POS Debit - Visa Check Card 1111 - AMAZON.COM             AMZN.COM/BILLWA',
    'POS Debit - Visa Check Card 1111 - AMAZON MKTPLACE PM     AMZN.COM/BILLWA',
    'POS Debit - Visa Check Card 1111 - AMAZON MKTPLACE PM    AMZN.COM/BILLWAUS',
    'POS Debit - Visa Check Card 1111 - COUNTRY KABOB GREE     TUSCON    AZ',
    'POS Debit - Visa Check Card 1111 - CPC*IMAGEKIND          800-3518986  AZ',
    'POS Debit - Visa Check Card 1111 - EXPRESS TIRE #556       PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - FRY&#039;S ELECTRONICS      TUSCON    AZ',
    'POS Debit - Visa Check Card 1111 - CHEEZEY CUISINE      800-461-2503 AZ',
    'POS Debit - Visa Check Card 1111 - ICHIBAN SUSHI          PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - ICHIBAN SUSHI         PARRIS        ORUS',
    'POS Debit - Visa Check Card 1111 - BIZUMI JAPANESE RES     PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - NAPA AUTO PARTS #22     PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - NETFLIX.COM            NETFLIX.COM  AZ',
    'POS Debit - Visa Check Card 1111 - PHO JOY AUTHENTIC      PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - POORBOYSUBSWRAPSSA     PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - SPICY HOUSE            TUSCON    AZ',
    'POS Debit - Visa Check Card 1111 - SPIRIT VOYAGE LIVE     HERNDON      VA',
    'POS Debit - Visa Check Card 1111 - STARBUCKS CORP0005     PARRIS        AZ',
    'POS Debit - Visa Check Card 1111 - THE HOME DEPOT 332     PORTLAND  AZ',
    'POS Debit - Visa Check Card 1111 - THE HOME DEPOT 876     TUSCON    AZ',
    'POS Debit - Visa Check Card 1111 - TWIGGS BAKERY COFF     TUSCON    AZ',
    'POS Debit - Visa Check Card 1111 - VOLVO OF OAK PARK      708-848-8500 IL',
    'Transfer From Shares',
    'Transfer To Credit Card',
    'Transfer To GEORGE HENRY-ARNOLD',
);

ITEM:
for my $item (@items) {

    my $regex = Finance::Bank::NFCU::Category::_build_regex($item);

    like( $item, qr{ $regex }xmsi, "correct regex for: $item" ) || last ITEM;
}

__END__
