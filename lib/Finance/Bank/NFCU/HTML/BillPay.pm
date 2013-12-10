package Finance::Bank::NFCU::HTML::BillPay;

use strict;
use warnings;
use base qw( Finance::Bank::NFCU::HTML );
{
    use Carp;
    use JSON qw( decode_json from_json );
    use Data::Dumper;
    use Readonly;
}

my ( $ACCOUNT_NUMBER_REGEX );
{
    Readonly $ACCOUNT_NUMBER_REGEX => qr{
        <td [^>]+ >                                         \s*
            <label [^>]+ >                                  \s*
                <strong>Pay \W nbsp;From \W nbsp;</strong>  \s*
            </label>                                        \s*
        </td>                                               \s*
        <td \s colspan="2">                                 \s*
        \W ( \d \d \d \d \d  )
    }xms;
}

sub get_payees {
    my ($self) = @_;

    return [ @{ $self->{payees} } ]
        if $self->{payees};

    $self->{payees} = $self->_parse_bizobjregistries('payee');

    return [ map { $_->{upperName} } @{ $self->{payees} } ];
}

sub get_transactions {
    my ( $self, $where_hr ) = @_;

    return [ @{ $self->{transactions} } ]
        if $self->{transactions};

    $where_hr ||= {};

    croak 'where clause must be a HASH ref'
        if ref $where_hr ne 'HASH';

    my $account_number;

    if ( $self->{html} =~ $ACCOUNT_NUMBER_REGEX ) {

        my $acct_num = $1;

        NUM:
        for my $number ( keys %{ $self->{account_for} } ) {

            my $num = substr $number, -5, 5;

            if ( $num eq $acct_num ) {

                $account_number = $number;

                last NUM;
            }
        }
    }

    die "couldn't parse account number"
        if !$account_number;

    my $payment_ar = $self->_parse_bizobjregistries('payment');

##print Dumper( $payment_ar );

    my @transactions;

    PAYMENT:
    for my $payment_hr ( @{$payment_ar} ) {

        my $amount_str = $payment_hr->{amount} . '-';
        my $amount     = $self->_to_cents($amount_str);
        my $date       = $payment_hr->{MMDDYYYY_dateToPay};
        my $item       = $payment_hr->{upperName};
        my $status     = lc $payment_hr->{status};

        next PAYMENT
            if !defined $amount;

        my ($year) = reverse split m{\D}, $date;

        my $detail_hr = $self->_parse_desc( \$item, $year );

        my $category = $self->{categorize}->(
            {   date      => $date,
                item      => $item,
                amount    => $amount,
                cache_dir => $self->{cache_dir},
            }
        );

        my $epoch = $self->{compute_epoch}->( $date );

        my %transaction = (
            account_number => $account_number,
            amount         => $amount,
            amount_str     => $amount_str,
            balance        => 0,
            balance_str    => "",
            category       => $category,
            date           => $date,
            details        => $detail_hr,
            epoch          => $epoch,
            item           => $item,
            source         => 'billpay',
            status         => $status,
            _sequence      => 0,
        );

        if ( $self->_matches( \%transaction, $where_hr ) ) {

            push @transactions, \%transaction;
        }
    }

    my $sequence_number = $self->{sequence_start};

    $self->{transactions} = [
        map { $_->{_sequence} = $sequence_number++; $_ }
        @transactions
    ];

    return [ @{ $self->{transactions} } ];
}

sub _parse_bizobjregistries {
    my ( $self, $type ) = @_;

    my $regex = qr/
        BizObjRegistry[.]add\( \s* '$type', \s+
            {                               \s+
                ( .+? )                     \s+
            }                               \s+
        \);?                                \s+
    /xms;

    my @objects;

    while ( $self->{html} =~ m{ $regex }xmsg ) {

        my $object = "{\n $1 \n}";

        $object =~ s{ ', \s ( \w ) }{',\n $1}xmsg;
        $object =~ s{ " }{\\"}xmsg;
        $object =~ s{ ' }{"}xmsg;
        $object =~ s{\n \s+ ( \w+ ): }{\n "$1":}xmsg;

        push @objects, decode_json($object);
    }

    return \@objects;
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU::HTML::BillPay - Used internally with Finance::Bank::NFCU.

=head1 SYNOPSIS

This module has no purpose as a stand-alone.

=head1 AUTHOR

Dylan Doxey, E<lt>dylan@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Dylan Doxey

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
