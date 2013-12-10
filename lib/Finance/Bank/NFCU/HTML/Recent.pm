package Finance::Bank::NFCU::HTML::Recent;

use strict;
use warnings;
use base qw( Finance::Bank::NFCU::HTML );
{
    use Carp;
}

sub get_payees {
    my ($self) = @_;

    return [ @{ $self->{payees} } ]
        if $self->{payees};

    $self->get_transactions();

    return [ @{ $self->{payees} } ];
}

sub get_transactions {
    my ( $self, $where_hr ) = @_;

    return [ @{ $self->{transactions} } ]
        if $self->{transactions};

    $where_hr ||= {};

    croak 'where clause must be a HASH ref'
        if ref $where_hr ne 'HASH';

    my $tomorrow;
    {
        my $epoch = 86_400 + time;
        my ( $m, $d, $y ) = ( localtime $epoch )[ 3, 4, 5 ];
        $tomorrow = sprintf '%02d/%02d/%04d', 1 + $m, $d, 1900 + $y;
    }

    my ( @transactions, %has_payee );

    my $trans_regex = qr{
            ( [\w/]+ )               \s*
            (?: </a> )?              \s*
        </td>                        \s*
        <td>                         \s*
            ( \S .+? )               \s*
        </td>                        \s*
        <td \s class="tdAmt">        \s*
            \$ ( \S+ )               \s*
        </td>                        \s*
        <td \s class="tdAmt \s stR"> \s*
            \$ ( \S+ )               \s*
        </td>
    }xms;

    my $account_number = $self->_parse_account_number();

    while ( $self->{html} =~ m{ $trans_regex }xmsg ) {

        my ( $status_date, $item )        = ( lc $1, $2 );
        my ( $amount_str,  $balance_str ) = ( $3, $4 );

        my $amount  = $self->_to_cents($amount_str);
        my $balance = $self->_to_cents($balance_str);

        my $date
            = $status_date =~ m{\A \d+ \W }xms
            ? $status_date
            : $tomorrow;

        my ($year) = reverse split m{\D}, $date;

        my $status
            = $date eq $tomorrow
            ? lc $status_date
            : 'confirmed';

        my $detail_hr = $self->_parse_desc( \$item, $year );

        my $category = $self->{categorize}->(
            {   date      => $date,
                item      => $item,
                amount    => $amount,
                cache_dir => $self->{cache_dir},
            }
        );

        my $epoch = $self->{compute_epoch}->( $status_date );

        my %transaction = (
            account_number => $account_number,
            amount         => $amount,
            amount_str     => $amount_str,
            balance        => $balance,
            balance_str    => $balance_str,
            category       => $category,
            date           => $status_date,
            details        => $detail_hr,
            epoch          => $epoch,
            item           => $item,
            source         => 'recent',
            status         => $status,
            _sequence      => 0,
        );

        if ( $self->_matches( \%transaction, $where_hr ) ) {

            push @transactions, \%transaction;
        }

        if ( exists $detail_hr->{payee} ) {

            $has_payee{ $detail_hr->{payee} } = 1;
        }
    }

    $self->{payees} = [ keys %has_payee ];

    my $sequence_number = $self->{sequence_start};

    $self->{transactions} = [
        map { $_->{_sequence} = $sequence_number++; $_ }
        @transactions
    ];

    return [ @{ $self->{transactions} } ];
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU::HTML::Recent - Used internally with Finance::Bank::NFCU.

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
