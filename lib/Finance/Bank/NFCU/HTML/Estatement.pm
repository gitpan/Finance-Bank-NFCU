package Finance::Bank::NFCU::HTML::Estatement;

use strict;
use warnings;
use base qw( Finance::Bank::NFCU::HTML );
{
    use Carp;
    use Readonly;
}

my ( $MONEY_REGEX, $AMOUNTS_REGEX, $ACCOUNT_NUMBER_REGEX,
    $BEGINNING_BALANCE_REGEX, $ENDING_BALANCE_REGEX );
{
    Readonly $MONEY_REGEX   => qr{ \d+ (?: ,\d+ )? [.] \d\d }xms;
    Readonly $AMOUNTS_REGEX => qr{ $MONEY_REGEX-? \s+ $MONEY_REGEX }xms;
    Readonly $ACCOUNT_NUMBER_REGEX => qr{
        \A
            \w+ \s+ (?: Checking|Savings )+
            \s+ -+ \s+
            ( \d+ )
    }xmsi;
    Readonly $BEGINNING_BALANCE_REGEX => qr{
        \A
            \d+ - \d+ \s+
            Beginning \s+ Balance
            \s+
    }xmsi;
    Readonly $ENDING_BALANCE_REGEX => qr{
        \A
            \d+ - \d+ \s+
            Ending \s+ Balance
            \s+
    }xmsi;
}

sub new {
    my ( $class, @args ) = @_;

    my $self = $class->SUPER::new( @args );

    if ( $self->{html} =~ m{<pre> \s+ ( .+? ) \s+ </pre>}xmsi ) {

        my $text = $1;

        my ($yr) = $text =~ m{
            STATEMENT \s PERIOD \s+
            (?: \w+ \s+ )?  \d+ \W \d+ \W ( \d+ )
        }xms;

        my $year = "20$yr";    # This program in use after 2099?

        $self->{text} = $text;
        $self->{year} = $year;
    }

    return bless $self, $class;
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

    my ( @transactions, %has_payee );

    my @lines = split /\n+/, $self->{text};

    my $year              = $self->{year};
    my $account_number    = 0;
    my $last_month        = 0;
    my $sequence_number   = $self->{sequence_start};

    LINE:
    while (@lines) {

        my $item = shift @lines;

        if (   @lines > 1
            && $lines[0] =~ m{\A Joint \s+ Owner }xmsi
            && $item =~ $ACCOUNT_NUMBER_REGEX )
        {
            $account_number = $1;
        }
        elsif ( $item =~ $ENDING_BALANCE_REGEX ) {

            $account_number = 0;
        }

        next LINE
            if !$account_number;

        next LINE
            if $item !~ m{\A \d+ \W \d+ \s }xms;

        next LINE
            if $item =~ $BEGINNING_BALANCE_REGEX;

        # consolidate multiline description
        {
            my $amounts;

            if ( $item =~ m{\s ( $AMOUNTS_REGEX ) \s* \z}xms ) {
                $amounts = $1;
                $item =~ s{\s $AMOUNTS_REGEX \s* \z}{}xms;
            }

            while (@lines
                && $lines[0] =~ m{\A \s+ }xms
                && $lines[0] !~ m{\A \s+ statement \s of \s account }xmsi )
            {
                my $line = shift @lines;

                if ( $line =~ m{\s ( $AMOUNTS_REGEX ) \s* \z}xms ) {
                    $amounts = $1;
                    $line =~ s{\s $AMOUNTS_REGEX \s* \z}{}xms;
                }

                $item .= " $line";
            }

            if ($amounts) {

                $item .= " $amounts";
            }
        }

        my $detail_hr = $self->_parse_desc( \$item, $year );

        my $date = delete $detail_hr->{date};

        # fix year transition
        {
            my ($m) = split m{\D}, $date;
            $last_month ||= $m;
            if ( $m == 1 && $last_month == 12 ) {
                $year++;
                $date =~ s{/ \d+ \z}{/$year}xms;
            }
            $last_month = $m;
        }

        my ( $balance_str, $amount_str ) = reverse split /\s+/, $item;

        $item =~ s{ \s+ \S+ \s+ \S+ \z}{}xms;

        my $amount  = $self->_to_cents($amount_str);
        my $balance = $self->_to_cents($balance_str);

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
            balance        => $balance,
            balance_str    => $balance_str,
            category       => $category,
            date           => $date,
            details        => $detail_hr,
            epoch          => $epoch,
            item           => $item,
            source         => 'estatement',
            status         => 'confirmed',
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

    $self->{transactions} = [
        map { $_->{_sequence} = $sequence_number++; $_ }
        reverse @transactions
    ];

    return [ @{ $self->{transactions} } ];
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU::HTML::Estatement - Used internally with Finance::Bank::NFCU.

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
