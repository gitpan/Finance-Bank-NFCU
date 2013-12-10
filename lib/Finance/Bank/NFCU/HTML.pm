package Finance::Bank::NFCU::HTML;

use strict;
use warnings;
{
    use Carp;
    use File::Slurp qw( read_file );
    use Readonly;
}

my ( $STATE_REGEX );
{
    Readonly $STATE_REGEX => qr{
        (?:
            AL | AK | AZ | AR | CA | CO | CT | DE | DC | FL | GA | HI | ID |
            IL | IN | IA | KS | KY | LA | ME | MD | MA | MI | MN | MS | MO |
            MT | NE | NV | NH | NJ | NM | NY | NC | ND | OH | OK | OR | PA |
            RI | SC | SD | TN | TX | UT | VT | VA | WA | WV | WI | WY
        )
    }xms;
}

sub new {
    my ( $class, $conf_hr ) = @_;

    my $html;
    {
        croak "you must supply HTML or a filename"
            if !$conf_hr->{html} && !$conf_hr->{filename};

        if ( $conf_hr->{html} ) {

            $html = $conf_hr->{html};
        }
        elsif ( stat $conf_hr->{filename} ) {

            $html = read_file($conf_hr->{filename});
        }
        else {

            croak "Couldn't find the HTML source";
        }

        $html =~ s{ &amp; }{\&}xmsg;
        $html =~ s{</?b>}{}xmsig;
        $html =~ s{<br \s* /? \s* >}{}xmsig;
    }

    croak "categorize doesn't look like a code ref"
        if ref $conf_hr->{categorize} ne 'CODE';

    croak "compute_epoch doesn't look like a code ref"
        if ref $conf_hr->{compute_epoch} ne 'CODE';

    croak "cache_dir doesn't look like a cache dir"
        if !$conf_hr->{cache_dir} || !-d $conf_hr->{cache_dir};

    croak "sequence_start doesn't look like an int"
        if $conf_hr->{sequence_start} !~ m{\A \d+ \z}xms;

    my %self = (
        html           => $html,
        payees         => 0,
        transactions   => 0,
        categorize     => $conf_hr->{categorize},
        compute_epoch  => $conf_hr->{compute_epoch},
        cache_dir      => $conf_hr->{cache_dir},
        account_for    => $conf_hr->{account_hr},
        sequence_start => $conf_hr->{sequence_start},
    );
    return bless \%self, $class;
}

sub _to_cents {
    my ( $self, $str ) = @_;

    return
        if !$str || $str !~ m{ \d }xms;

    my $sign = $str =~ m{ - \z}xms ? '-' : "";

    $str =~ s{(?: \A \W | \W \z )}{}xmsg;
    $str =~ s{,}{}xmsg;

    my ( $d, $c ) = split m{[.]}, $str;

    $c //= 0;

    return 0 + sprintf '%s%d%02d', $sign, $d, $c;
}

sub _parse_account_number {
    my ($self) = @_;

    my ($account_number) = $self->{html} =~ m{
        < \w+ > Account \s+ Details: \s*
        < \w+ \s id="h1Span"> ( \d+ )
    }xmsi;

    return $account_number || 0;
}

sub _parse_desc {
    my ( $self, $desc_sr, $year ) = @_;

    return {}
        if !$desc_sr;

    croak 'year required in _parse_desc'
        if !$year;

    ${ $desc_sr } =~ s{ \s+ }{ }xmsg;
    ${ $desc_sr } =~ s{(?: \A \s* | \s* \z)}{}xmsg;
    ${ $desc_sr } =~ s{ \s [#] \s+ ( \d ) }{ #$1}xms;
    ${ $desc_sr } =~ s{ & [#] ( \d+ ) ; }{ chr $1 }xmseg;
    ${ $desc_sr } =~ s{ -- }{ - }xms;

    if ( ${ $desc_sr } =~ m{\A ( .+? ) \s see \s legend \s below }xmsi ) {

        ${ $desc_sr } = $1;
    }

    my $desc = ${ $desc_sr };

    my %details;

    if ( $desc =~ m{ POS \s Debit \s }xmsi ) {

        $details{pos} = 1;
        
        ${ $desc_sr } =~ s{ \s* POS \s Debit \s (?: - \s? )? }{}xmsi;
    }

    if ( $desc =~ m{ Transaction \s ( \d+ )-( \d+ )-( \d+ ) \s }xmsi ) {

        $details{transaction_date} = "$1/$2/20$3";

        ${$desc_sr} =~ s{ Transaction \s \d+ - \d+ - \d+ \s }{}xmsi;
    }

    if ( $desc =~ m{ ( check \s card | cucard ) \s ( \d+ ) }xmsi ) {

        $details{card_type}   = $1;
        $details{card_number} = $2;

        ${ $desc_sr } =~ s{
            \s*
            (?: visa \s )?
            (?: check \s | cu ) card \s
            \d+
            (?: \s - \s )?
        }{}xmsi;
    }
    elsif ( $desc =~ m{ ( vcc ) \s+ ( \d+ ) }xmsi ) {

        $details{card_type}   = $1;
        $details{card_number} = $2;

        ${ $desc_sr } =~ s{ \s* vcc \s+ \d+ (?: \s - \s )? }{}xmsi;
    }

    if ( $desc =~ m{\A( .+? )\s<a\shref="( .+? )"[^>]+>( [^<]+ )</a>\z}xms ) {

        $details{url} = $2;
        $desc         = "$1 $3";
        ${$desc_sr}   = $desc;
    }

    if ( $desc =~ m/ \s - \s+ ( [^-]+ ) \z/xms ) {

        $details{payee} = $1;
    }
    elsif ( $desc =~ m/ \s \d{2}\W\d{2}\W\d{2} \s+ ( .+ ) \z/xms ) {

        $details{payee} = $1;
    }
    elsif ( $desc =~ m/[,\d]+[.]\d+-? \s+ [,\d]+[.]\d+ \s+ ( .+? ) \z/xms ) {

        $details{payee} = $1;
    }

    if ( $details{payee} ) {

        # remove amounts
        $details{payee} =~ s{ (?: \s+ \d [\d,.]+ -? )+ \z}{}xmsg;

        $details{payee} =~ s{ ($STATE_REGEX)US \z}{$1}xmsi;

        # tidy whitespace
        $details{payee} =~ s{ \s+ }{ }xms;
        $details{payee} =~ s{(?: \A \s* | \s* \z )}{}xmsg;
    }

    if ( $desc =~ m{\A ( \d+ \W \d+ ) \s }xms ) {

        $details{date} = "$1-$year";
    }
    elsif ( $desc =~ m{\s ( \d+ \W \d+ \W \d+ ) \s }xms ) {

        $details{date} = $1;
    }

    if ( $details{date} ) {

        $details{date} =~ s{\D}{/}xmsg;

        if ( $details{date} =~ m{\A( .+? )/( \d\d )\z}xms ) {

            $details{date} = "$1/20$2";
        }
    }

    # trim leading date token
    ${ $desc_sr } =~ s{\A \d+ \W \d+ \s+ (?: - \s+ )? }{}xms;

    # remove "paid to - "
    ${ $desc_sr } =~ s{ \s* paid \s+ to \s+ \W \s+ }{}xmsi;

    ${ $desc_sr } =~ s{ \s+ ([ACDF-VW][AC-EH-OR-VX-Z]) US \s* \z}{ $1}xmsi;

    ${ $desc_sr } =~ s{(?: \A \s* | \s* \z)}{}xmsg;

    return \%details;
}

sub _matches {
    my ( $self, $trans_hr, $where_hr ) = @_;

    return 1
        if !%{ $where_hr };

    FIELD:
    for my $field ( keys %{$where_hr} ) {

        return
            if !defined $trans_hr->{$field};

        if ( ref $where_hr->{$field} eq 'HASH' ) {

            my ($op)      = keys %{ $where_hr->{$field} };
            my ($list_ar) = values %{ $where_hr->{$field} };

            die "$op is not a recognized operator"
                if $op ne '$in';

            die "$op should indicated an ARRAY ref"
                if ref $list_ar ne 'ARRAY';

            my $hit;

            VALUE:
            for my $value (@{ $list_ar }) {

                if ( $trans_hr->{$field} eq $value ) {

                    $hit = 1;
                    last VALUE;
                }
            }

            return
                if !$hit;

            next FIELD;
        }

        return
            if $trans_hr->{$field} ne $where_hr->{$field};
    }

    return 1;
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU::HTML - Used internally with Finance::Bank::NFCU.

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
