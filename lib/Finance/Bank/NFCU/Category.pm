package Finance::Bank::NFCU::Category;

use strict;
use warnings;
use base qw( Exporter );
{
    use IO::Prompter;
    use Cwd qw( cwd );
    use Data::Dumper;
    use File::Slurp qw( write_file );
    use List::MoreUtils qw( uniq );
    use Text::FormatTable;
    use Term::ReadKey qw( GetTerminalSize );
    use English qw( -no_match_vars $OS_ERROR );
}

@Finance::Bank::NFCU::Category::EXPORT_OK = qw(
    categorize
    set_cache_dir
);

my ( $Cache_Dirname, $Category_Rh, @Categories );

sub categorize {
    my ($param_rh) = @_;

    my $date      = $param_rh->{date}      || "";
    my $item      = $param_rh->{item}      || "";
    my $amount    = $param_rh->{amount}    || "";
    my $cache_dir = $param_rh->{cache_dir} || "";

    if ( !@Categories ) {

        $Cache_Dirname ||= $cache_dir;

        if ( !$Cache_Dirname ) {

            my ($cwd) = cwd() =~ m{\A ( .+ ) \z}xms;

            $Cache_Dirname = $cwd;
        }

        my $data_rh;
        {
            my $filename
                = File::Spec->catfile( $Cache_Dirname, 'category.dat' );

            if ( stat $filename ) {

                $data_rh = do $filename;
            }
            $data_rh ||= {};
        }

        $Category_Rh = $data_rh->{category_rh} || {};

        my $criterion_Rh = $data_rh->{criterion_rh} || {};

        for my $category ( keys %{$criterion_Rh} ) {

            for my $criterion_ra ( @{ $criterion_Rh->{$category} } ) {

                my ( $regex, $range_ra, $pennies ) = @{$criterion_ra};

                push @Categories,
                    {
                    category => $category,
                    regex    => $regex,
                    range_ra => $range_ra,
                    pennies  => $pennies,
                    };
            }
        }

        @Categories = sort { _cat_cmp( $b, $a ) } @Categories;
    }

    my $check_number;

    if ( $item =~ m{ check \s+ ( \d+ ) }xmsi ) {

        $check_number = $1;

        return $Category_Rh->{$check_number}
            if exists $Category_Rh->{$check_number};
    }

    CRIT:
    for my $criterion_rh (@Categories) {

        my $category = $criterion_rh->{category};
        my $regex    = $criterion_rh->{regex};
        my $range_ra = $criterion_rh->{range_ra};
        my $pennies  = $criterion_rh->{pennies};

        next CRIT
            if $item !~ $regex;

        if ($amount) {

            if ( $range_ra && @{$range_ra} ) {

                my ( $min, $max ) = sort { $a <=> $b } @{$range_ra};

                next CRIT
                    if $amount < $min || $max < $amount;
            }

            if ( $pennies && length $pennies ) {

                my ($cents) = $amount =~ m{ [.] ( \d\d ) }xms;

                next CRIT
                    if !defined $cents;

                next CRIT
                    if $pennies ne $cents;
            }
        }

        return $category;
    }

    my $category = _promp_category( $date, $item, $amount );

    return $category
        if $category;

    die "uncategorizable transaction: '$item' for $amount\n";
}

sub _promp_category {
    my ( $date, $item, $amount ) = @_;

    my ( @categories, %is_defined, %category_for, $longest );
    {
        @categories = uniq sort map { $_->{category} } @Categories;

        %is_defined = map { ( $_ => 1 ) } @categories;

        %category_for = do {
            my $n = 0;
            map { ( ++$n, $_ ) } @categories;
        };

        my $format = @categories;

        $format
            = $format < 10   ? '%.1d) %s'
            : $format < 100  ? '%.2d) %s'
            : $format < 1000 ? '%.3d) %s'
            :                  '%.4d) %s';

        @categories = do {
            my $n = 0;
            map { sprintf $format, ++$n, $_ } @categories;
        };

        $longest = 0;

        map { $longest = $longest < length $_ ? length $_ : $longest }
            @categories;
    }

    if ($longest) {

        my $width = ( GetTerminalSize() )[0] || 80;

        my $n = sprintf '%d', ( $width - 1 ) / ( $longest + 3 );

        my $format = sprintf ' %s ', join ' ', (' l ') x $n;

        my $table = Text::FormatTable->new($format);

        while ( my @cats = splice @categories, 0, $n ) {

            while ( @cats < $n ) {

                push @cats, "";
            }

            $table->row(@cats);
        }

        print "\n", $table->render($width), "\n"
            or die "print error: $OS_ERROR\n";
    }

    $item =~ s{ \s+ }{ }xmsg;

    print "   date: $date\n";
    print "   item: $item\n";
    printf "amount: %s\n", $amount / 100;

    my $category;

    CAT:
    {
        $category = IO::Prompter::prompt( 'Enter a category: ', -_ );

        redo CAT
            if !$category;

        if ( $category =~ m{\A ( \d+ ) \z}xms ) {

            my $id = int $1;

            if ( exists $category_for{$id} ) {

                $category = $category_for{$id};
            }
            else {

                warn "$id not found above!\n";
                redo CAT;
            }
        }
    }

    if ( $item =~ m{ check \s+ ( \d+ ) }xmsi ) {

        my $check_number = $1;

        $Category_Rh->{$check_number} = $category;
    }
    else {

        my $regex = _build_regex($item);

        my ( $range, $pennies );
        {
            $range = IO::Prompter::prompt( 'Qualifying range: ', -_ );

            if ( $item =~ m{ transfer \s to \s (?: other|checking ) }xmsi ) {

                $pennies = IO::Prompter::prompt( 'Pennies value: ', -_ );
            }

            $range = length $range ? [ split m{ \s+ }xms, $range ] : undef;
            $pennies = length $pennies ? 0 + $pennies : undef;
        }

        push @Categories,
            {
            range_ra => $range,
            regex    => $regex,
            pennies  => $pennies,
            category => $category,
            };

        @Categories = sort { _cat_cmp( $b, $a ) } @Categories;
    }

    _store( $Category_Rh, \@Categories );

    return $category;
}

sub _build_regex {
    my ($item) = @_;

    $item ||= "";

    if ( $item =~ m{ paid \s+ check \s+ ( \d+ ) }xmsi ) {

        return qr{ paid \s+ check \s+ $1 \s }xmsi;
    }

    my @chaffe_regexes = (
        qr{ pos \s+ debit }xmsi,
        qr{ visa \s+ check \s+ card \s+ \d+ }xmsi,
        qr{ cucard \s+ \d+ (?: \s+ transaction )? }xmsi,
    );

    RE:
    for my $regex (@chaffe_regexes) {

        $item =~ s{ $regex \s+ (?: - | \d+-\d+-\d+ \s+ )? }{}xmsi;
    }

    my @tokens;

    for my $token ( split m{ \s+ }xms, $item ) {

        if ( $token =~ m{\A \d+ \z}xms ) {

            push @tokens, '\d+';
        }
        elsif ( $token && $token !~ m{\A \s+ \z}xms ) {

            $token =~ s{ \W }{.}xmsg;

            push @tokens, lc $token;
        }
    }

    my $regex = sprintf '(?: \A | \s | \b ) %s (?: \b | \s | \z )',
        join ' \s+ ',
        @tokens;

    return qr{$regex}xmsi;
}

sub _store {
    my ( $category_rh, $category_ra ) = @_;

    my %criterion;

    for my $cate_rh ( @{$category_ra} ) {

        my $category = $cate_rh->{category};

        $criterion{$category} ||= [];

        push @{ $criterion{$category} },
            [ $cate_rh->{regex}, $cate_rh->{range_ra}, $cate_rh->{pennies}, ];
    }

    my $filename = "$Cache_Dirname/category.dat";

    my %data = (
        category_rh  => $Category_Rh,
        criterion_rh => \%criterion,
    );
    return write_file( $filename, Dumper( \%data ) );
}

sub _cat_cmp {
    my ( $x, $y ) = @_;
    my $x_has_range = defined $x->{range_ra} ? 1 : 0;
    my $y_has_range = defined $y->{range_ra} ? 1 : 0;
    return $x_has_range <=> $y_has_range;
}

1;

__END__

=head1 NAME

Finance::Bank::NFCU::Category - Module provides the interactive categorizer
function for use with Finance::Bank::NFCU.

=head1 SYNOPSIS

The categorize function is used by default when none is provided. This module
has no purpose as a stand-alone.

=head1 AUTHOR

Dylan Doxey, E<lt>dylan@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Dylan Doxey

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
