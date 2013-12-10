use 5.010001;
use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME         => 'Finance::Bank::NFCU',
    VERSION_FROM => 'lib/Finance/Bank/NFCU.pm',    # finds $VERSION
    PREREQ_PM    => {
        'Cwd'               => '0',
        'Data::Dumper'      => '2.101',
        'Date::Manip::TZ'   => '5.56',
        'DateTime'          => '0.65',
        'Env'               => '1.00',
        'File::Slurp'       => '9999.13',
        'File::Spec'        => '3.33',
        'IO::Prompter'      => '0',
        'JSON'              => '0',
        'List::MoreUtils'   => '0',
        'Readonly'          => '1.02',
        'Term::ReadKey'     => '0',
        'Text::FormatTable' => '0',
        'WWW::Mechanize'    => '1.66',
    },
    (   $] >= 5.005
        ?    ## Add these new keywords supported since 5.005
            (
            ABSTRACT_FROM => 'lib/Finance/Bank/NFCU.pod',    # retrieve abstract from module
            AUTHOR        => 'Dylan Doxey <dylan@cpan.org>'
            )
        : ()
    ),
);
