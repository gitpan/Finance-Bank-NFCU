use 5.010001;
use ExtUtils::MakeMaker;
WriteMakefile(
    NAME              => 'Finance::Bank::NFCU',
    VERSION_FROM      => 'lib/Finance/Bank/NFCU.pm',
    PREREQ_PM         => {
        'Data::Dumper'    => '2.101',
        'Date::Manip::TZ' => '5.56',
        'DateTime'        => '0.65',
        'Env'             => '1.00',
        'File::Slurp'     => '9999.13',
        'File::Spec'      => '3.33',
        'Readonly'        => '1.02',
        'WWW::Mechanize'  => '1.66',
        'IO::Prompt'      => '0.997001',
    },
    ($] >= 5.005 ?
      (ABSTRACT_FROM  => 'lib/Finance/Bank/NFCU.pm',
       AUTHOR         => 'Dylan Doxey <dylan@>') : ()),
);
