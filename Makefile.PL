use 5.010001;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Finance::Bank::NFCU',
    VERSION_FROM      => 'lib/Finance/Bank/NFCU.pm', # finds $VERSION
    PREREQ_PM         => {
        'WWW::Mechanize'  => '1.66', # other versions not tested
        'Data::Dumper'    => '2.101',
        'Date::Manip::TZ' => '5.56',
        'Env'             => '1.00',
        'Readonly'        => '1.02',
        'File::Spec'      => '3.33',
        'File::Slurp'     => '9999.13',
    },
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Finance/Bank/NFCU.pm', # retrieve abstract from module
       AUTHOR         => 'Dylan Doxey <dylan@>') : ()),
);
