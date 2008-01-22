#!/usr/bin/perl

use Benchmark qw(cmpthese timeit);

use Path::Class;

die ".pmc's missing from 'compiled_lib', please run compile.sh" unless -f dir( $FindBin::Bin )->subdir("compiled_lib")->file("Point.pmc");

use FindBin;

print <<'BLURB';
The following tests will run:

    just_fork:               Just a fork & waitpid to compare with the overhead of starting a new environment

    plain_moose:             Mint moose based Point.pm & Point3D.pm

    class_accessor:          Class::Accessor based Point & Point3D

    moose_pmc:               Loads Point, Point3D and Moose::Object from .pmc and
                             nothing more (classes are usable but no using the MOP
                             causes a .mopc load overhead)

    moose_pmc_and_all_meta:  Loads the .pmc files and all the meta .mopc files
                             for Point, Point3D and Moose::Object

    moose_pmc_and_one_meta:  Loads the .pmc files and only Point's .mopc


Benchmarking...
BLURB

# bah, for some reason Benchmark seems to hang when providing e.g. -3
# instead we give each test it's own fudged factor and multiply by $it

my $it = shift || 5;

cmpthese({
    just_fork => timeit(50 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            exit;
        }
    }),
    plain_moose => timeit(3 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            die "blah: $!" unless defined $pid;

            unshift @INC, "$FindBin::Bin/../../t/lib";

            require Moose::Object;
            require Point;
            require Point3D;

            Moose::Object->meta;
            Point->meta;
            Point3D->meta;

            exit;
        }
    }),
    class_accessor => timeit(5 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            die "blah: $!" unless defined $pid;

            unshift @INC, "$FindBin::Bin/class_accessor";

            require Point;
            require Point3D;

            exit;
        }
    }),
    moose_pmc => timeit(10 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            die "blah: $!" unless defined $pid;

            unshift @INC, "$FindBin::Bin/compiled_lib";

            require Moose::Object;
            require Point;
            require Point3D;

            exit;
        }
    }),
    moose_pmc_and_all_meta => timeit(3 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            die "blah: $!" unless defined $pid;

            unshift @INC, "$FindBin::Bin/compiled_lib";

            require Moose::Object;
            require Point;
            require Point3D;

            MooseX::Compile::Bootstrap->load_cached_meta( class => "Moose::Object", pmc_file => $INC{'Moose/Object.pm'} . 'c' );
            MooseX::Compile::Bootstrap->load_cached_meta( class => "Point",         pmc_file => $INC{'Point.pm'} . 'c' );
            MooseX::Compile::Bootstrap->load_cached_meta( class => "Point3D",       pmc_file => $INC{'Point3D.pm'} . 'c' );

            exit;
        }
    }),
    moose_pmc_and_one_meta => timeit(3 * $it, q{
        if ( my $pid = fork ) {
            waitpid $pid, 0;
        } else {
            die "blah: $!" unless defined $pid;

            unshift @INC, "$FindBin::Bin/compiled_lib";

            require Moose::Object;
            require Point;
            require Point3D;

            MooseX::Compile::Bootstrap->load_cached_meta( class => "Point", pmc_file => $INC{'Point.pm'} . 'c' );

            exit;
        }
    }),
});

