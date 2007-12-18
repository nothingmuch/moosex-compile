#!/usr/bin/perl

BEGIN {
    require Carp::Heavy;
    unshift @INC, sub {
        my ( $self, $file ) = @_;
#        warn "loading $file" . Carp::longmess if $file =~ /MOP|meta|Moose/i;
        return;
    }
}

#use Carp ();
#use Carp::Heavy ();
#BEGIN { unshift @INC, sub { warn "loading file $_[1] at " . times . "\n"; warn Carp::longmess() } }

package MooseX::Compile;

use strict;
use warnings;

if ( $ENV{MX_COMPILE_PRELOAD_MOOSE} ) {
    __PACKAGE__->load_moose();
}

our %known_pmc_files;

sub import {
    my ($self, @args) = @_;

    my ( $class, $file ) = caller();

    if ( $known_pmc_files{$file} ) {
        return $self->load_pmc( $class, $file, @args );
    } else {
        require Check::UnitCheck;
        Check::UnitCheck::unitcheckify(sub { $self->end_of_file_execution( $class, $file, @args ) });

        require Moose;
        shift; unshift @_, "Moose";
        goto \&Moose::import;
    }
}

sub load_classes {
    my ( $self, @classes ) = @_;

    foreach my $class ( @classes ) {
        next if $class->can("meta");
        ( my $file = "${class}.pm" ) =~ s{::}{/}g;
        require $file;
    }
}

sub sym ($$) {
    my ( $sym, $type ) = @_;
    bless \$sym, __PACKAGE__ . "::mangled::$type";
}

sub subref ($) {
    sym($_[0], "subref");
}

sub end_of_file_execution {
    my ( $self, $class, $file, @args ) = @_;
    
    if ( $ENV{MX_COMPILE_IMPLICIT_ANCESTORS} ) {
        $self->compile_ancestors( $class );
    }

    $self->compile_class( $class, $file, @args );
}

sub compile_class {
    my ( $self, $class, $file, @args ) = @_;

    unless ( defined $file ) {
        ( my $class_file = "$class.pm" ) =~ s{::}{/}g;
        $file = $INC{$class_file};
    }

    $self->store_meta( $class, $file, @args );

    $self->write_pmc_file( $class, $file, @args );
}

sub compile_ancestors {
    my ( $self, $class, %files ) = @_;

    foreach my $superclass ( reverse $class->meta->linearized_isa ) {
        next if $superclass eq $class;
        $self->compile_class( $superclass, $files{$superclass} );
    }
}

sub store_meta {
    my ( $self, $class, $file, @args ) = @_;

    require Clone;
    require Data::Visitor::Callback;

    # refer to global items symbolically
    my $meta = Data::Visitor::Callback->new(
        ignore_return_values => 1,
        "Moose::Meta::Class"     => "visit_ref",
        "Moose::Meta::Method"    => "visit_ref",
        "Moose::Meta::Attribute" => "visit_ref",
        "Moose::Meta::TypeConstraint" => sub {
            my ( $self, $constraint ) = @_;

            if ( defined ( my $name = $constraint->name ) ) {
                $_ = sym $name, "constraint";
            } else {
                warn "FOOO";
            }
        },
        "Moose::Meta::Method::Accessor" => sub {
            my ( $self, $method ) = @_;

            $self->visit( $method->body );
        },
        code   => sub {
            my ( $self, $code ) = @_;

            require Devel::Sub::Which;
            if ( my $subname = Devel::Sub::Which::which($code) ) {
                $subname =~ s/^Moose::Meta::Method::Accessor/$class/;
                if ( $subname =~ /^Moose::([^:]+)$/ ) {
                    my $method = $1;

                    if ( $method eq 'meta' ) {
                        $_ = subref "${class}::meta";
                    } else {
                        die "subname: $subname";
                    }
                } elsif ( $subname !~ /__ANON__$/ ) {
                    $_ = subref $subname;
                }
            }
        },
    )->visit( Clone::clone($class->meta) );

    require Storable;
    Storable::nstore( $meta, $self->cached_meta_file_for_class( $class, $file ) );

    #require YAML;
    #warn YAML::Dump($meta);
}

sub get_generated_methods {
    my ( $self, $class ) = @_;

    grep { $_->isa("Class::MOP::Method::Generated") } values %{ $class->meta->get_method_map };
}

sub compile_methods {
    my ( $self, $class ) = @_;

    my @methods = $self->get_generated_methods( $class );

    join("\n\n", ( map { sprintf "*%s = %s;", $_->name => $self->compile_method($class, $_) } @methods ) );
}

sub compile_method {
    my ( $self, $class, $method ) = @_;

    require B::Deparse;
    require PadWalker;

    my $d = B::Deparse->new;

    use Data::Dumper;


    my $body = $method->body;

    my $body_str = $d->coderef2text($body);

    warn "Compiling ${class}::" . $method->name;

    my $closure_vars = PadWalker::closed_over($body);

    my @env;

    if ( my $constraints = delete $closure_vars->{'@type_constraints'} ) {
        my @constraint_code = map {
            my $name = $_->name;

            defined $name
                ? "Moose::Util::TypeConstraints::find_type_constraint('$name')"
                : "die 'missing constraint'"
        } @$constraints;
        
        push @env, "require Moose::Util::TypeConstraints", join("\n    ", 'my @type_constraints = (', map { "$_," } @constraint_code ) . "\n)",
    }
    
    push @env, map {
        my $ref = $closure_vars->{$_};

        my $scalar = ref($ref) eq 'SCALAR' || ref($ref) eq 'REF';

        "my $_ = " . ( $scalar
            ? $self->_value_to_perl($$ref)
            : "(" . join(", ", map { $self->_value_to_perl($_) } @$ref ) . ")" )
    } keys %$closure_vars;

    if ( @env ) {
        my $env = join(";\n\n", @env);
        $env =~ s/^/    /gm;
        return "do {\n$env;\n\nsub $body_str\n}";
    } else {
        return "sub $body_str";
    }
}

sub _value_to_perl {
    my ( $self, $value ) = @_;

    require Data::Dump;
    require B::Deparse;

    ( (ref($value)||'') eq 'CODE'
        ? "sub " . B::Deparse->new->coderef2text($value)
        : Data::Dump::dump($value) ) 
}

sub write_pmc_file {
    my ( $self, $class, $file, @args ) = @_;

    open my $pm_fh, "<", $file or die "open($file): $!";
    open my $pmc_fh, ">", "${file}c" or die "Can't write .pmc, open(${file}c): $!";

    local $/;

    my $pm = <$pm_fh>;

    $pm =~ s/(?=__PACKAGE__->meta->make_immutable)/#/gm; # FIXME hack hack hack

    print $pmc_fh "$1\n\n" if $pm =~ /^(\#\!.*)/; # copy shebang

    require Data::Dump;
    my $ISA = Data::Dump::dump(do { no strict 'refs'; @{"${class}::ISA"} });

    my $methods = $self->compile_methods( $class, $file );
    
    print $pmc_fh <<PREAMBLE;
# Register this file as a PMC so MooseX::Compile can hax0r it
BEGIN {
    require MooseX::Compile;
    \$MooseX::Compile::known_pmc_files{+__FILE__} = 1;
}

# without use Moose we need to enable these manually
use strict;
use warnings;

# stub the sugar
{
    package $class;

    sub extends { }
    sub has { }

    sub meta { MooseX::Compile->load_cached_meta(__PACKAGE__, __FILE__) }
}

# try to approximate the time that Moose generated code enters the class
# this presumes you didn't stick the moose sugar in a BEGIN { } block
my \$__mx_compile_run_at_end = bless [ sub {
    package $class;

    our \@ISA = $ISA;
    MooseX::Compile->load_classes(\@ISA);

$methods

} ], "MooseX::Compile::Scope::Guard";

# line 1
PREAMBLE

    print $pmc_fh $pm;
}

sub load_moose {
    require Class::MOP;
    require Moose;
    require Moose::Util::TypeConstraints;
}

sub load_pmc {

}

sub load_cached_meta {
    my ( $self, $class, $file ) = @_;

    my $meta = $self->inflate_cached_meta(
        $self->load_raw_cached_meta($class, $file),
        $class, $file
    );

    {
        no strict 'refs';
        no warnings 'redefine';
        *{ "${class}::meta" } = sub { $meta };
    }

    Class::MOP::store_metaclass_by_name($class, $meta);
}

sub inflate_cached_meta {
    my ( $self, $meta ) = @_;

    require Class::Autouse;

    Class::Autouse->autouse('Moose::Meta::Class');
    Class::Autouse->autouse('Moose::Meta::Instance');

    require Data::Visitor;

    Data::Visitor::Callback->new(
        ignore_return_values => 1,
        object => sub {
            my ( $self, $obj ) = @_;
            #MooseX::Compile::load_classes(ref $obj);
            Class::Autouse->autouse(ref $obj);
        },
        "Moose::Meta::Class"     => "visit_ref",
        "Moose::Meta::Method"    => "visit_ref",
        "Moose::Meta::Attribute" => "visit_ref",
        "MooseX::Compile::mangled::constraint" => sub {
            my ( $self, $sym ) = @_;
            require Moose::Util::TypeConstraints;
            $_ = Moose::Util::TypeConstraints::find_type_constraint($$sym);
        },
        "MooseX::Compile::mangled::subref" => sub {
            my ( $self, $sym ) = @_;
            no strict 'refs';
            $_ = \&{ $$sym };
        },
    )->visit( $meta );

    $meta;
}

sub load_raw_cached_meta {
    my ( $self, $target, $file ) = @_;
    
    my $meta_file = $self->cached_meta_file_for_class($target, $file);

    require Storable;
    Storable::retrieve($meta_file);
}

sub cached_meta_file_for_class {
    my ( $self, $target, $file ) = @_;

    ( my $class_file = "$target.pm" ) =~ s{::}{/}g;

    if ( $file eq ($INC{$class_file} || '') ) {
        ( my $meta_file = $file ) =~ s/\.pm$/.mopc/;
        #return file($meta_file);
        return $meta_file;
    } else { # try to support multiple classes per file
        die "filename != class name ($file, $target, $INC{$class_file}";
        ( my $meta_dir = $file ) =~ s/\.pm$//;
        ( my $mangled_class = $target ) =~ s{::}{_}g;
        # mkpath
        require Path::Class;
        return Path::Class::dir($meta_dir)->file( "$mangled_class.mopc" );
    }
}

sub mockattr::type_constraint { bless {}, "mockconstraint" }

sub mockconstraint::_compiled_type_constraint { sub { 1 } }

sub MooseX::Compile::Scope::Guard::DESTROY {
    my $self = shift;
    $self->[0]->();
}

__PACKAGE__;

__END__

=pod

=encoding utf8

=head1 NAME

MooseX::Compile - Moose ♥ .pmc

=head1 SYNOPSIS

    use Moose;

	use MooseX::Compile; # defaults to maximal caching

    # your moose class normally here

=head1 DESCRIPTION

L<MooseX::Compile> will attempt to use Perl's C<.pmc> mechanism to cache some
of the meta computations performed by L<Moose> and L<Class::MOP> in order to
cut load times.

=cut


