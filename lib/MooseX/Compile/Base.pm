#!/usr/bin/perl

package MooseX::Compile::Base;

use strict;
use warnings;

our $VERSION = "0.01";

use constant DEBUG => our $DEBUG || $ENV{MX_COMPILE_DEBUG};

sub check_version {
    my ( $self, %args ) = @_;

    my ( $class, $version, $file ) = @args{qw(class version file)};

    if ( $self->is_version_compatible($version) ) {
        my ( $basename ) = ( $class =~ /([^:]+)$/ );
        require Carp;
        Carp::croak( "class '$class' was compiled by an incompatible version of MooseX::Compile ($version, this is $VERSION). "
                   . "Please remove the files '$basename.pmc' and '$basename.mopc' next to '$file' and recompile the class" );
    }
}

sub is_version_compatible {
    my ( $self, $compiled_version, $runtime_version ) = @_;

    $runtime_version = $VERSION if not defined $runtime_version;

    $compiled_version gt $runtime_version or ( $compiled_version lt ( $runtime_version - 1 ) );
}

sub load_classes {
    my ( $self, @classes ) = @_;

    foreach my $class ( @classes ) {
        next if $class->can("meta");
        ( my $file = "$class.pm" ) =~ s{::}{/}g;
        require $file;
    }
}

sub cached_meta_file_for_class {
    my ( $self, %args ) = @_;
    my ( $target, $file ) = @args{qw(class file)};

    ( my $class_file = "$target.pm" ) =~ s{::}{/}g;

    if ( $file eq ($INC{$class_file} || '') ) {
        ( my $meta_file = $file ) =~ s/\.pm$/.mopc/;
        #return file($meta_file);
        return $meta_file;
    } else { # try to support multiple classes per file
        die "filename != class name ($file, $target, $INC{$class_file})";
        ( my $meta_dir = $file ) =~ s/\.pm$//;
        ( my $mangled_class = $target ) =~ s{::}{_}g;
        # mkpath
        require Path::Class;
        return Path::Class::dir($meta_dir)->file( "$mangled_class.mopc" );
    }
}

{
    package MooseX::Compile::Scope::Guard;

    sub DESTROY {
        my $self = shift;
        $self->[0]->();
    }

    package MooseX::Compile::mangled::immutable_metaclass;

    package MooseX::Compile::mangled::constraint;

    package MooseX::Compile::mangled::subref;

    package MooseX::Compile::MetaBlackHole;
    # FIXME use Class::MOP::Immutable's opts?

    sub DESTROY {}

    sub make_immutable {}

    sub name {
        my $self = shift;
        $self->{name};
    }

    sub superclasses {
        my $self = shift;
        die "Can't set superclasses in class body for compiled classes, use __mx_compile_post_hook" if @_;
        no strict 'refs';
        @{ $self->name . '::ISA' };
    }

    sub AUTOLOAD {
        my $self = shift;
        require Carp;
        Carp::carp(sprintf "ignoring meta method %s till pmc finishes loading", our $AUTOLOAD);
        return;
    }
}

__PACKAGE__

__END__
