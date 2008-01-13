#!/usr/bin/perl

package MooseX::Compile;
use base qw(MooseX::Compile::Base);

use strict;
use warnings;

use constant DEBUG => MooseX::Compile::Base::DEBUG();
use constant default_compiler_class => "MooseX::Compile::Compiler";

BEGIN {
    unshift @INC, sub {
        my ( $self, $file ) = @_;

        if ( DEBUG ) {
            foreach my $pkg qw(
                Moose
                Moose::Meta::Class

                Class::MOP
                Class::MOP::Class

                metaclass

                Moose::Util::TypeConstraints
                Moose::Meta::TypeConstraint
                Moose::Meta::TypeCoercion
            ) {
                ( my $pkg_file = "$pkg.pm" ) =~ s{::}{/}g;
                require Carp and Carp::carp("loading $pkg") if $file eq $pkg_file;
            }
        }

        if ( $ENV{MX_COMPILE_CLEAN} ) {
            foreach my $dir ( grep { not ref } @INC ) {
                my $full = "$dir/$file";

                my $pmc = "${full}c";
                ( my $mopc = $full ) =~ s/\.pm$/.mopc/;

                if ( -e $pmc && -e $mopc ) {
                    warn "removing compiled class for file '$file'\n" if DEBUG;
                    unlink $pmc or die "Can't remove pmc file (unlink($pmc)): $!";
                    unlink $mopc or die "Can't remove cached metaclass (unlink($mopc)): $!";
                }
            }
        }

        return;
    }
}

sub import {
    my ($self, @args) = @_;

    my ( $class, $file ) = caller();

    if ( $MooseX::Compile::Bootstrap::known_pmc_files{$file} ) {
        return $self->import_from_pmc( $class, $file, @args );
    } else {
        warn "class '$class' requires compilation\n" if DEBUG;

        require Check::UnitCheck;
        Check::UnitCheck::unitcheckify(sub {
            warn "compilation unit of class '$class' calling UNITCHECK\n" if DEBUG;
            $self->unit_check( $class, $file, @args );
        });

        require Moose;
        shift; unshift @_, "Moose";
        goto \&Moose::import;
    }
}

sub import_from_pmc {

}

sub unit_check {
    my ( $self, $class, $file, @args ) = @_;

    $self->compile_from_import(
        class => $class,
        file  => $file,
        @args,
    );
}

sub compile_from_import {
    my ( $self, %args ) = @_;

    if ( $ENV{MX_COMPILE_IMPLICIT_ANCESTORS} ) {
        warn "implicitly compiling all ancestors of class '$args{class}'\n" if DEBUG;
        $self->compile_ancestors( %args );
    }

    $self->compile_class( %args );
}

sub compile_ancestors {
    my ( $self, %args ) = @_;

    my $class = $args{class};
    my $files = $args{files} || {};

    foreach my $superclass ( reverse $class->meta->linearized_isa ) {
        next if $superclass eq $class;
        warn "compiling '$class' superclass '$superclass'\n" if DEBUG;
        $self->compile_class( %args, class => $superclass, file => $files->{$superclass} );
    }
}

sub compile_class {
    my ( $self, %args ) = @_;

    my $compiler = $self->create_compiler( %args );

    $compiler->compile_class( %args );
}

sub compiler_class {
    my ( $self, %args ) = @_;

    $args{compiler_class} || $self->default_compiler_class;
}

sub create_compiler {
    my ( $self, @args ) = @_;

    my $compiler_class = $self->compiler_class(@args);

    $self->load_classes($compiler_class);

    $compiler_class->new( @args );
}

__PACKAGE__;

__END__

=pod

=encoding utf8

=head1 NAME

MooseX::Compile - Moose ♥ .pmc

=head1 SYNOPSIS

    use Moose;

	use MooseX::Compile;

    # your moose class normally here

    # on the second load of your module the metaclass instance and all
    # generated code will be loaded from a cached copy

=head1 DESCRIPTION

L<MooseX::Compile> will attempt to use Perl's C<.pmc> mechanism to cache some
of the meta computations performed by L<Moose> and L<Class::MOP> in order to
cut load times.

=cut


