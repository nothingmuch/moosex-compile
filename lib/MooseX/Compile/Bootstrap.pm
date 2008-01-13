#!/usr/bin/perl

package MooseX::Compile::Bootstrap;
use base qw(MooseX::Compile::Base);

use strict;
use warnings;

use constant DEBUG => MooseX::Compile::Base::DEBUG();

our %known_pmc_files;

sub import {
    my ( $self, %args ) = @_;

    $self->check_version(%args);

    $known_pmc_files{$args{file}} = 1;
}

sub load_cached_meta {
    my ( $self, %args ) = @_;
    my ( $class, $file ) = @args{qw(class file)};

    my $t = times;

    warn "loading metaclass for class '$class' from cached file\n" if DEBUG;

    my $meta = $self->inflate_cached_meta(
        $self->load_raw_cached_meta(%args),
        %args,
    );

    {
        no strict 'refs';
        no warnings 'redefine';
        *{ "${class}::meta" } = sub { $meta };
    }

    warn "registering loaded metaclass '$meta' for class '$class', loaded in " . (times - $t) . "s\n" if DEBUG;

    Class::MOP::store_metaclass_by_name($class, $meta);
}

sub inflate_cached_meta {
    my ( $self, $meta, %args ) = @_;

    #$Class::Autouse::DEBUG = 1;
    require Class::Autouse;

    foreach my $class qw(
        Moose::Meta::Class
        Moose::Meta::Instance
        Moose::Meta::TypeConstraint
        Moose::Meta::TypeCoercion
        Moose::Meta::Attribute
    ) {
        #warn "marking $class for autouse\n" if DEBUG;
        Class::Autouse->autouse($class);
    }

    $self->create_visitor(%args)->visit($meta);
}

sub create_visitor {
    my ( $self, %args ) = @_;

    require Data::Visitor::Callback;

    Data::Visitor::Callback->new(
        object_no_class => "visit_ref",
        object_final => sub {
            my ( $self, $obj ) = @_;

            die "Invalid object loaded from cached meta: $obj"
                if ref($obj) =~ /^MooseX::Compile::/;

            unless ( ref($obj) =~ /^Class::MOP::Class::__ANON__::/x ) {
                #warn "marking " . ref($obj) . " for autouse\n" if DEBUG;
                Class::Autouse->autouse(ref($obj));
            }

            return $obj;
        },
        "MooseX::Compile::mangled::immutable_metaclass" => sub {
            my ( $self, $spec ) = @_;
            my ( $class, $options ) = @{ $spec }{qw(class options)};

            $class = $self->visit_ref($class);

            require Class::MOP::Immutable;
            my $t = Class::MOP::Immutable->new( $class, $options );
            my $new_metaclass = $t->create_immutable_metaclass;
            bless $class, $new_metaclass->name;
            
            warn "recreated immutable metaclass for " . $class->name . " as " . $new_metaclass->name . "\n" if DEBUG;

            return $class;
        },
        "MooseX::Compile::mangled::constraint" => sub {
            my ( $self, $sym ) = @_;
            warn "loading type constraint named '$sym->{name}' in cached metaclass\n" if DEBUG;
            require Moose::Util::TypeConstraints;
            Moose::Util::TypeConstraints::find_type_constraint($sym->{name});
        },
        "MooseX::Compile::mangled::subref" => sub {
            my ( $self, $sym ) = @_;
            no strict 'refs';
            if ( my $file = $sym->{file} ) {
                warn "loading file $sym->{file} for the definition of \&$sym->{name}\n" if DEBUG && !exists($INC{$sym->{file}});
                require $file;
            }
            \&{ $sym->{name} };
        },
    );
}

sub load_raw_cached_meta {
    my ( $self, %args ) = @_;
    
    my $meta_file = $self->cached_meta_file_for_class(%args);

    require Storable;
    Storable::retrieve($meta_file);
}


__PACKAGE__

__END__
