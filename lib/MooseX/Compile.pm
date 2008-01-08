#!/usr/bin/perl

#BEGIN {
#    require Carp::Heavy;
#    unshift @INC, sub {
#        my ( $self, $file ) = @_;
#        warn "loading $file" . Carp::longmess if $file =~ /MOP|meta|Moose/i;
#        return;
#    }
#}

package MooseX::Compile;

use strict;
use warnings;

use Scalar::Util qw(blessed);

use Hash::Util qw(fieldhash);

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

sub sym ($$;@) {
    my ( $sym, $type, @args ) = @_;
    bless { @args, name => $sym }, __PACKAGE__ . "::mangled::$type";
}

sub code_name ($;$) {
    my ( $code, $cv ) = @_;
    require B;
    $cv ||= B::svref_2object($code);
    local $@;
    return eval { join("::", $cv->STASH->NAME, $cv->GV->NAME) };
}

sub verified_code_name ($;$) {
    my ( $code, $cv ) = @_;

    if ( my $name = code_name($code, $cv) ) {
        if ( verify_code_name($code, $name) ) {
            return $name;
        }
    }

    return;
}

sub verify_code_name ($$) {
    my ( $code, $name ) = @_;

    no strict 'refs';
    \&$name == $code;
}

sub subref ($;$) {
    my ( $code, $name ) = @_;

    if ( ref $code ) {
        require B;
        my $cv = B::svref_2object($code);
        $name ||= code_name($code, $cv);
        if ( $name && verify_code_name($code,$name) ) {
            my $file = $cv->FILE;
            my %rev_inc = reverse %INC;
            return sym( $name, "subref", ( -f $file ? ( file => $rev_inc{$file} ) : () ) );
        } else {
            warn "$code has name '$name', but it doesn't point back to the cv" if $name;
            require Data::Dumper;
            no strict 'refs';
            $Data::Dumper::Deparse = 1;
            warn Data::Dumper::Dumper({
                name => $name,
                name_strval => ("". \&$name),
                name_ref => \&$name,
                arg_ref => $code,
                arg_strval => "$code",
            });
            die "Can't make a symbolic ref to $code, it has no name or the name is invalid";
        }
    } else {
        return sym($code, "subref");
    }
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

    require Data::Visitor::Callback;

    my $meta = Data::Visitor::Callback->new(
        "object" => sub {
            my ( $self, $obj ) = @_;
            $self->visit_ref($obj) unless $obj->isa("Moose::Meta::TypeConstraint");
        },
        "Class::MOP::Class" => sub {
            my ( $self, $meta ) = @_;

            if ( $meta->is_immutable ) {
                return bless {
                    class      => $meta,
                    orig_class => $meta->{___original_class},
                    options    => $meta->immutable_transformer->options,
                }, "MooseX::Compile::mangled::immutable_metaclass";
            }
            
            if ( $meta->is_anon_class ){
                warn "Can't reliably store anonymouse metaclasses yet";
            }

            $meta;
        },
        "Moose::Meta::TypeConstraint" => sub {
            my ( $self, $constraint ) = @_;

            if ( defined ( my $name = $constraint->name ) ) {
                return sym $name, "constraint";
            } else {
                warn "FOOO";
                return $constraint;
            }
        },
        code => sub {
            my ( $self, $code ) = @_;

            if ( my $subname = code_name($code) ) {
                if ( $subname =~ /^Moose::Meta::Method::\w+::(.*)$/ ) {
                    # FIXME should this be verified more closely?
                    # sometimes the coderef $code doesn't match \&{ $class::$1 }
                    return subref "${class}::$1";
                } elsif ( $subname =~ /^(?:Moose|metaclass)::([^:]+)$/ ) {
                    my $method = $1;

                    if ( $method eq 'meta' ) {
                        return subref "${class}::meta";
                    } else {
                        die "subname: $subname";
                    }
                } elsif ( $subname !~ /__ANON__$/ ) {
                    return subref $code, $subname;
                } else {
                    warn "Unable to locate symbol for $code";
                    return $code;
                }
            }

            return $code;
        },
    )->visit( $class->meta );

    require Storable;
    Storable::nstore( $meta, $self->cached_meta_file_for_class( $class, $file ) );
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

    my $closure_vars = PadWalker::closed_over($body);

    my @env;

    if ( my $constraints = delete $closure_vars->{'@type_constraints'} ) {
        my @constraint_code = map {
            my $name = $_->name;

            defined $name
                ? "Moose::Util::TypeConstraints::find_type_constraint('$name')"
                : "die 'missing constraint'"
        } @$constraints;
        
        push @env, "require Moose::Util::TypeConstraints::OptimizedConstraints", join("\n    ", 'my @type_constraints = (', map { "$_," } @constraint_code ) . "\n)",
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

    ( (ref($value)||'') eq 'CODE'
        ? $self->_subref_to_perl($value)
        : Data::Dump::dump($value) ) 
}

sub _subref_to_perl {
    my ( $self, $subref ) = @_;

    my %rev_inc = reverse %INC;

    if ( ( my $name = code_name($subref) ) !~ /__ANON__$/ ) {
        require B;
        if ( -f ( my $file = B::svref_2object($subref)->FILE ) ) {
            return qq|do { require "\Q$rev_inc{$file}\E"; \\&$name }|;
        } else {
            return '\&' . $name;
        }
    } else {
        require B::Deparse;
        "sub " . B::Deparse->new->coderef2text($subref);
    }
}

sub write_pmc_file {
    my ( $self, $class, $file, @args ) = @_;

    open my $pm_fh, "<", $file or die "open($file): $!";
    open my $pmc_fh, ">", "${file}c" or die "Can't write .pmc, open(${file}c): $!";

    local $/;

    my $pm = <$pm_fh>;

    print $pmc_fh "$1\n\n" if $pm =~ /^(\#\!.*)/; # copy shebang

    require Data::Dump;
    my $ISA = Data::Dump::dump(do { no strict 'refs'; @{"${class}::ISA"} });

    my $methods = $self->compile_methods( $class, $file );

    $methods .= <<META;
{
    no warnings 'redefine';
    *meta = sub { MooseX::Compile->load_cached_meta(__PACKAGE__, __FILE__) };
}
META
    
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
BEGIN {
    package $class;

    sub extends { }
    sub has { }

    my \$fake_meta = bless { name => q{$class} }, "MooseX::Compile::MetaBlackHole";
    sub meta { \$fake_meta }

    our \$__mx_is_compiled = 1;
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

    #$Class::Autouse::DEBUG = 1;
    require Class::Autouse;

    Class::Autouse->autouse('Moose::Meta::Class');
    Class::Autouse->autouse('Moose::Meta::Instance');
    Class::Autouse->autouse('Moose::Meta::TypeConstraint');
    Class::Autouse->autouse('Moose::Meta::TypeCoercion');
    Class::Autouse->autouse('Moose::Meta::Attribute');

    require Data::Visitor::Callback;

    fieldhash my %ok_anons;
    fieldhash my %found_anons;

    my $thawed_meta = Data::Visitor::Callback->new(
        object => "visit_ref",
        object_final => sub {
            my ( $self, $obj ) = @_;

            die "Invalid object loaded from cached meta: $obj"
                if ref($obj) =~ /^MooseX::Compile::/;

            # can't load an __ANON__ from the store without some fixing
            if ( ref($obj) =~ /^Class::MOP::Class::__ANON__::/x ) {
                $found_anons{$obj}++;
            } else {
                Class::Autouse->autouse(ref($obj));
            }
            return $obj;
        },
        "MooseX::Compile::mangled::immutable_metaclass" => sub {
            my ( $self, $spec ) = @_;
            my ( $class, $orig_class, $options ) = @{ $spec }{qw(class orig_class options)};

            bless $class, $orig_class;

            require Class::MOP::Immutable;
            my $t = Class::MOP::Immutable->new( $class, $options );
            my $new_metaclass = $t->create_immutable_metaclass;
            bless $class, $new_metaclass->name;

            $ok_anons{$class}++;

            return $class;
        },
        "MooseX::Compile::mangled::constraint" => sub {
            my ( $self, $sym ) = @_;
            require Moose::Util::TypeConstraints;
            Moose::Util::TypeConstraints::find_type_constraint($sym->{name});
        },
        "MooseX::Compile::mangled::subref" => sub {
            my ( $self, $sym ) = @_;
            no strict 'refs';
            if ( my $file = $sym->{file} ) {
                require $file;
            }
            \&{ $sym->{name} };
        },
    )->visit( $meta );


    delete @found_anons{keys %ok_anons};
    die "Instance of anonymous class cannot be thawed: " . keys %found_anons if scalar keys %found_anons;

    return $thawed_meta;
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

{
    package MooseX::Compile::mangled::immutable_metaclass;
}

{
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
        no strict 'refs';
        @{ $self->name . '::ISA' };
    }

    sub AUTOLOAD {
        my $self = shift;
        require Carp;
        Carp::carp sprintf "ignoring meta method %s till pmc finishes loading", our $AUTOLOAD;
        return;
    }
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


