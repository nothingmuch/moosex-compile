#!/usr/bin/perl

package MooseX::Compile;

use strict;
use warnings;

use constant DEBUG => $ENV{MX_COMPILE_DEBUG};

our $VERSION = "0.01";

#BEGIN {
#    require Carp::Heavy;
#    unshift @INC, sub {
#        my ( $self, $file ) = @_;
#        warn "loading $file" . Carp::longmess if $file =~ /MOP|meta|Moose/i;
#        return;
#    }
#}

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
                require Carp and Carp::carp "loading $pkg" if $file eq $pkg_file;
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


use Scalar::Util qw(blessed);

if ( $ENV{MX_COMPILE_PRELOAD_MOOSE} ) {
    __PACKAGE__->load_moose();
}

our %known_pmc_files;
our %compiled_classes;

sub import {
    my ($self, @args) = @_;

    my ( $class, $file ) = caller();

    if ( $known_pmc_files{$file} ) {
        return $self->load_pmc( $class, $file, @args );
    } else {
        warn "class '$class' requires compilation\n" if DEBUG;
        require Check::UnitCheck;
        Check::UnitCheck::unitcheckify(sub { $self->end_of_file_execution( $class, $file, @args ) });

        require Moose;
        shift; unshift @_, "Moose";
        goto \&Moose::import;
    }
}

sub check_version {
    my ( $self, %args ) = @_;

    my ( $class, $version, $file ) = @args{qw(class version file)};

    if ( $version gt $VERSION or ( $version lt $VERSION - 1 ) ) {
        require Carp;
        my ( $basename ) = ( $class =~ /([^:]+)$/ );
        Carp::croak "class '$class' was compiled by an incompatible version of MooseX::Compile ($version, this is $VERSION). "
                  . "Please remove the files '$basename.pmc' and '$basename.mopc' next to '$file' and recompile the class";
    }
}

sub load_classes {
    my ( $self, @classes ) = @_;

    foreach my $class ( @classes ) {
        next if $class->can("meta");
        ( my $file = "$class.pm" ) =~ s{::}{/}g;
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
 
    warn "compilation unit of class '$class' finished, compiling\n" if DEBUG;

    if ( $ENV{MX_COMPILE_IMPLICIT_ANCESTORS} ) {
        warn "implicitly compiling all ancestors of class '$class'\n" if DEBUG;
        $self->compile_ancestors( $class );
    }

    $self->compile_class( $class, $file, @args );
}

sub compile_class {
    my ( $self, $class, $file, @args ) = @_;

    if ( $compiled_classes{$class}++ ) {
        warn "already compiled class '$class'\n" if DEBUG;
        return;
    }

    my $t = times;

    unless ( defined $file ) {
        ( my $class_file = "$class.pm" ) =~ s{::}{/}g;
        $file = $INC{$class_file};
    }

    $self->store_meta( $class, $file, @args );

    $self->write_pmc_file( $class, $file, @args );

    warn "compilation of .pmc and .mopc for class '$class' took " . ( times - $t ) . "s\n" if DEBUG;
}

sub compile_ancestors {
    my ( $self, $class, %files ) = @_;

    foreach my $superclass ( reverse $class->meta->linearized_isa ) {
        next if $superclass eq $class;
        warn "compiling '$class' superclass '$superclass'\n" if DEBUG;
        $self->compile_class( $superclass, $files{$superclass} );
    }
}

sub store_meta {
    my ( $self, $class, $file, @args ) = @_;

    require Data::Visitor::Callback;

    my $meta = Data::Visitor::Callback->new(
        "object" => sub {
            my ( $self, $obj ) = @_;

            return $obj if $obj->isa("Moose::Meta::TypeConstraint");

            $self->visit_ref($obj);
        },
        object_final => sub {
            my ( $self, $obj ) = @_;

            if ( ref($obj) =~ /^Class::MOP::Class::__ANON__::/x ) {
                die "Instance of anonymous class cannot be thawed: $obj";
            }

            return $obj;
        },
        "Class::MOP::Class" => sub {
            my ( $self, $meta ) = @_;

            if ( $meta->is_immutable ) {
                my $options = $meta->immutable_transformer->options;
                bless( $meta, $meta->{___original_class} ), # it's a copy, we can rebless
                return bless {
                    class      => $meta,
                    options    => $options,
                }, "MooseX::Compile::mangled::immutable_metaclass";
            }
            
            if ( $meta->is_anon_class ){
                warn "Can't reliably store anonymouse metaclasses yet";
            }

            return $meta;
        },
        "Moose::Meta::TypeConstraint" => sub {
            my ( $self, $constraint ) = @_;

            if ( defined ( my $name = $constraint->name ) ) {
                return sym $name, "constraint";
            } else {
                warn "Anonymous constraint $constraint left in metaclass";
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

    local $@;
    eval { Storable::nstore( $meta, $self->cached_meta_file_for_class( $class, $file ) ) };

    if ( $@ ) {
        require YAML;
        $YAML::UseCode = 1;
        die join("\n", $@, YAML::Dump($meta) );
    }
    
    if ( DEBUG ) {
        ( my $short_name = "${class}.mopc" ) =~ s{::}{/}g;
        warn "stored $meta for $short_name\n";
    }

    return 1;
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

    ( my $short_name = "$class.pm" ) =~ s{::}{/}g;

    $methods .= <<META;
{
    no warnings 'redefine';
    *meta = sub { MooseX::Compile->load_cached_meta(__PACKAGE__, __FILE__) };
}
META
    
    print $pmc_fh <<PREAMBLE;
my \$__mx_compile_t; BEGIN { \$__mx_compile_t = times }
# Register this file as a PMC so MooseX::Compile can hax0r it
BEGIN {
    require MooseX::Compile;
    MooseX::Compile->check_version(class => "$class", file => __FILE__, version => "$MooseX::Compile::VERSION");
    warn "found .pmc file for '$short_name'\\n" if MooseX::Compile::DEBUG();
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

    ${class}::__mx_compile_post_hook()
        if defined \&${class}::__mx_compile_post_hook;

    warn "bootstrap of class '$class' finished in " . (times - \$__mx_compile_t) . "s\\n" if MooseX::Compile::DEBUG();
} ], "MooseX::Compile::Scope::Guard";

BEGIN { warn "giving control back to original '$short_name', bootstrap preamble took " . (times - \$__mx_compile_t) . "s\\n" if MooseX::Compile::DEBUG() }

# line 1
PREAMBLE

    print $pmc_fh $pm;

    close $pmc_fh;

    warn "wrote PMC file '${short_name}c'\n" if DEBUG;
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

    my $t = times;

    warn "loading metaclass for class '$class' from cached file\n" if DEBUG;

    my $meta = $self->inflate_cached_meta(
        $self->load_raw_cached_meta($class, $file),
        $class, $file
    );

    {
        no strict 'refs';
        no warnings 'redefine';
        *{ "${class}::meta" } = sub { $meta };
    }

    warn "registering loaded metaclass '$meta' for class '$class', loaded in " . (times - $t) . "s\n";

    Class::MOP::store_metaclass_by_name($class, $meta);
}

sub inflate_cached_meta {
    my ( $self, $meta ) = @_;

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

    require Data::Visitor::Callback;

    my $thawed_meta = Data::Visitor::Callback->new(
        object => "visit_ref",
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
    )->visit( $meta );

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
        die "Can't set superclasses in class body for compiled classes, use __mx_compile_post_hook" if @_;
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


