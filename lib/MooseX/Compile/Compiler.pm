#!/usr/bin/perl

package MooseX::Compile::Compiler;
use base qw(MooseX::Compile::Base);

use strict;
use warnings;

use Data::Visitor::Callback;
use Storable;

our %compiled_classes;

sub DEBUG () { 0 }

sub new {
    my ( $class, %args ) = @_;
    bless \%args, $class;
}

sub compile_class {
    my ( $self, %args ) = @_;
    my $class = $args{class};

    unless ( defined $args{file} ) {
        ( my $class_file = "$class.pm" ) =~ s{::}{/}g;
        $args{file} = $INC{$class_file};
    }

    if ( $compiled_classes{$class}++ ) {
        warn "already compiled class '$class'\n" if DEBUG;
        return;
    }

    my $t = times;

    $self->cache_meta(%args);
    $self->write_pmc_file(%args);

    warn "compilation of .pmc and .mopc for class '$class' took " . ( times - $t ) . "s\n" if DEBUG;
}


sub sym ($$;@) {
    my ( $sym, $type, @args ) = @_;
    bless { @args, name => $sym }, "MooseX::Compile::mangled::$type";
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

sub create_visitor {
    my ( $self, %args ) = @_;
    my $class = $args{class};

    Data::Visitor::Callback->new(
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
    );
}

sub deflate_meta {
    my ( $self, $meta, %args ) = @_;
    
    my $visitor = $self->create_visitor(%args);
    
    $visitor->visit($meta);
}

sub cache_meta {
    my ( $self, %args ) = @_;
    my $class = $args{class};

    my $meta = $self->deflate_meta( $class->meta, %args );
    $self->store_meta( $meta, %args );
}

sub store_meta {
    my ( $self, $meta, %args ) = @_;

    local $@;
    eval { Storable::nstore( $meta, $self->cached_meta_file_for_class(%args) ) };

    if ( $@ ) {
        require YAML;
        no warnings 'once';
        $YAML::UseCode = 1;
        die join("\n", $@, YAML::Dump($meta) );
    }
    
    if ( DEBUG ) {
        my $class = $args{class};
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
    my ( $self, %args ) = @_;
    my $class = $args{class};

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

    ( my $short_name = "$class.pm" ) =~ s{::}{/}g;

    open my $pm_fh, "<", $file or die "open($file): $!";
    open my $pmc_fh, ">", "${file}c" or die "Can't write .pmc, open(${file}c): $!";

    local $/;

    my $pm = <$pm_fh>;

    close $pm_fh;

    print $pmc_fh "$1\n\n" if $pm =~ /^(\#\!.*)/; # copy shebang

    print $pmc_fh $self->pmc_preamble( $class, $file, @args );

    print $pmc_fh "\n# line 1\n";

    print $pmc_fh $pm;

    close $pmc_fh or die "Can't write .pmc, close(${file}c): $!";

    warn "wrote PMC file '${short_name}c'\n" if DEBUG;
}

sub pmc_preamble {
    my ( $self, %args ) = @_;
    my ( $class, $file ) = @args{qw(class file)};

    require Data::Dump;
    my $ISA = Data::Dump::dump($class->meta->superclasses);

    my $methods = $self->compile_methods(%args);

    ( my $short_name = "$class.pm" ) =~ s{::}{/}g;

    $methods .= <<META;
{
    no warnings 'redefine';
    *meta = sub { MooseX::Compile::Bootstrap->load_cached_meta( class => __PACKAGE__, file => __FILE__) };
}
META
    
    return <<PREAMBLE;
my \$__mx_compile_t; BEGIN { \$__mx_compile_t = times }
# Register this file as a PMC so MooseX::Compile can hax0r it

use MooseX::Compile::Bootstrap(
    class   => "$class",
    file    => __FILE__,
    version => "$MooseX::Compile::Base::VERSION",
);

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
    MooseX::Compile::Bootstrap->load_classes(\@ISA);

$methods

    ${class}::__mx_compile_post_hook()
        if defined \&${class}::__mx_compile_post_hook;

    warn "bootstrap of class '$class' finished in " . (times - \$__mx_compile_t) . "s\\n" if MooseX::Compile::DEBUG();
} ], "MooseX::Compile::Scope::Guard";

BEGIN { warn "giving control back to original '$short_name', bootstrap preamble took " . (times - \$__mx_compile_t) . "s\\n" if MooseX::Compile::DEBUG() }
PREAMBLE
}

__PACKAGE__

__END__
