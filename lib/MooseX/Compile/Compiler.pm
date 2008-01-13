#!/usr/bin/perl

package MooseX::Compile::Compiler;
use base qw(MooseX::Compile::Base);

use strict;
use warnings;

use 5.010;

use Data::Dump qw(dump);
use Data::Visitor::Callback;
use Storable;
use B;
use B::Deparse;
use PadWalker;
use Class::Inspector;

our %compiled_classes;

use constant DEBUG => MooseX::Compile::Base::DEBUG();

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
            local $Data::Dumper::Deparse = 1;
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

sub extract_code_symbols {
    my ( $self, %args ) = @_;
    my ( $class, $file ) = @args{qw(class file)};

    my ( @file, @generated, @aliased, @moose, @meta, @unknown );

    my $method_map = $class->meta->get_method_map;

    my %seen;

    foreach my $name ( keys %$method_map ) {
        $seen{$name}++;

        my $method = $method_map->{$name};
        my $body = $method->body;
        my $b = B::svref_2object($body);

        my $entry = { name => $name, meta => $method, body => $body };

        given ( $entry ) {
            when ( $method->isa("Class::MOP::Method::Generated") ) {
                push @generated, $entry;
            }

            when ( $b->FILE eq $file ) {
                push @file, $entry;
            }

            when ( $name eq 'meta' and $b->STASH->NAME ~~ [qw(Moose metaclass)] ) {
                push @meta, $entry;
            }

            default {
                use Data::Dumper;
                $Data::Dumper::Deparse = 1;
                die Dumper( $entry );
                push @unknown, $entry;
            }
        }
    }

    my %symbols; @symbols{@{ Class::Inspector->functions($class) || [] }} = @{ Class::Inspector->function_refs($class) || [] };

    foreach my $name ( grep { not $seen{$_}++ } keys %symbols ) {
        my $body = $symbols{$name};
        my $b = B::svref_2object( $symbols{$name} );
        my $entry = { name => $name, body => $body };

        local $@;

        given ( $entry ) {
            when ( eval { $b->STASH->NAME } ~~ 'Moose' ) {
                push @moose, $entry;
            }

            default {
                push @unknown, $entry;
            }
        }
    }

    return (
        file => \@file,
        generated => \@generated,
        aliased => \@aliased,
        meta => \@meta,
        moose => \@moose,
        unknown => \@moose,
    );
}

sub compile_code_symbols {
    my ( $self, %args ) = @_;

    my $symbols = $args{all_symbols};

    my @ret;

    foreach my $category ( @{ $args{'symbol_categories'} } ) {
        my $method = "compile_${category}_code_symbols";
        push @ret, $self->$method( %args, symbols => delete($symbols->{$category}) );
    }

    join "\n\n", @ret;
}

sub compile_file_code_symbols {
    # this is already taken care of by the inclusion of the whole .pm after the preamble
    return;
}

sub compile_meta_code_symbols {
    # we fake this one 
    return;
}

sub compile_moose_code_symbols {
    my ( $self, %args ) = @_;
    return map {
        my $name = $_->{name};
        my $proto = prototype($_->{body});
        $proto = $proto ? "($proto)" : "";
        "sub $name $proto { }";
    } @{ $args{symbols} };
}


sub compile_generated_code_symbols {
    my ( $self, %args ) = @_;
    map { sprintf "*%s = %s;", $_->name => $self->compile_method(%args, method => $_) } map { $_->{meta} } @{ $args{symbols} };
}

sub compile_aliased_code_symbols {
    return;
}

sub compile_unknown_code_symbols {
    my ( $self, %args ) = @_;
    use Data::Dumper;
    warn "Unknown functions: " . Dumper($args{symbols});
    return;
}

sub compile_method {
    my ( $self, %args ) = @_;
    my ( $class, $method ) = @args{qw(class method)};

    my $d = B::Deparse->new;

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

    ( (ref($value)||'') eq 'CODE'
        ? $self->_subref_to_perl($value)
        : Data::Dump::dump($value) ) 
}

sub _subref_to_perl {
    my ( $self, $subref ) = @_;

    my %rev_inc = reverse %INC;

    if ( ( my $name = code_name($subref) ) !~ /__ANON__$/ ) {
        if ( -f ( my $file = B::svref_2object($subref)->FILE ) ) {
            return qq|do { require "\Q$rev_inc{$file}\E"; \\&$name }|;
        } else {
            return '\&' . $name;
        }
    } else {
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

sub pmc_preamble_comment {
    my ( $self, %args ) = @_;

    return <<COMMENT;
# This file is generated by MooseX::Compile, and contains a cached
# version of the class '$args{class}'.
COMMENT
}

sub pmc_preamble_header {
    my( $self, %args ) = @_;
    my $class = $args{class};

    my $quoted_class = dump($class);
    my $version = dump($MooseX::Compile::Base::VERSION);

    return <<HEADER;
# used in debugging output if any
my \$__mx_compile_t; BEGIN { \$__mx_compile_t = times }

# Register this file as a PMC
use MooseX::Compile::Bootstrap(
    class   => $quoted_class,
    file    => __FILE__,
    version => $version,
);


# without use Moose we need to enable these manually
use strict;
use warnings;
HEADER

}

sub pmc_preamble_setup_env {
    my ( $self, %args ) = @_;

    my $class = $args{class};

    my $quoted_class = dump($class);

    my $sugar = $self->compile_code_symbols( %args, symbol_categories => [qw(moose)] );

    return <<ENV;
# stub the sugar
BEGIN {
    package $class;

    my \$fake_meta = bless { name => $quoted_class }, "MooseX::Compile::MetaBlackHole";
    sub meta { \$fake_meta }

$sugar

    our \$__mx_is_compiled = 1;
}
ENV
}

sub pmc_preamble_at_end {
    my ( $self, $code, %args ) = @_;

    return <<HOOK
# try to approximate the time that Moose generated code enters the class
# this presumes you didn't stick the moose sugar in a BEGIN { } block
my \$__mx_compile_run_at_end = bless [ sub {

$code

} ], "MooseX::Compile::Scope::Guard";
HOOK
}

sub pmc_preamble_generated_code {
    my ( $self, %args ) = @_;

    return $self->pmc_preamble_at_end($self->pmc_preamble_generated_code_body(%args), %args);
}

sub pmc_preamble_generated_code_body {
    my ( $self, %args ) = @_;

    my $class = $args{class};

    my $quoted_class = dump($class);

    return join("\n",
        "package $class;",
        $self->pmc_preamble_define_isa(%args),
        $self->pmc_preamble_define_code_symbols(%args),
        $self->pmc_preamble_call_post_hook(%args),
        qq{warn "bootstrap of class '$class' finished in " . (times - \$__mx_compile_t) . "s\n" if MooseX::Compile::DEBUG();},
    );
}

sub pmc_preamble_define_isa {
    my ( $self, %args ) = @_;

    my $ISA = dump($args{class}->meta->superclasses);

    return <<ISA
our \@ISA = $ISA;
MooseX::Compile::Bootstrap->load_classes(\@ISA);
ISA
}

sub pmc_preamble_define_code_symbols {
    my ( $self, %args ) = @_;

    return (
        $self->compile_code_symbols(%args, symbol_categories => [qw(generated aliased)]),
        $self->pmc_preamble_faked_code_symbols(%args),
    );
}

sub pmc_preamble_faked_code_symbols {
    my ( $self, %args ) = @_;

    return <<METHODS
{
    no warnings 'redefine';
    *meta = sub { MooseX::Compile::Bootstrap->load_cached_meta( class => __PACKAGE__, file => __FILE__) };
}
METHODS
}

sub pmc_preamble_call_post_hook {
    my ( $self, %args ) = @_;
    my $class = $args{class};

    return <<HOOK
${class}::__mx_compile_post_hook()
    if defined \&${class}::__mx_compile_post_hook;
HOOK
}

sub pmc_preamble {
    my ( $self, %args ) = @_;
    my ( $class, $file ) = @args{qw(class file)};

    ( my $short_name = "$class.pm" ) =~ s{::}{/}g;

    $args{short_name} = $short_name;

    $args{all_symbols} = { $self->extract_code_symbols(%args) };

    return join("\n",
        $self->pmc_preamble_comment(%args),
        $self->pmc_preamble_header(%args),
        $self->pmc_preamble_setup_env(%args),
        $self->pmc_preamble_generated_code(%args),
        $self->pmc_preamble_footer(%args),
    )
}

sub pmc_preamble_footer {
    my ( $self, %args ) = @_;
    return <<FOOTER
BEGIN { warn "giving control back to original '$args{short_name}', bootstrap preamble took " . (times - \$__mx_compile_t) . "s\\n" if MooseX::Compile::DEBUG() }
FOOTER
}

__PACKAGE__

__END__
