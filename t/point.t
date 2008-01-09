#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
use Test::Exception;

use lib "t/lib";

use MooseX::Compile (); # clean hook

use ok "Point";
use ok "Point3D";

my $point = Point->new(x => 1, y => 2);	

isa_ok($point, 'Point');
isa_ok($point, 'Moose::Object');

is($point->x, 1, '... got the right value for x');
is($point->y, 2, '... got the right value for y');

$point->y(10);
is($point->y, 10, '... got the right (changed) value for y');

dies_ok {
	$point->y('Foo');
} '... cannot assign a non-Int to y';

dies_ok {
    $point->x(1000);
} '... cannot assign to a read-only method';
is($point->x, 1, '... got the right (un-changed) value for x');

$point->clear();

is($point->x, 0, '... got the right (cleared) value for x');
is($point->y, 0, '... got the right (cleared) value for y');

# check the type constraints on the constructor

lives_ok {
	Point->new(x => 0, y => 0);
} '... can assign a 0 to x and y';

dies_ok {
	Point->new(x => 10, y => 'Foo');
} '... cannot assign a non-Int to y';

# Point3D

my $point3d = Point3D->new({ x => 10, y => 15, z => 3 });
isa_ok($point3d, 'Point3D');
isa_ok($point3d, 'Point');
isa_ok($point3d, 'Moose::Object');

is($point3d->x, 10, '... got the right value for x');
is($point3d->y, 15, '... got the right value for y');
is($point3d->{'z'}, 3, '... got the right value for z');

dies_ok {
	$point3d->z;
} '... there is no method for z';

$point3d->clear();

is($point3d->x, 0, '... got the right (cleared) value for x');
is($point3d->y, 0, '... got the right (cleared) value for y');
is($point3d->{'z'}, 0, '... got the right (cleared) value for z');

dies_ok {
	Point3D->new(x => 10, y => 'Foo', z => 3);
} '... cannot assign a non-Int to y';

dies_ok {
	Point3D->new(x => 'Foo', y => 10, z => 3);
} '... cannot assign a non-Int to x';

dies_ok {
	Point3D->new(x => 0, y => 10, z => 'Bar');
} '... cannot assign a non-Int to z';

# test some class introspection

can_ok('Point', 'meta');
isa_ok(Point->meta, 'Moose::Meta::Class');

can_ok('Point3D', 'meta');
isa_ok(Point3D->meta, 'Moose::Meta::Class');

isnt(Point->meta, Point3D->meta, '... they are different metaclasses as well');

