#!/usr/bin/env perl

# Corpus Visualizer
# Copyright (C) 2007-2010 Dan Blanchard.
# 
# This file is part of PHOCUS.
# 
# PHOCUS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#     
# PHOCUS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#     
# You should have received a copy of the GNU General Public License
# along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use Cairo;
use Math::Round qw(nearest_ceil);

my $scale = 4;
my $corpusLength = 9790;
my $width = 100;
my $margin = $scale * 10;
my $padding = 1;
my $padded_width = ($scale * $width) + ($padding * $width) + (2 * $margin);
my $height = $corpusLength / $width;
my $padded_height = ($scale * $height) + ($padding * $height) + (2 * $margin);
my $surface = Cairo::ImageSurface->create('argb32', $padded_width, $padded_height);
my $cr = Cairo::Context->create($surface);
my $hash_marks = 1;
my $show_scale = 1;

my $min_r = 0;
my $min_g = 0;
my $min_b = 1;
my $max_r = 1;
my $max_g = 0;
my $max_b = 0;

my $line;
my $i = 0;
my $x, my $y, my $raw_x, my $raw_y;
while ($line = <>)
{
	chomp $line;
	$raw_x = int($i % $width);
	$raw_y = int($i  / $width); 
	$x = $raw_x * $scale + $margin;
	$y = $raw_y * $scale + $margin;
	$cr->rectangle(($raw_x == 0) ? $x : $x + $padding * $raw_x, ($raw_y == 0) ? $y : $y + $padding * $raw_y, $scale, $scale);
	# print "($x, $y) = $line\n";
	$cr->set_source_rgb((($max_r - $min_r) * $line) + $min_r, (($max_g - $min_g) * $line) + $min_g, (($max_b - $min_b) * $line) + $min_b);
	$cr->fill;
	
	if ($hash_marks && ($raw_x == 0) && ($raw_y % 10 == 0))
	{
		my $y_scale = $raw_y / $height;
		# print "y_scale = $y_scale\n";
		$cr->rectangle(int($margin / 3), ($raw_y == 0) ? $y : $y + $padding * $raw_y, int($margin / 3), $scale);
		if ($show_scale)
		{
			$cr->set_source_rgb((($max_r - $min_r) * $y_scale) + $min_r, (($max_g - $min_g) * $y_scale) + $min_g, (($max_b - $min_b) * $y_scale) + $min_b);
		}
		else
		{
			$cr->set_source_rgb(0, 0, 0);
		}
		$cr->fill;
	}

	$i++;
}

if ($hash_marks)
{
	$raw_y = nearest_ceil(10, $raw_y);
	$y = $raw_y * $scale + $margin;
	my $y_scale = $raw_y / $height;
	# print "y_scale = $y_scale\n";
	$cr->rectangle(int($margin / 3), $y + $padding * $raw_y, int($margin / 3), $scale);
	if($show_scale)
	{
		$cr->set_source_rgb((($max_r - $min_r) * $y_scale) + $min_r, (($max_g - $min_g) * $y_scale) + $min_g, (($max_b - $min_b) * $y_scale) + $min_b);
	}
	else
	{
		$cr->set_source_rgb(0, 0, 0);
	}
	$cr->fill;
}

$surface->write_to_png('output.png');