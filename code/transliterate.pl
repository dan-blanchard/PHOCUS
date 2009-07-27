#!/usr/bin/perl

# Corpus Transliterater 
# Copyright (C) 2007-2009 Dan Blanchard.
# 
# This file is part of PHOCUS.
# 
# PHOCUS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#     
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#     
# You should have received a copy of the GNU General Public License
# along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use POSIX;
use utf8;
use Encode;

my @pairs;
my @trPair;
my $line;

# Setup Unicode input and output
binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

my $usage = "\nUsage: ./transliterate.pl KEY-FILE [FILES-TO-TRANSLITERATE]\n\n";
die $usage if @ARGV < 1;
my $keyFile = shift @ARGV;


open(KEYFILE, $keyFile);
binmode KEYFILE, ":utf8";
while (<KEYFILE>)
{
	chomp;
	@trPair = split /\t/,$_;
	push @pairs, [@trPair];
}
close(KEYFILE);

while (<>)
{
	$line = $_;
	for (my $i = 0; $i < scalar(@pairs); $i++) 
	{
		$line =~ s/$pairs[$i][0]/$pairs[$i][1]/g;
	}
	print $line;
}