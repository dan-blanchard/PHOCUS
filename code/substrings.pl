#!/usr/bin/perl

# Corpus Substring Analyzer
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
use Getopt::Std;

our ($opt_s,$opt_p,$opt_t);
my $usage = "\nCorpus Substring Analyzer:\n" .
 			"Outputs number of times each word in corpus is substring of another.\n\n" .
			"Usage: ./substrings.pl [OPTIONS] CORPUS\n\n" . 
			"Options:\n" . 
			"\t-s\t\tSuffix mode.  Only count suffixes.\n" .
			"\t-p\t\tPrefix mode.  Only count prefixes.\n" .
			"\t-t\t\tToken mode.  Returns token frequencies instead of type.\n";
my $line;
my %substringCounts;

# Handle arguments
getopts('spt');
die $usage if (@ARGV < 1);

open WORDLIST, "tr ' ' '\\n' < $ARGV[0] | sort " . (($opt_t) ? "" : "-u") . " | awk '{ print length(), \$0 | \"sort -n\" }' | gsed -r 's/^[0-9]+ //g' |";
while ($line = <WORDLIST>)
{
	chomp $line;
	foreach my $prevLine (keys %substringCounts)
	{
		if (($prevLine ne $line) && 
		    (($opt_p && ($line =~ m/^$prevLine/)) || 
		     ($opt_s && ($line =~ m/$prevLine$/)) || 
		     (!$opt_p && !$opt_s && ($line =~ m/$prevLine/))))
		{
			$substringCounts{$prevLine}++;			
		}			
	}
	
	if (!(exists $substringCounts{$line}))
	{
		$substringCounts{$line} = 0;
	}
}

foreach my $prevLine (sort {$substringCounts{$a} <=> $substringCounts{$b}} (keys %substringCounts))
{
	print "$substringCounts{$prevLine}\t$prevLine\n";
}
