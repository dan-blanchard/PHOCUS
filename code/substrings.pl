#!/usr/bin/perl

# Dan Blanchard
# Calculates how many words in corpus are prefixes of other words.

use strict;
use Getopt::Std;

our ($opt_s,$opt_p);
my $usage = "\nCorpus Substring Analyzer:\n" .
 			"Outputs number of times each word in corpus is substring of another.\n\n" .
			"Usage: ./substrings.pl [OPTIONS] CORPUS\n\n" . 
			"Options:\n" . 
			"\t-s\t\tSuffix mode.  Only count suffixes.\n" .
			"\t-p\t\tPrefix mode.  Only count prefixes.\n" .
my $line;
my %substringCounts;

# Handle arguments
getopts('sp');
die $usage if (@ARGV < 1);

open WORDLIST, "tr ' ' '\\n' < $ARGV[0] | sort -u | awk '{ print length(), \$0 | \"sort -n\" }' | gsed -r 's/^[0-9]+ //g' |";
while ($line = <WORDLIST>)
{
	chomp $line;
	foreach my $prevLine (keys %substringCounts)
	{
		if (($opt_p && ($line =~ m/^$prevLine/)) || ($opt_s && ($line =~ m/$prevLine$/)) || (!$opt_p && !$opt_s && ($line =~ m/$prevLine/)))
		{
			$substringCounts{$prevLine}++;			
		}
	}
	
	$substringCounts{$line} = 0;
}

foreach my $prevLine (sort {$substringCounts{$a} <=> $substringCounts{$b}} (keys %substringCounts))
{
	print "$substringCounts{$prevLine}\t$prevLine\n";
}
