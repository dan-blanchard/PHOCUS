#!/usr/bin/perl

# Dan Blanchard
# Calculates how many words in corpus are prefixes of other words.

use strict;

my $line;
my %prefixCounts;

open WORDLIST, "tr ' ' '\\n' < $ARGV[0] | sort -u | awk '{ print length(), \$0 | \"sort -n\" }' | gsed -r 's/^[0-9]+ //g' |";
while ($line = <WORDLIST>)
{
	chomp $line;
	foreach my $prevLine (keys %prefixCounts)
	{
		if ($line =~ m/^$prevLine/)
		{
			$prefixCounts{$prevLine}++;			
		}
	}
	
	$prefixCounts{$line} = 0;
}

foreach my $prevLine (sort {$prefixCounts{$a} <=> $prefixCounts{$b}} (keys %prefixCounts))
{
	print "$prevLine\t$prefixCounts{$prevLine}\n";
}
