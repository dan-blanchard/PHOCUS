#!/usr/bin/perl

# Dan Blanchard
# Error Analyzer

# usage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE 

use strict;
use Getopt::Std;

our ($opt_d);
my $wordDelimiter = ' ';
my $goldLine;
my $line;
my $underSegmentationCount = 0;
my $overSegmentationCount = 0;
my $crossingBracketCount = 0;
my $trueOpen = 0;
my $trueClose = 0;
my $open = 0;
my $close = 0;
my $currentIndex = 0;
my $currentChar = "";
my $trueChar = "";
my $shiftedChars = 0;
# Handle arguments
getopts('d:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

die "\nSegmentation Error Analyzer\nusage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE\n" if @ARGV < 2;

open(GOLDFILE, $ARGV[0]);
open(FILE, $ARGV[1]);

while ($goldLine = <GOLDFILE>)
{
	chomp $goldLine;
	$goldLine =~ s/^(.+)$/\($1\)/;
	$goldLine =~ s/\Q$wordDelimiter\E/\)\(/g;
	
	$line = <FILE>;
	chomp $line;
	$line =~ s/^(.+)$/\($1\)/;
	$line =~ s/\Q$wordDelimiter\E/\)\(/g;
	
	$currentIndex = -1;
	$shiftedChars = 0;
	foreach $trueChar (split //, $goldLine)
	{
		print "True char: $trueChar\tShifted Chars: $shiftedChars\n";
		if ($trueChar eq "(")
		{
			$trueOpen++;
		}
		elsif ($trueChar eq ")")
		{
			$trueClose++;
		}
		
		if ($shiftedChars == 0)
		{
			do
			{
				$currentIndex++;
				if ($currentIndex < (length $line))
				{
					$currentChar = substr($line,$currentIndex,1);
					print "Current char: $currentChar\tCurrent index: $currentIndex\tLine length: " . (length $line) . "\n";
					if ($currentChar eq "(")
					{
						$open++;
					}
					elsif ($currentChar eq ")")
					{
						$close++;
					}
					elsif ($currentChar ne $trueChar)
					{
						$shiftedChars++;
					}
				}	
			} 
			while (($currentChar ne $trueChar) && ($currentIndex < (length $line)));
		}
		elsif (($trueChar ne "(") && ($trueChar ne ")"))
		{
			$shiftedChars--;
		}
		
		if ($trueClose == $trueOpen)
		{
			if ($open == $close)
			{
				if ($open > $trueOpen)
				{
					$overSegmentationCount += $open - $trueOpen;
				}
				elsif ($open < $trueOpen)
				{
					$underSegmentationCount += $trueOpen - $open;
				}
				$open = $close = 0;
			}
			else
			{
				$crossingBracketCount++;
			}
			$trueOpen = $trueClose = 0;
		}
	}
	print "\nScore for $line against $goldLine:\n";
	print "\tOver-segmentations: $overSegmentationCount\tUnder-segmentations: $underSegmentationCount\tCrossing-Brackets: $crossingBracketCount\n";
}




























