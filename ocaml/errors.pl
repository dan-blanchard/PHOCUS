#!/usr/bin/perl

# Dan Blanchard
# Error Analyzer

# usage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE 

use strict;
use Getopt::Std;

our ($opt_d,$opt_v);
my $wordDelimiter = ' ';
my $trueLine;
my $foundLine;
my $missingBoundaries = 0;
my $extraBoundaries = 0;
my $crossingBracketCount = 0;
my $trueTotalBoundaries = 0;
my $foundTotalBoundaries = 0;
my $trueTotalWords = 0;
my $foundTotalWords = 0;
my $perfectWords = 0;
my $matchedPositions = 0;

# Handle arguments
getopts('vd:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

die "\nSegmentation Error Analyzer\nusage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE\n" if @ARGV < 2;

# Take a segmented utterance, and returns an array of binary values that specify whether there is 
# a break in the string after the indexed position in the array.  The indices match up with an unsegmented
# version of the string. 
#
# Example: "yu want tu" should return [0,1,0,0,0,1,0,1]

sub stringToSegArray
{
	my $utterance = shift;
	my $unsegUtterance = $utterance;
	$unsegUtterance =~ s/\Q$wordDelimiter\E//g;	
	if ((substr $utterance, -1, 1) ne $wordDelimiter)
	{
		$utterance = $utterance . $wordDelimiter;		
	}
	my $uttChar = "";
	my $boundaryCount = 0;
	my @segArray = ();
	for (my $i = 0; $i < length($unsegUtterance); $i++)
	{
		push @segArray, 0;
	}
	
	for (my $i = 0; $i < length($utterance); $i++)
	{
		$uttChar = substr($utterance, $i,1);
		if ($uttChar eq $wordDelimiter)
		{
			$boundaryCount++;
			$segArray[$i - $boundaryCount] = 1;
		}
	}
	return @segArray;
}

open(GOLDFILE, $ARGV[0]);
open(FILE, $ARGV[1]);

while ($trueLine = <GOLDFILE>)
{
	chomp $trueLine;
	$foundLine = <FILE>;
	chomp $foundLine;

	my $unsegLine = $foundLine;
	$unsegLine =~ s/\Q$wordDelimiter\E//g;
	$unsegLine =~ s/(.)/\1 /g;
	my @trueArray = stringToSegArray($trueLine);
	my @foundArray = stringToSegArray($foundLine);
	
	my $missingSinceLastAgree = 0;
	my $extraSinceLastAgree = 0;
	my $crossingsSinceLastAgree = 0;
	
	my $utteranceMissingBoundaries = 0;
	my $utteranceExtraBoundaries = 0;
	my $utteranceCrossingBrackets = 0;
	
	# Loop through both segmentation arrays at the same time.
	for (my $i = 0; $i < scalar(@trueArray); $i++) 
	{
		# Check for word boundary in true corpus
		if ($trueArray[$i] == 0)
		{
			# See if found corpus disagrees
			if ($foundArray[$i] == 1)
			{
				# Check for crossing-brackets
				if ($missingSinceLastAgree > 0)
				{
					$missingSinceLastAgree--;
					$crossingsSinceLastAgree++;
				}
				else
				{
					$extraSinceLastAgree++;
				}
			}
		}
		else
		{
			$trueTotalWords++;
			# See if found corpus disagrees
			if ($foundArray[$i] == 0)
			{
				# Check for crossing-brackets
				if ($extraSinceLastAgree > 0)
				{
					$crossingsSinceLastAgree++;
					$extraSinceLastAgree--;
				}
				else
				{
					$missingSinceLastAgree++;
				}
			}
			# Update utterance error counts when found and true agree on a word boundary.
			else
			{
				if (($extraSinceLastAgree == 0) && ($missingSinceLastAgree == 0) && ($crossingsSinceLastAgree == 0))
				{
					$perfectWords++;
				}
				$utteranceMissingBoundaries += $missingSinceLastAgree;
				$utteranceExtraBoundaries += $extraSinceLastAgree;
				$utteranceCrossingBrackets += $crossingsSinceLastAgree;
				$missingSinceLastAgree = $extraSinceLastAgree = 0;					
			}
		}
		if ($foundArray[$i] == 1)
		{
			$foundTotalWords++;
		}
	}
	
	$missingBoundaries += $utteranceMissingBoundaries;
	$extraBoundaries += $utteranceExtraBoundaries;
	$crossingBracketCount += $utteranceCrossingBrackets;
	
	if ($opt_v)
	{
		print "\nScore for [$foundLine] against [$trueLine]:\n";
		print "\tExtra boundaries: $utteranceExtraBoundaries\tMissing boundaries: $utteranceMissingBoundaries\tCrossing brackets: $utteranceCrossingBrackets\n";
	}
}

print "\nStats\n---------\nExtra boundaries: $extraBoundaries\n" . 
	"Missing boundaries: $missingBoundaries\n" . 
	"Crossing brackets: $crossingBracketCount\n" .
	"Crossing brackets: $crossingBracketCount\n" .
	"True total words: $trueTotalWords\n" .
	"Found total words: $foundTotalWords\n";
