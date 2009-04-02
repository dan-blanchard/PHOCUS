#!/usr/bin/perl

# Dan Blanchard
# Error Analyzer

# usage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE 

use strict;
use Getopt::Std;

our ($opt_d,$opt_v,$opt_i);
my $wordDelimiter = ' ';
my $trueLine;
my $foundLine;
my $missingBoundaries = 0;
my $extraBoundaries = 0;
my $crossingBrackets = 0;
my $trueTotalBoundaries = 0;
my $foundTotalBoundaries = 0;
my $trueTotalWords = 0;
my $foundTotalWords = 0;
my $perfectWords = 0;
my $matchedBoundaries = 0;
my $matchedLackOfBoundaries = 0;
my $lineNumber = 0;
my $underSegmentedWords = 0;
my $overSegmentedWords = 0;

# Handle arguments
getopts('vd:i:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

die "\nSegmentation Error Analyzer\nusage: ./errors.pl [-d WORD_DELIMITER] [-i IGNORE_LINE_NUM] GOLD-CORPUS FILE\n" if @ARGV < 2;

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
	$lineNumber++;
	chomp $trueLine;
	$foundLine = <FILE>;
	chomp $foundLine;
	
	# Ignores first x lines specified by the -i argument
	if ($opt_i < $lineNumber)
	{
		my $unsegLine = $foundLine;
		$unsegLine =~ s/\Q$wordDelimiter\E//g;
		$unsegLine =~ s/(.)/\1 /g;
		my @trueArray = stringToSegArray($trueLine);
		my @foundArray = stringToSegArray($foundLine);

		my $previousErrorType = 0;	# 0 = no error, 1 = missing boundary, 2 = extra boundary
		my $extraSinceLastTrueBoundary = 0;
		
		my $utteranceUnderSegmentedWords = 0;
		my $utteranceOverSegmentedWords = 0;
		my $utterancePerfectWords = 0;
		my $utteranceFoundTotalWords = 0;
		my $utteranceTrueTotalWords = 0;
		
		my $utteranceMissingBoundaries = 0;
		my $utteranceExtraBoundaries = 0;
		my $utteranceCrossingBrackets = 0;
		my $utteranceMatchedBoundaries = 0;
		my $utteranceMatchedLackOfBoundaries = 0;

		# Loop through both segmentation arrays at the same time.
		for (my $i = 0; $i < scalar(@trueArray); $i++) 
		{
			# Check for word boundary in true corpus
			if ($trueArray[$i] == 0)
			{
				# See if found corpus disagrees
				if ($foundArray[$i] == 1)
				{
					$extraSinceLastTrueBoundary++;
					# Check for crossing-brackets
					if ($previousErrorType == 1)
					{
						$utteranceCrossingBrackets++;
					}
					$previousErrorType = 2;
				}
				else
				{
					$utteranceMatchedLackOfBoundaries++;
				}
			}
			else
			{
				# See if found corpus disagrees
				if ($foundArray[$i] == 0)
				{
					$utteranceMissingBoundaries++;
					# Check for crossing-brackets
					if ($previousErrorType == 2)
					{
						$utteranceCrossingBrackets++;
						$utteranceUnderSegmentedWords++;
					}
					else
					{
						$utteranceUnderSegmentedWords += 2;	
					}
					$previousErrorType = 1;
				}
				# Update utterance error counts when found and true agree on a word boundary.
				else
				{
					if ($previousErrorType == 0)
					{
						$utterancePerfectWords++;
					}
					$utteranceMatchedBoundaries++;
					$previousErrorType = 0;
				}
				
				$utteranceTrueTotalWords++;
				if ($extraSinceLastTrueBoundary > 0)
				{
					$utteranceOverSegmentedWords++;
				}
				$utteranceExtraBoundaries += $extraSinceLastTrueBoundary;
				$extraSinceLastTrueBoundary = 0;
			}
			if ($foundArray[$i] == 1)
			{
				$utteranceFoundTotalWords++;
			}
		}

		$utteranceMatchedBoundaries++; # For extra boundary at start of sentence
		$missingBoundaries += $utteranceMissingBoundaries;
		$extraBoundaries += $utteranceExtraBoundaries;
		$crossingBrackets += $utteranceCrossingBrackets;
		$overSegmentedWords += $utteranceOverSegmentedWords;
		$underSegmentedWords += $utteranceUnderSegmentedWords;
		$perfectWords += $utterancePerfectWords;
		$matchedBoundaries += $utteranceMatchedBoundaries;
		$matchedLackOfBoundaries += $utteranceMatchedLackOfBoundaries;
		$foundTotalWords += $utteranceFoundTotalWords;
		$trueTotalWords += $utteranceTrueTotalWords;
		$foundTotalBoundaries += ($utteranceFoundTotalWords + 1);
		$trueTotalBoundaries += ($utteranceTrueTotalWords + 1);
				
		if ($opt_v)
		{
			print "\nScores for [$foundLine] against [$trueLine]:\n";
			print "\tBoundaries\n" .
				  "\t----------\n" .
				  "\tMissing (false neg.): $utteranceMissingBoundaries\n" .
				  "\tExtra (false pos.): $utteranceExtraBoundaries\n" .
				  "\tCorrect (true pos.): " . $utteranceMatchedBoundaries . "\n" .
				  "\tIncorrect: " . (($utteranceTrueTotalWords + 1) - $utteranceMatchedBoundaries) . "\n" .
				  "\tTrue Total: " . ($utteranceTrueTotalWords + 1) . "\n" .
				  "\tFound Total: " . ($utteranceFoundTotalWords + 1) . "\n" .
				  "\tPrecision: " . ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceExtraBoundaries)) . "\n" .
				  "\tRecall: " . ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceMissingBoundaries)) . "\n\n" .
				  "\tWords\n" .
				  "\t----------\n" .
				  "\tOver-segmented: $utteranceOverSegmentedWords\n" .
				  "\tUnder-segmented: $utteranceUnderSegmentedWords\n" .
				  "\tCrossing Brackets: $utteranceCrossingBrackets\n" .
				  "\tCorrect (true pos.): $utterancePerfectWords\n" .
				  "\tExtra (false pos.): " . ($utteranceFoundTotalWords - $utterancePerfectWords) . "\n" .
				  "\tMissing (false neg.): " . ($utteranceTrueTotalWords - $utterancePerfectWords) . "\n" .
				  "\tTrue Total: $utteranceTrueTotalWords\n" .
				  "\tFound Total: $utteranceFoundTotalWords\n" .
				  "\tPrecision: " . ($utterancePerfectWords / $utteranceFoundTotalWords) . "\n" .
				  "\tRecall: " . ($utterancePerfectWords / $utteranceTrueTotalWords) . "\n";
		}
	}
}

print "\n\nResults for $ARGV[1]:";
if ($opt_i)
{
	print " (ignoring first $opt_i utterances)";
}
print "\n============================\n\n" .
	  "Boundaries\n" .
	  "----------\n" .
	  "Missing (false neg.): $missingBoundaries\n" .
	  "Extra (false pos.): $extraBoundaries\n" .
	  "Correct (true pos.): " . $matchedBoundaries . "\n" .
	  "Incorrect: " . ($trueTotalBoundaries - $matchedBoundaries) . "\n" .
	  "True Total: " . $trueTotalBoundaries . "\n" .
	  "Found Total: " . $foundTotalBoundaries . "\n" .
	  "Precision: " . ($matchedBoundaries / ($matchedBoundaries + $extraBoundaries)) . "\n" .
	  "Recall: " . ($matchedBoundaries / ($matchedBoundaries + $missingBoundaries)) . "\n\n" .
	  "Words\n" .
	  "----------\n" .
	  "Over-segmented: $overSegmentedWords (" . (($overSegmentedWords / ($trueTotalWords - $perfectWords)) * 100). "%)\n" .
	  "Under-segmented: $underSegmentedWords (" . (($underSegmentedWords / ($trueTotalWords - $perfectWords)) * 100). "%)\n" .
	  "Crossing Brackets: $crossingBrackets (" . (($crossingBrackets / ($trueTotalWords - $perfectWords)) * 100) . "%)\n" .
	  "Correct (true pos.): $perfectWords\n" .
	  "Extra (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
	  "Missing (false neg.): " . ($trueTotalWords - $perfectWords) . "\n" .
	  "True Total: $trueTotalWords\n" .
	  "Found Total: $foundTotalWords\n" .
	  "Precision: " . ($perfectWords / $foundTotalWords) . "\n" .
	  "Recall: " . ($perfectWords / $trueTotalWords) . "\n";