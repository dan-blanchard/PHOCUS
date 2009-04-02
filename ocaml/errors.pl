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
	
	my $unsegFoundLine = $foundLine;
	$unsegFoundLine =~ s/\Q$wordDelimiter\E//g;
	
	my $unsegTrueLine = $trueLine;
	$unsegTrueLine =~ s/\Q$wordDelimiter\E//g;
	
	die "Utterances do not match!\n" if ($unsegTrueLine ne $unsegFoundLine);
	
	# Ignores first x lines specified by the -i argument
	if ($opt_i < $lineNumber)
	{
		my @trueArray = stringToSegArray($trueLine);
		my @foundArray = stringToSegArray($foundLine);

		my $extraSinceLastTrueBoundary = 0;
		my $previousBoundaryMissing = 0;
		my $currentBoundaryMissing = 0;
		
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
					$currentBoundaryMissing = 1;
				}
				# Found and true agree on a word boundary.
				else
				{
					if (($previousBoundaryMissing == 0) && ($extraSinceLastTrueBoundary == 0))
					{
						$utterancePerfectWords++;
					}
					$utteranceMatchedBoundaries++;
				}
				
				$utteranceTrueTotalWords++;
				if ($currentBoundaryMissing || $previousBoundaryMissing)
				{
					if ($extraSinceLastTrueBoundary > 0)
					{
						$utteranceCrossingBrackets++;
					}
					else
					{
						$utteranceUnderSegmentedWords++;
					}
					$previousBoundaryMissing = $currentBoundaryMissing;
					$currentBoundaryMissing = 0;
				}
				else
				{
					if ($extraSinceLastTrueBoundary > 0)
					{
						$utteranceOverSegmentedWords++;
					}
				}
				$utteranceExtraBoundaries += $extraSinceLastTrueBoundary;
				$extraSinceLastTrueBoundary = 0;
				
			}
			if ($foundArray[$i] == 1)
			{
				$utteranceFoundTotalWords++;
			}
		}

		$utteranceMatchedBoundaries--; # Only count phrase-internal boundaries
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
		$foundTotalBoundaries += ($utteranceFoundTotalWords - 1);
		$trueTotalBoundaries += ($utteranceTrueTotalWords - 1);
		
		if ($opt_v)
		{
			my $boundaryPrecision = (($utteranceMatchedBoundaries > 0) ? ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceExtraBoundaries)) : 0);
			my $boundaryRecall = (($utteranceMatchedBoundaries > 0) ? ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceMissingBoundaries)) : 0);
			my $boundaryF = (($boundaryPrecision + $boundaryRecall) > 0) ? ((2 * $boundaryPrecision * $boundaryRecall) / ($boundaryPrecision + $boundaryRecall)) : 0;
			my $wordPrecision = ($utterancePerfectWords / $utteranceFoundTotalWords);
			my $wordRecall = ($utterancePerfectWords / $utteranceTrueTotalWords);
			my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
			
			print "\nScores for [$foundLine] against [$trueLine]:\n";
			print "\tBoundaries\n" .
				  "\t----------\n" .
				  "\tMissing (false neg.): $utteranceMissingBoundaries\n" .
				  "\tExtra (false pos.): $utteranceExtraBoundaries\n" .
				  "\tCorrect (true pos.): " . $utteranceMatchedBoundaries . "\n" .
				  "\tIncorrect: " . (($utteranceTrueTotalWords + 1) - $utteranceMatchedBoundaries) . "\n" .
				  "\tTrue Total: " . ($utteranceTrueTotalWords + 1) . "\n" .
				  "\tFound Total: " . ($utteranceFoundTotalWords + 1) . "\n" .
				  "\tPrecision: " . $boundaryPrecision * 100 . "%\n" .
				  "\tRecall: " . $boundaryRecall * 100 . "%\n" .
				  "\tF: " . $boundaryF * 100 . "%\n\n" .
				  "\tWords\n" .
				  "\t----------\n" .
				  "\tOnly Over-segmented: $utteranceOverSegmentedWords\n" .
				  "\tOnly Under-segmented: $utteranceUnderSegmentedWords\n" .
				  "\tBoth Over- and Under-segmented: $utteranceCrossingBrackets\n" .
				  "\tCorrect (true pos.): $utterancePerfectWords\n" .
				  "\tIncorrect found words (false pos.): " . ($utteranceFoundTotalWords - $utterancePerfectWords) . "\n" .
				  "\tMissing true words (false neg.): " . ($utteranceTrueTotalWords - $utterancePerfectWords) . "\n" .
				  "\tTrue Total: $utteranceTrueTotalWords\n" .
				  "\tFound Total: $utteranceFoundTotalWords\n" .
				  "\tPrecision: " . $wordPrecision * 100 . "%\n" .
				  "\tRecall: " . $wordRecall * 100 . "%\n" . 
				  "\tF: " . $wordF * 100 . "%\n"; 
		}
	}
}
die "Ignored more than length of file!\n" if ($trueTotalWords == 0);


print "\n\nResults for $ARGV[1]:";
if ($opt_i)
{
	print " (ignoring first $opt_i utterances)";
}

my $boundaryPrecision = (($matchedBoundaries > 0) ? ($matchedBoundaries / ($matchedBoundaries + $extraBoundaries)) : 0);
my $boundaryRecall = (($matchedBoundaries > 0) ? ($matchedBoundaries / ($matchedBoundaries + $missingBoundaries)) : 0);
my $boundaryF = (($boundaryPrecision + $boundaryRecall) > 0) ? ((2 * $boundaryPrecision * $boundaryRecall) / ($boundaryPrecision + $boundaryRecall)) : 0;
my $wordPrecision = ($perfectWords / $foundTotalWords);
my $wordRecall = ($perfectWords / $trueTotalWords);
my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
my $wordFalseNeg = $trueTotalWords - $perfectWords;

print "\n============================\n\n" .
	  "Boundaries\n" .
	  "----------\n" .
	  "Missing (false neg.): $missingBoundaries\n" .
	  "Extra (false pos.): $extraBoundaries\n" .
	  "Correct (true pos.): " . $matchedBoundaries . "\n" .
	  "Incorrect: " . ($trueTotalBoundaries - $matchedBoundaries) . "\n" .
	  "True Total: " . $trueTotalBoundaries . "\n" .
	  "Found Total: " . $foundTotalBoundaries . "\n" .
	  "Precision: " . $boundaryPrecision * 100 . "%\n" .
	  "Recall: " . $boundaryRecall * 100 . "%\n" .
	  "F: " . $boundaryF * 100 . "%\n\n" .
	  "Words\n" .
	  "----------\n" .
	  "Only Over-segmented: $overSegmentedWords (" . (($overSegmentedWords / $wordFalseNeg) * 100). "%)\n" .
	  "Only Under-segmented: $underSegmentedWords (" . (($underSegmentedWords / $wordFalseNeg) * 100). "%)\n" .
	  "Both Over- and Under-segmented: $crossingBrackets (" . (($crossingBrackets / $wordFalseNeg) * 100) . "%)\n" .
	  "Correct (true pos.): $perfectWords\n" .
	  "Incorrect found words (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
	  "Missing true words (false neg.): " . ($trueTotalWords - $perfectWords) . "\n" .
	  "True Total: $trueTotalWords\n" .
	  "Found Total: $foundTotalWords\n" .
	  "Precision: " . $wordPrecision * 100 . "%\n" .
	  "Recall: " . $wordRecall * 100 . "%\n" . 
	  "F: " . $wordF * 100 . "%\n"; 
	  