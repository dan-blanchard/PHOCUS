#!/usr/bin/perl

# Dan Blanchard
# Segmentation Error Analyzer

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
my %affixes;
my %determiners;
my %vowels;

my $determinerUnderSegmentations = 0;
my $vowelOverSegmentations = 0;
my $affixOverSegmentations = 0;
my $affixGloms = 0;
my $determinerUnderSegmentationsInCrossingBrackets = 0;
my $vowelOverSegmentationsInCrossingBrackets = 0;
my $affixOverSegmentationsInCrossingBrackets = 0;
my $affixGlomsInCrossingBrackets = 0;



# Initialize Hashes
$affixes{"IN"} = $affixes{"z"} = $affixes{"s"} = $affixes{"In"} = $affixes{"~t"} = $affixes{"nt"} = $affixes{"li"} = $affixes{"6v"} = $affixes{"Id"} = $affixes{"d"} = 1;
$affixes{"In"} = 1;
$determiners{"D6"} = $determiners{"6"} = 1;
$vowels{"&"} = $vowels{"6"} = $vowels{"9"} = $vowels{"A"} = $vowels{"E"} = $vowels{"I"} = $vowels{"O"} = $vowels{"U"} = $vowels{"a"} = $vowels{"e"} = $vowels{"i"} = $vowels{"o"} = $vowels{"u"} = 1;
$vowels{"9I"} = $vowels{"9U"} = $vowels{"OI"} = 1; # diphthongs
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
#									   y,u,w,a,n,t,t,u
# Example: "yu want tu" should return [0,1,0,0,0,1,0,1]
# 									   0,1,2,3,4,5,6,7
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
		
		my $vowelOverSegmentationsSinceLastTrueBoundary = 0;
		my $affixOverSegmentationsSinceLastTrueBoundary = 0;
		my $affixGlomsSinceLastTrueBoundary = 0;
		my $utteranceDeterminerUnderSegmentations = 0;
		my $utteranceVowelOverSegmentations = 0;
		my $utteranceAffixOverSegmentations = 0;
		my $utteranceAffixGloms = 0;
		my $utteranceDeterminerUnderSegmentationsInCrossingBrackets = 0;
		my $utteranceVowelOverSegmentationsInCrossingBrackets = 0;
		my $utteranceAffixOverSegmentationsInCrossingBrackets = 0;
		
		my $utteranceMissingBoundaries = 0;
		my $utteranceExtraBoundaries = 0;
		my $utteranceCrossingBrackets = 0;
		my $utteranceMatchedBoundaries = 0;
		my $utteranceMatchedLackOfBoundaries = 0;
		
		my $previousTrueBoundary = -1;
		my $previousFoundBoundary = -1;
		my $currentFoundWord = "";
		my $currentTrueWord = "";

		# Loop through both segmentation arrays at the same time.
		for (my $i = 0; $i < scalar(@trueArray); $i++) 
		{
			# Check if this is the end/start of a new found word
			if ($foundArray[$i] == 1)
			{
				$currentFoundWord = substr($unsegFoundLine, $previousFoundBoundary + 1, $i - $previousFoundBoundary);
				$previousFoundBoundary = $i;
				$utteranceFoundTotalWords++;
			}
			
			# Check for word boundary in true segmentation
			if ($trueArray[$i] == 0)
			{
				# See if found segmentation disagrees
				if ($foundArray[$i] == 1)
				{
					$extraSinceLastTrueBoundary++;
					if (exists $vowels{$currentFoundWord})
					{
						$vowelOverSegmentationsSinceLastTrueBoundary++;
					}
					elsif (exists $affixes{$currentFoundWord})
					{
						$affixOverSegmentationsSinceLastTrueBoundary++;
					}
				}
				else
				{
					$utteranceMatchedLackOfBoundaries++;
				}
			}
			else
			{
				$currentTrueWord = substr($unsegTrueLine, $previousTrueBoundary + 1, $i - $previousTrueBoundary);
				$previousTrueBoundary = $i;
							
				# See if found segmentation disagrees
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
						if (exists $determiners{$currentTrueWord})
						{
							$utteranceDeterminerUnderSegmentationsInCrossingBrackets++;
						}
						if ($vowelOverSegmentationsSinceLastTrueBoundary > 0)
						{
							$utteranceVowelOverSegmentationsInCrossingBrackets++;
						}
						if ($affixOverSegmentationsSinceLastTrueBoundary > 0)
						{
							$utteranceAffixOverSegmentationsInCrossingBrackets++;
						}
					}
					else
					{
						$utteranceUnderSegmentedWords++;
						if (exists $determiners{$currentTrueWord})
						{
							$utteranceDeterminerUnderSegmentations++;
						}
					}
					$previousBoundaryMissing = $currentBoundaryMissing;
					$currentBoundaryMissing = 0;
				}
				else
				{
					if ($extraSinceLastTrueBoundary > 0)
					{
						$utteranceOverSegmentedWords++;
						if ($vowelOverSegmentationsSinceLastTrueBoundary > 0)
						{
							$utteranceVowelOverSegmentations++;
						}
						if ($affixOverSegmentationsSinceLastTrueBoundary > 0)
						{
							$utteranceAffixOverSegmentations++;
						}
					}
				}
				$utteranceExtraBoundaries += $extraSinceLastTrueBoundary;
				$affixOverSegmentationsSinceLastTrueBoundary = $vowelOverSegmentationsSinceLastTrueBoundary = $extraSinceLastTrueBoundary = 0;
				
				
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
		$determinerUnderSegmentations += $utteranceDeterminerUnderSegmentations;
		$affixGloms += $utteranceAffixGloms;
		$affixOverSegmentations += $utteranceAffixOverSegmentations;
		$vowelOverSegmentations += $utteranceVowelOverSegmentations;
		$determinerUnderSegmentationsInCrossingBrackets += $utteranceDeterminerUnderSegmentationsInCrossingBrackets;
		$affixOverSegmentationsInCrossingBrackets += $utteranceAffixOverSegmentationsInCrossingBrackets;
		$vowelOverSegmentationsInCrossingBrackets += $utteranceVowelOverSegmentationsInCrossingBrackets;
		
		if ($opt_v)
		{
			my $boundaryPrecision = (($utteranceMatchedBoundaries > 0) ? ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceExtraBoundaries)) : 0);
			my $boundaryRecall = (($utteranceMatchedBoundaries > 0) ? ($utteranceMatchedBoundaries / ($utteranceMatchedBoundaries + $utteranceMissingBoundaries)) : 0);
			my $boundaryF = (($boundaryPrecision + $boundaryRecall) > 0) ? ((2 * $boundaryPrecision * $boundaryRecall) / ($boundaryPrecision + $boundaryRecall)) : 0;
			my $wordPrecision = ($utterancePerfectWords / $utteranceFoundTotalWords);
			my $wordRecall = ($utterancePerfectWords / $utteranceTrueTotalWords);
			my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
			
			print "\nScores for [$foundLine] against [$trueLine]:\n";
			print "  Boundaries\n" .
				  "  ----------\n" .
				  "  Missing (false neg.): $utteranceMissingBoundaries\n" .
				  "  Extra (false pos.): $utteranceExtraBoundaries\n" .
				  "  Correct (true pos.): " . $utteranceMatchedBoundaries . "\n" .
				  "  Incorrect: " . (($utteranceTrueTotalWords + 1) - $utteranceMatchedBoundaries) . "\n" .
				  "  True Total: " . ($utteranceTrueTotalWords + 1) . "\n" .
				  "  Found Total: " . ($utteranceFoundTotalWords + 1) . "\n" .
				  "  Precision: " . $boundaryPrecision * 100 . "%\n" .
				  "  Recall: " . $boundaryRecall * 100 . "%\n" .
				  "  F: " . $boundaryF * 100 . "%\n\n" .
				  "  Words\n" .
				  "  ----------\n" .
				  "  Only Over-segmented: $utteranceOverSegmentedWords\n";
			if ($utteranceOverSegmentedWords > 0)
			{
			  	print "    Vowel over-segmentations: $utteranceVowelOverSegmentations (" . (($utteranceVowelOverSegmentations / $utteranceOverSegmentedWords) * 100) . "%)\n";
				print "    Affix over-segmentations: $utteranceAffixOverSegmentations (" . (($utteranceAffixOverSegmentations / $utteranceOverSegmentedWords) * 100) . "%)\n";
				
			}
			print "  Only Under-segmented: $utteranceUnderSegmentedWords\n";
			if ($utteranceUnderSegmentedWords > 0)
			{
				print "    Determiner under-segmentations: $utteranceDeterminerUnderSegmentations (" . (($utteranceDeterminerUnderSegmentations / $utteranceUnderSegmentedWords) * 100) . "%)\n";
			}
			print "  Both Over- and Under-segmented: $utteranceCrossingBrackets\n";
			if ($utteranceCrossingBrackets > 0)
			{
				print "    Determiner under-segmentations: $utteranceDeterminerUnderSegmentationsInCrossingBrackets (" . (($utteranceDeterminerUnderSegmentationsInCrossingBrackets / $utteranceCrossingBrackets) * 100) . "%)\n";
				print "    Vowel over-segmentations: $utteranceVowelOverSegmentationsInCrossingBrackets (" . (($utteranceVowelOverSegmentationsInCrossingBrackets / $utteranceCrossingBrackets) * 100) . "%)\n";
				print "    Affix over-segmentations: $utteranceAffixOverSegmentationsInCrossingBrackets (" . (($utteranceAffixOverSegmentationsInCrossingBrackets / $utteranceCrossingBrackets) * 100) . "%)\n";
			}
			print "  Correct (true pos.): $utterancePerfectWords\n" .
				  "  Incorrect found words (false pos.): " . ($utteranceFoundTotalWords - $utterancePerfectWords) . "\n" .
				  "  Missing true words (false neg.): " . ($utteranceTrueTotalWords - $utterancePerfectWords) . "\n" .
				  "  True Total: $utteranceTrueTotalWords\n" .
				  "  Found Total: $utteranceFoundTotalWords\n" .
				  "  Precision: " . $wordPrecision * 100 . "%\n" .
				  "  Recall: " . $wordRecall * 100 . "%\n" . 
				  "  F: " . $wordF * 100 . "%\n"; 
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
	  "Only Over-segmented: $overSegmentedWords (" . (($overSegmentedWords / $wordFalseNeg) * 100). "%)\n";
if ($overSegmentedWords > 0)
{
  	print "  Vowel over-segmentations: $vowelOverSegmentations (" . (($vowelOverSegmentations / $overSegmentedWords) * 100) . "%)\n";
	print "  Affix over-segmentations: $affixOverSegmentations (" . (($affixOverSegmentations / $overSegmentedWords) * 100) . "%)\n";
	
}
print "Only Under-segmented: $underSegmentedWords\n";
if ($underSegmentedWords > 0)
{
	print "  Determiner under-segmentations: $determinerUnderSegmentations (" . (($determinerUnderSegmentations / $underSegmentedWords) * 100) . "%)\n";
}
print "Both Over- and Under-segmented: $crossingBrackets\n";
if ($crossingBrackets > 0)
{
	print "  Determiner under-segmentations: $determinerUnderSegmentationsInCrossingBrackets (" . (($determinerUnderSegmentationsInCrossingBrackets / $crossingBrackets) * 100) . "%)\n";
	print "  Vowel over-segmentations: $vowelOverSegmentationsInCrossingBrackets (" . (($vowelOverSegmentationsInCrossingBrackets / $crossingBrackets) * 100) . "%)\n";
	print "  Affix over-segmentations: $affixOverSegmentationsInCrossingBrackets (" . (($affixOverSegmentationsInCrossingBrackets / $crossingBrackets) * 100) . "%)\n";
}
print "Correct (true pos.): $perfectWords\n" .
	  "Incorrect found words (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
	  "Missing true words (false neg.): " . ($trueTotalWords - $perfectWords) . "\n" .
	  "True Total: $trueTotalWords\n" .
	  "Found Total: $foundTotalWords\n" .
	  "Precision: " . $wordPrecision * 100 . "%\n" .
	  "Recall: " . $wordRecall * 100 . "%\n" . 
	  "F: " . $wordF * 100 . "%\n"; 
	  