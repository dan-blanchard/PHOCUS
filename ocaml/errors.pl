#!/usr/bin/perl

# Dan Blanchard
# Segmentation Error Analyzer

# usage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE 

# TODO: Double-check/fix method for counting specific types of over and under segmentations. 

use strict;
use Getopt::Std;

our ($opt_d,$opt_v,$opt_i,$opt_e);
my $wordDelimiter = ' ';
my $trueLine;
my $foundLine;
my $crossingBrackets = 0;
my $trueTotalWords = 0;
my $foundTotalWords = 0;
my $perfectWords = 0;
my $lineNumber = 0;
my $underSegmentedWords = 0;
my $overSegmentedWords = 0;
my %affixes;
my %determiners;
my %vowels;
my %overSegmentedWordFrequencies;
my %underSegmentedWordFrequencies;
my %bothErrorsWordFrequencies;

my $determinerUnderSegmentations = 0;
my $vowelOverSegmentations = 0;
my $affixOverSegmentations = 0;
my $affixGloms = 0;



# Initialize Hashes
$affixes{"IN"} = $affixes{"z"} = $affixes{"s"} = $affixes{"In"} = $affixes{"~t"} = $affixes{"nt"} = $affixes{"li"} = $affixes{"6v"} = $affixes{"Id"} = $affixes{"d"} = 1;
$affixes{"In"} = 1;
$determiners{"D6"} = $determiners{"6"} = 1;
$vowels{"&"} = $vowels{"6"} = $vowels{"9"} = $vowels{"A"} = $vowels{"E"} = $vowels{"I"} = $vowels{"O"} = $vowels{"U"} = $vowels{"a"} = $vowels{"e"} = $vowels{"i"} = $vowels{"o"} = $vowels{"u"} = 1;
$vowels{"9I"} = $vowels{"9U"} = $vowels{"OI"} = 1; # diphthongs
# Handle arguments
getopts('ved:i:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

die "\nSegmentation Error Analyzer\nUsage: ./errors.pl [-v] [-e] [-d WORD_DELIMITER] [-i IGNORE_LINE_NUM] GOLD-CORPUS FILE\n" if @ARGV < 2;

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

# Takes an unsegmented utterance, a start index, an end index, and a segmentation array for that utterance, and returns all words that intersect that span.
# Example: wordsInUtterance("yuwanttuit", 5, 7, [0,1,0,0,0,1,0,1,0,1]) returns ("want","tu")
#							 0123456789
#							      ^ ^
sub wordsInSubUtterance
{
	my $utterance = " " . shift;
	my $startIndex = shift;
	my $endIndex = shift;
	my @segArray = (1);
	push @segArray, @_;
	$startIndex++;
	$endIndex++;
	my @wordArray = ();
	my $currentWord = "";
	my $currentWordStart;
	my $currentWordEnd = scalar(@segArray) - 1;

	# This loop (and this whole function really) should be rewritten to work from the beginning to end of utterance, since that would be more efficient.
	for (my $i = scalar(@segArray) - 2; $i >= 0; $i--) 
	{
		if ($segArray[$i] == 1)
		{
			$currentWordStart = $i + 1;
			$currentWord = substr($utterance, $currentWordStart, $currentWordEnd - $i);
			if ((($currentWordStart <= $endIndex) && ($currentWordStart >= $startIndex)) 
			|| (($currentWordEnd <= $endIndex) && ($currentWordEnd >= $startIndex)) 
			|| (($currentWordEnd >= $endIndex) && ($currentWordStart <= $startIndex)))
			{
				push @wordArray, $currentWord;
			}
			elsif (scalar(@wordArray) > 0) # If we're no longer in the span, and we've already added stuff to the word array, we're done.
			{
				last;
			}
			$currentWordEnd = $i;
		}
		
	}	return reverse @wordArray;
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

		my $utteranceCrossingBrackets = 0;
		my $utteranceUnderSegmentedWords = 0;
		my $utteranceOverSegmentedWords = 0;
		my $utterancePerfectWords = 0;
		my $utteranceFoundTotalWords = 0;
		my $utteranceTrueTotalWords = 0;
		
		my $utteranceDeterminerUnderSegmentations = 0;
		my $utteranceVowelOverSegmentations = 0;
		my $utteranceAffixOverSegmentations = 0;
		my $utteranceAffixGloms = 0;
			
		my $previousTrueBoundary = -1;
		my $currentTrueWord = "";
		
		my @foundWordArray = ();
		my $foundLength;
		my $trueLength;
		my $foundSpan;
		
		my $trueWordVowelOverSegmentation = 0;
		my $trueWordAffixOverSegmentation = 0;
		
		# Loop through  both segmentation arrays at the same time.
		for (my $i = 0; $i < scalar(@trueArray); $i++) 
		{
			# Check if this is the end/start of a new true word
			if ($trueArray[$i] == 1)
			{
				$trueLength = $i - $previousTrueBoundary;
				$currentTrueWord = substr($unsegTrueLine, $previousTrueBoundary + 1, $trueLength);
				@foundWordArray = wordsInSubUtterance($unsegTrueLine, $previousTrueBoundary + 1, $i,@foundArray);
				$foundSpan = join($wordDelimiter,@foundWordArray);
				$foundLength = length($foundSpan) - (scalar(@foundWordArray) - 1);
				if (scalar(@foundWordArray) > 1) # Check for over-segmentation
				{
					if ($foundLength > $trueLength) # Check if this is also under-segmented
					{
						$utteranceCrossingBrackets++;
						if (exists $bothErrorsWordFrequencies{$currentTrueWord}{$foundSpan})
						{
							$bothErrorsWordFrequencies{$currentTrueWord}{$foundSpan}++;
						}
						else
						{
							$bothErrorsWordFrequencies{$currentTrueWord}{$foundSpan} = 1;
						}
						if (exists $determiners{$currentTrueWord})
						{
							$utteranceDeterminerUnderSegmentations++;
						}
					}
					else
					{
						$utteranceOverSegmentedWords++;
						if (exists $overSegmentedWordFrequencies{$currentTrueWord}{$foundSpan})
						{
							$overSegmentedWordFrequencies{$currentTrueWord}{$foundSpan}++;
						}
						else
						{
							$overSegmentedWordFrequencies{$currentTrueWord}{$foundSpan} = 1;
						}						
					}
					foreach my $currentFoundWord (@foundWordArray) # Examine each over-segmented word
					{
						if (exists $vowels{$currentFoundWord})
						{
							$trueWordVowelOverSegmentation = 1;
						}
						if (exists $affixes{$currentFoundWord})
						{
							$trueWordAffixOverSegmentation = 1;
						}						
					}
					$utteranceVowelOverSegmentations += $trueWordVowelOverSegmentation;
					$utteranceAffixOverSegmentations += $trueWordAffixOverSegmentation;
				}
				elsif ($foundLength > $trueLength) # Check for only under-segmentation
				{
					$utteranceUnderSegmentedWords++;					
					if (exists $determiners{$currentTrueWord})
					{
						$utteranceDeterminerUnderSegmentations++;
					}
					if (exists $underSegmentedWordFrequencies{$currentTrueWord}{$foundSpan})
					{
						$underSegmentedWordFrequencies{$currentTrueWord}{$foundSpan}++;
					}
					else
					{
						$underSegmentedWordFrequencies{$currentTrueWord}{$foundSpan} = 1;
					}
				}
				else # Otherwise, word is correctly segmented
				{
					die "Supposedly correct word is actually wrong!  Found [$foundSpan] instead of [$currentTrueWord] when comparing [$foundLine] to [$trueLine]!\n\tFound Seg Array: @foundArray\n\tTrue Seg Array:  @trueArray\n" if ($currentTrueWord ne $foundSpan);
					$utterancePerfectWords++;					
				}				
				
				$trueWordVowelOverSegmentation = 0;
				$trueWordAffixOverSegmentation = 0;
				$utteranceTrueTotalWords++;
				$utteranceFoundTotalWords += scalar(@foundWordArray);
				$previousTrueBoundary = $i;						
			} 
		}

		$crossingBrackets += $utteranceCrossingBrackets;
		$overSegmentedWords += $utteranceOverSegmentedWords;
		$underSegmentedWords += $utteranceUnderSegmentedWords;
		$perfectWords += $utterancePerfectWords;
		$foundTotalWords += $utteranceFoundTotalWords;
		$trueTotalWords += $utteranceTrueTotalWords;
		$determinerUnderSegmentations += $utteranceDeterminerUnderSegmentations;
		$affixGloms += $utteranceAffixGloms;
		$affixOverSegmentations += $utteranceAffixOverSegmentations;
		$vowelOverSegmentations += $utteranceVowelOverSegmentations;
		
		if ($opt_v)
		{
			my $wordPrecision = ($utterancePerfectWords / $utteranceFoundTotalWords);
			my $wordRecall = ($utterancePerfectWords / $utteranceTrueTotalWords);
			my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
			my $utteranceWordFalseNeg = $utteranceTrueTotalWords - $utterancePerfectWords;
			print "\nScores for [$foundLine] against [$trueLine]:\n";
			print "  True Words\n" .
				  "  ----------\n" .
				  "  Found just over-segmented: $utteranceOverSegmentedWords (";
			printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceOverSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
			print "  Found just under-segmented: $utteranceUnderSegmentedWords (";
			printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceUnderSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
			print "  Found both over- and under-segmented: $utteranceCrossingBrackets (";
			printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceCrossingBrackets / $utteranceTrueTotalWords) * 100) : 0;
		  	print "  Found with over-segmented vowels: $utteranceVowelOverSegmentations (";
			printf "%1.2f\%)\n", (($utteranceVowelOverSegmentations / $utteranceTrueTotalWords) * 100);
			print "  Found with over-segmented affixes: $utteranceAffixOverSegmentations (";
			printf "%1.2f\%)\n", (($utteranceAffixOverSegmentations / $utteranceTrueTotalWords) * 100);
			print "  Determiners that were found under-segmented: $utteranceDeterminerUnderSegmentations (";
			printf "%1.2f\%)\n", (($utteranceDeterminerUnderSegmentations / $utteranceTrueTotalWords) * 100);
			print "\n  Word Totals\n" .
				  "  ----------\n";
			print "  Correct (true pos.): $utterancePerfectWords\n" .
				  "  Incorrect found words (false pos.): " . ($utteranceFoundTotalWords - $utterancePerfectWords) . "\n" .
				  "  Missing true words (false neg.): " . ($utteranceTrueTotalWords - $utterancePerfectWords) . "\n" .
				  "  True Total: $utteranceTrueTotalWords\n" .
				  "  Found Total: $utteranceFoundTotalWords\n";
			printf "  Precision: %1.2f\%\n  Recall: %1.2f\%\n  F: %1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;
		}
	}
}
die "Ignored more than length of file!\n" if ($trueTotalWords == 0);


print "\n\nResults for $ARGV[1]:";
if ($opt_i)
{
	print " (ignoring first $opt_i utterances)";
}

my $wordPrecision = ($perfectWords / $foundTotalWords);
my $wordRecall = ($perfectWords / $trueTotalWords);
my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
my $wordFalseNeg = $trueTotalWords - $perfectWords;

print "\n============================\n\n";
print "True Words\n" .
	  "----------\n" .
	  "Found just over-segmented: $overSegmentedWords (";
printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($overSegmentedWords / $trueTotalWords) * 100) : 0;
print "Found just under-segmented: $underSegmentedWords (";
printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($underSegmentedWords / $trueTotalWords) * 100) : 0;
print "Found both over- and under-segmented: $crossingBrackets (";
printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($crossingBrackets / $trueTotalWords) * 100) : 0;
print "Found with over-segmented vowels: $vowelOverSegmentations (";
printf "%1.2f\%)\n", (($vowelOverSegmentations / $trueTotalWords) * 100);
print "Found with over-segmented affixes: $affixOverSegmentations (";
printf "%1.2f\%)\n", (($affixOverSegmentations / $trueTotalWords) * 100);
print "Determiners that were found under-segmented: $determinerUnderSegmentations (";
printf "%1.2f\%)\n", (($determinerUnderSegmentations / $trueTotalWords) * 100);
print "\nWord Totals\n" .
	  "----------\n";
print "Correct (true pos.): $perfectWords\n" .
	  "Incorrect found words (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
	  "Missing true words (false neg.): " . ($trueTotalWords - $perfectWords) . "\n" .
	  "True Total: $trueTotalWords\n" .
	  "Found Total: $foundTotalWords\n";
printf "Precision: %1.2f\%\nRecall: %1.2f\%\nF: %1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;

if ($opt_e) # Print errors, if asked
{
	print "\nUndersegmented words\n------------------\n";
	foreach my $key (sort (keys %underSegmentedWordFrequencies))
	{
		print $key . ":\n";
		foreach my $subKey (sort (keys %{$underSegmentedWordFrequencies{$key}}))
		{
			print "\t$subKey\t" . $underSegmentedWordFrequencies{$key}{$subKey} . "\n";
		}
	
	}

	print "\nOversegmented words\n------------------\n";
	foreach my $key (sort (keys %overSegmentedWordFrequencies))
	{
		print $key . ":\n";
		foreach my $subKey (sort (keys %{$overSegmentedWordFrequencies{$key}}))
		{
			print "\t$subKey\t" . $overSegmentedWordFrequencies{$key}{$subKey} . "\n";
		}
	}

	print "\nWords with both types of errors\n------------------\n";
	foreach my $key (sort (keys %bothErrorsWordFrequencies))
	{
		print $key . ":\n";
		foreach my $subKey (sort (keys %{$bothErrorsWordFrequencies{$key}}))
		{
			print "\t$subKey\t" . $bothErrorsWordFrequencies{$key}{$subKey} . "\n";
		}
	}
}