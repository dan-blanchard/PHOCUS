#!/usr/bin/perl

# Dan Blanchard
# Segmentation Error Analyzer

# usage: ./errors.pl [OPTIONS] GOLD-CORPUS FILE 

# TODO: Double-check/fix method for counting specific types of over and under segmentations. 

use strict;
use Getopt::Std;

our ($opt_d,$opt_v,$opt_i,$opt_e,$opt_b,$opt_s);
my $wordDelimiter = ' ';
my $trueLine;
my $foundLine;
my $trueWordCrossingBrackets = 0;
my $foundWordCrossingBrackets = 0;
my $trueTotalWords = 0;
my $foundTotalWords = 0;
my $perfectWords = 0;
my $lineNumber = 0;
my $trueWordUnderSegmentedWords = 0;
my $trueWordOverSegmentedWords = 0;
my $foundWordUnderSegmentedWords = 0;
my $foundWordOverSegmentedWords = 0;
my %affixes;
my %determiners;
my %vowels;
my %overSegmentedWordFrequencies;
my %underSegmentedWordFrequencies;
my %bothErrorsWordFrequencies;

my $trueWordDeterminerUnderSegmentations = 0;
my $trueWordVowelOverSegmentations = 0;
my $trueWordAffixOverSegmentations = 0;
my $foundWordDeterminerUnderSegmentations = 0;
my $foundWordVowelOverSegmentations = 0;
my $foundWordAffixOverSegmentations = 0;

my $utterancePerfectWords = 0;
my $utteranceFoundTotalWords = 0;
my $utteranceTrueTotalWords = 0;

# Initialize Hashes
$affixes{"IN"} = $affixes{"z"} = $affixes{"s"} = $affixes{"In"} = $affixes{"~t"} = $affixes{"nt"} = $affixes{"li"} = $affixes{"6v"} = $affixes{"Id"} = $affixes{"d"} = 1;
$affixes{"In"} = 1;
$determiners{"D6"} = $determiners{"6"} = 1;
$vowels{"&"} = $vowels{"6"} = $vowels{"9"} = $vowels{"A"} = $vowels{"E"} = $vowels{"I"} = $vowels{"O"} = $vowels{"U"} = $vowels{"a"} = $vowels{"e"} = $vowels{"i"} = $vowels{"o"} = $vowels{"u"} = 1;
$vowels{"9I"} = $vowels{"9U"} = $vowels{"OI"} = 1; # diphthongs
# Handle arguments
getopts('sved:i:b:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

die "\nSegmentation Error Analyzer\nUsage: ./errors.pl [-s] [-v] [-e] [-d WORD_DELIMITER] [-i IGNORE_LINE_NUM] [-b BLOCK_SIZE] GOLD_CORPUS FILE\n" if @ARGV < 2;

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

print "\n\nResults for $ARGV[1]:\n";
if ($opt_i)
{
	print " (ignoring first $opt_i utterances)";
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
		if ($opt_v)
		{
			print "\nScores for [$foundLine] against [$trueLine]:";
		}

		calculateFoundWordStats($unsegTrueLine);
		calculateTrueWordStats($unsegTrueLine);
		
		if ($opt_v)
		{
			my $wordPrecision = ($utterancePerfectWords / $utteranceFoundTotalWords);
			my $wordRecall = ($utterancePerfectWords / $utteranceTrueTotalWords);
			my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
			my $utteranceWordFalseNeg = $utteranceTrueTotalWords - $utterancePerfectWords;
			print "\n  Word Stats\n" .
				  "  ----------\n";
			print "  Correct (true pos.): $utterancePerfectWords\n" .
				  "  Incorrect found words (false pos.): " . ($utteranceFoundTotalWords - $utterancePerfectWords) . "\n" .
				  "  Missing true words (false neg.): " . ($utteranceTrueTotalWords - $utterancePerfectWords) . "\n";
			printf "  Precision: %1.2f\%\n  Recall: %1.2f\%\n  F: %1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;
		}
		
		if (($opt_b) && ($lineNumber % $opt_b == 0))
		{
			printCurrentTotalResults($lineNumber);
			$trueWordCrossingBrackets = 0;
			$foundWordCrossingBrackets = 0;
			$trueTotalWords = 0;
			$foundTotalWords = 0;
			$perfectWords = 0;
			$trueWordUnderSegmentedWords = 0;
			$trueWordOverSegmentedWords = 0;
			$foundWordUnderSegmentedWords = 0;
			$foundWordOverSegmentedWords = 0;
			%affixes = ();
			%determiners = ();
			%vowels = ();
			%overSegmentedWordFrequencies = ();
			%underSegmentedWordFrequencies = ();
			%bothErrorsWordFrequencies = ();           
			$trueWordDeterminerUnderSegmentations = 0;
			$trueWordVowelOverSegmentations = 0;
			$trueWordAffixOverSegmentations = 0;
			$foundWordDeterminerUnderSegmentations = 0;
			$foundWordVowelOverSegmentations = 0;
			$foundWordAffixOverSegmentations = 0;
		}
	}
}
if (!$opt_b && $opt_s)
{
	print "WP\tWR\tWF\n";
}
printCurrentTotalResults($opt_b ? $lineNumber : 0); 

sub calculateTrueWordStats
{
	my $unsegTrueLine = shift;

	my @trueArray = stringToSegArray($trueLine);
	my @foundArray = stringToSegArray($foundLine);

	my $utteranceTrueWordCrossingBrackets = 0;
	my $utteranceTrueWordUnderSegmentedWords = 0;
	my $utteranceTrueWordOverSegmentedWords = 0;
	$utteranceTrueTotalWords = 0;	
	$utterancePerfectWords = 0;
	my $utteranceTrueWordDeterminerUnderSegmentations = 0;
	my $utteranceTrueWordVowelOverSegmentations = 0;
	my $utteranceTrueWordAffixOverSegmentations = 0;

	my $previousTrueBoundary = -1;
	my $currentTrueWord = "";

	my @foundWordArray = ();
	my $foundLength;
	my $trueLength;
	my $foundSpan;
	
	my $trueWordVowelOverSegmentation = 0;
	my $trueWordAffixOverSegmentation = 0;
	
	# Loop through true segmentation array to gather stats with respect to true words.
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
					$utteranceTrueWordCrossingBrackets++;
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
						$utteranceTrueWordDeterminerUnderSegmentations++;
					}
				}
				else
				{
					$utteranceTrueWordOverSegmentedWords++;
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
				$utteranceTrueWordVowelOverSegmentations += $trueWordVowelOverSegmentation;
				$utteranceTrueWordAffixOverSegmentations += $trueWordAffixOverSegmentation;
			}
			elsif ($foundLength > $trueLength) # Check for only under-segmentation
			{
				$utteranceTrueWordUnderSegmentedWords++;					
				if (exists $determiners{$currentTrueWord})
				{
					$utteranceTrueWordDeterminerUnderSegmentations++;
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
			$previousTrueBoundary = $i;						
		} 
	}
	$trueWordCrossingBrackets += $utteranceTrueWordCrossingBrackets;
	$trueWordOverSegmentedWords += $utteranceTrueWordOverSegmentedWords;
	$trueWordUnderSegmentedWords += $utteranceTrueWordUnderSegmentedWords;
	$trueTotalWords += $utteranceTrueTotalWords;
	$trueWordDeterminerUnderSegmentations += $utteranceTrueWordDeterminerUnderSegmentations;
	$trueWordAffixOverSegmentations += $utteranceTrueWordAffixOverSegmentations;
	$trueWordVowelOverSegmentations += $utteranceTrueWordVowelOverSegmentations;
	$perfectWords += $utterancePerfectWords;
	
	if ($opt_v)
	{
		print "\n  True Words\n" .
			  "  ----------\n" .
			  "  Found just over-segmented: $utteranceTrueWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordOverSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
		print "  Found just under-segmented: $utteranceTrueWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordUnderSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
		print "  Found both over- and under-segmented: $utteranceTrueWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordCrossingBrackets / $utteranceTrueTotalWords) * 100) : 0;
	  	print "  Found with over-segmented vowels: $utteranceTrueWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordVowelOverSegmentations / $utteranceTrueTotalWords) * 100);
		print "  Found with over-segmented affixes: $utteranceTrueWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordAffixOverSegmentations / $utteranceTrueTotalWords) * 100);
		print "  Determiners that were found under-segmented: $utteranceTrueWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordDeterminerUnderSegmentations / $utteranceTrueTotalWords) * 100);
		print  "  Total: $utteranceTrueTotalWords\n";		
	}
}

sub calculateFoundWordStats
{
	my $unsegTrueLine = shift;
	my @trueArray = stringToSegArray($trueLine);
	my @foundArray = stringToSegArray($foundLine);

	my $utteranceFoundWordCrossingBrackets = 0;
	my $utteranceFoundWordUnderSegmentedWords = 0;
	my $utteranceFoundWordOverSegmentedWords = 0;
	$utteranceFoundTotalWords = 0;
	my $utteranceFoundWordDeterminerUnderSegmentations = 0;
	my $utteranceFoundWordVowelOverSegmentations = 0;
	my $utteranceFoundWordAffixOverSegmentations = 0;
		
	my $previousFoundBoundary = -1;
	my $currentFoundWord = "";
	
	my @trueWordArray = ();
	my $foundLength;
	my $trueLength;
	my $trueSpan;
	
	my $foundWordDeterminerUnderSegmentation = 0;
	
	
	# Loop through found segmentation to calculate stats with respect to found words.
	for (my $i = 0; $i < scalar(@foundArray); $i++)
	{
		# Check if this is the end/start of a new found word
		if ($foundArray[$i] == 1)
		{
			$foundLength = $i - $previousFoundBoundary;
			$currentFoundWord = substr($unsegTrueLine, $previousFoundBoundary + 1, $foundLength);
			@trueWordArray = wordsInSubUtterance($unsegTrueLine, $previousFoundBoundary + 1, $i,@trueArray);
			$trueSpan = join($wordDelimiter,@trueWordArray);
			$trueLength = length($trueSpan) - (scalar(@trueWordArray) - 1);
			if (scalar(@trueWordArray) > 1) # Check if this found word contains under-segmented true words
			{
				if ($trueLength > $foundLength) # Check if this is also over-segmented
				{
					$utteranceFoundWordCrossingBrackets++;
					if (exists $vowels{$currentFoundWord})
					{
						$utteranceFoundWordVowelOverSegmentations++;
					}
					if (exists $affixes{$currentFoundWord})
					{
						$utteranceFoundWordAffixOverSegmentations++;
					}						
				}
				else
				{
					$utteranceFoundWordUnderSegmentedWords++;					
				}
				foreach my $currentTrueWord (@trueWordArray) # Examine each under-segmented word
				{
					if (exists $determiners{$currentTrueWord})
					{
						$foundWordDeterminerUnderSegmentation = 1;
					}
				}
			}
			elsif ($trueLength > $foundLength) # Check for only over-segmentation
			{
				$utteranceFoundWordOverSegmentedWords++;					
				if (exists $vowels{$currentFoundWord})
				{
					$utteranceFoundWordVowelOverSegmentations++;
				}
				if (exists $affixes{$currentFoundWord})
				{
					$utteranceFoundWordAffixOverSegmentations++;
				}						
			}
			$utteranceFoundWordDeterminerUnderSegmentations += $foundWordDeterminerUnderSegmentation;
			$foundWordDeterminerUnderSegmentation = 0;
			$utteranceFoundTotalWords++;
			$previousFoundBoundary = $i;
		} 
	}
	$foundWordCrossingBrackets += $utteranceFoundWordCrossingBrackets;
	$foundWordOverSegmentedWords += $utteranceFoundWordOverSegmentedWords;
	$foundWordUnderSegmentedWords += $utteranceFoundWordUnderSegmentedWords;
	$foundTotalWords += $utteranceFoundTotalWords;
	$foundWordDeterminerUnderSegmentations += $utteranceFoundWordDeterminerUnderSegmentations;		
	$foundWordAffixOverSegmentations += $utteranceFoundWordAffixOverSegmentations;
	$foundWordVowelOverSegmentations += $utteranceFoundWordVowelOverSegmentations;
	
	if ($opt_v)
	{
		print "\n  Found Words\n" .
			  "  -----------\n" .
			  "  Over-segmented section of a true word: $utteranceFoundWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceFoundTotalWords > 0) ? (($utteranceFoundWordOverSegmentedWords / $utteranceFoundTotalWords) * 100) : 0;
		print "  Contained multiple whole true words: $utteranceFoundWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceFoundTotalWords > 0) ? (($utteranceFoundWordUnderSegmentedWords / $utteranceFoundTotalWords) * 100) : 0;
		print "  Contained partial true words: $utteranceFoundWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($utteranceFoundTotalWords > 0) ? (($utteranceFoundWordCrossingBrackets / $utteranceFoundTotalWords) * 100) : 0;
	  	print "  Over-segmented vowel from a true word: $utteranceFoundWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordVowelOverSegmentations / $utteranceFoundTotalWords) * 100);
		print "  Over-segmented affixes from true words: $utteranceFoundWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordAffixOverSegmentations / $utteranceFoundTotalWords) * 100);
		print "  Contained an under-segmented determiner: $utteranceFoundWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordDeterminerUnderSegmentations / $utteranceFoundTotalWords) * 100);
		print  "  Total: $utteranceFoundTotalWords\n";		
	}
}

sub printCurrentTotalResults
{
	my $utteranceNum = shift;
	die "Ignored more than length of file!\n" if ($trueTotalWords == 0);
	my $wordPrecision = ($perfectWords / $foundTotalWords);
	my $wordRecall = ($perfectWords / $trueTotalWords);
	my $wordF = (($wordPrecision + $wordRecall) > 0) ? ((2 * $wordPrecision * $wordRecall) / ($wordPrecision + $wordRecall)) : 0;
	my $wordFalseNeg = $trueTotalWords - $perfectWords;
	if (!$opt_s) # Print long error analysis by default
	{
		print "\n==============" . (($utteranceNum) ? $utteranceNum : "") . "==============\n";
		print "\nFound Words\n" .
			  "-----------\n" .
			  "Over-segmented section of a true word: $foundWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordOverSegmentedWords / $foundTotalWords) * 100) : 0;
		print "Contained multiple whole true words: $foundWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordUnderSegmentedWords / $foundTotalWords) * 100) : 0;
		print "Contained partial true words: $foundWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordCrossingBrackets / $foundTotalWords) * 100) : 0;
		print "Over-segmented vowel from a true word: $foundWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($foundWordVowelOverSegmentations / $foundTotalWords) * 100);
		print "Over-segmented affixes from true words: $foundWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($foundWordAffixOverSegmentations / $foundTotalWords) * 100);
		print "Contained an under-segmented determiner: $foundWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($foundWordDeterminerUnderSegmentations / $foundTotalWords) * 100);
		print "Total: $foundTotalWords\n";
		print "\nTrue Words\n" .
			  "----------\n" .
			  "Found just over-segmented: $trueWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordOverSegmentedWords / $trueTotalWords) * 100) : 0;
		print "Found just under-segmented: $trueWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordUnderSegmentedWords / $trueTotalWords) * 100) : 0;
		print "Found both over- and under-segmented: $trueWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordCrossingBrackets / $trueTotalWords) * 100) : 0;
		print "Found with over-segmented vowels: $trueWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($trueWordVowelOverSegmentations / $trueTotalWords) * 100);
		print "Found with over-segmented affixes: $trueWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($trueWordAffixOverSegmentations / $trueTotalWords) * 100);
		print "Determiners that were found under-segmented: $trueWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($trueWordDeterminerUnderSegmentations / $trueTotalWords) * 100);
		print "Total: $trueTotalWords\n";
		print "\nWord Stats\n" .
			  "----------\n";
		print "Correct (true pos.): $perfectWords\n" .
			  "Incorrect found words (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
			  "Missing true words (false neg.): " . ($trueTotalWords - $perfectWords) . "\n";
		printf "Precision: %1.2f\%\nRecall: %1.2f\%\nF: %1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;
	}
	else
	{
		if ($opt_b)
		{
			if ($opt_b == $lineNumber)
			{
				print "\tWP\tWR\tWF\n";
			}
			print "$lineNumber\t";
		}
		printf "%1.2f\%\t%1.2f\%\t%1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;
	}
	
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
}



