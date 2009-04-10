#!/usr/bin/perl

# Dan Blanchard
# Segmentation Error Analyzer

use strict;
use Getopt::Std;

our ($opt_d,$opt_v,$opt_i,$opt_e,$opt_b,$opt_s,$opt_t,$opt_n,$opt_c);
my $usage = "\nSegmentation Error Analyzer\n\n" . 
			"Usage: ./errors.pl [OPTIONS] TRUE_CORPUS FOUND_CORPUS\n\n" . 
			"TRUE_CORPUS = The correctly segmented reference corpus.\n" .
			"FOUND_CORPUS = The corpus you want to evaluate.\n\n" .
			"Options:\n" . 
			"\t-s\t\tShort summary mode.  Outputs word, boundary, and lexicon precision, recall, and f in a simple table format.\n" .
			"\t-v\t\tVerbose mode.  Prints results for each utterance, instead of just for the entire corpus (or block when -b is specified).\n" .
			"\t-e\t\tDumps all errors in the found corpus and their counts.\n" .
			"\t-c\t\tOutputs n-gram frequency differences between true and found corpora, with frequencies normalized to the true corpus.\n" .
			"\t-t\t\tCalculate character n-gram frequencies once per word token instead of per word type.\n" .
			"\t-n NGRAM_SIZE\tSets greatest character n-gram size to keep track of to NGRAM_SIZE. (Default = 2 for character bigrams and unigrams.)\n" .
			"\t-d DELIMITER\tSets the word delimiter character to DELIMITER.\n" .
			"\t-i NUM_LINES\tIgnores the first NUM_LINES lines in the corpora.\n" .
			"\t-b BLOCK_SIZE\tCalculates scores over blocks of size BLOCK_SIZE.  For example, -b 500 will output results for block of 500 utterances.\n\n";
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
my $foundWordFrequentCollocationUnderSegmentation = 0;

my $utterancePerfectWords = 0;
my $utteranceFoundTotalWords = 0;
my $utteranceTrueTotalWords = 0;

my @trueCharNgramCounts = ();
my @trueCharNgramTotals = ();
my @foundCharNgramCounts = ();
my @foundCharNgramTotals = ();
my $charWindowSize = 2;
my @differenceCharNgramCounts = ();

my %trueLexicon;
my %foundLexicon;

my @trueCollocations;
my @trueTotalCollocations;
my @foundCollocations;
my @foundTotalCollocations;
my $maxCollocationSize = 5;
my $frequentCollocationThreshold = 10;

my $matchedBoundaries = 0;
my $missingBoundaries = 0;
my $extraBoundaries = 0;

# Initialize Hashes
$affixes{"IN"} = $affixes{"z"} = $affixes{"s"} = $affixes{"In"} = $affixes{"~t"} = $affixes{"nt"} = $affixes{"li"} = $affixes{"6v"} = $affixes{"Id"} = $affixes{"d"} = 1;
$affixes{"In"} = 1;
$determiners{"D6"} = $determiners{"6"} = 1;
$vowels{"&"} = $vowels{"6"} = $vowels{"9"} = $vowels{"A"} = $vowels{"E"} = $vowels{"I"} = $vowels{"O"} = $vowels{"U"} = $vowels{"a"} = $vowels{"e"} = $vowels{"i"} = $vowels{"o"} = $vowels{"u"} = 1;
$vowels{"9I"} = $vowels{"9U"} = $vowels{"OI"} = 1; # diphthongs
for (my $i = 0; $i < $charWindowSize; $i++) 
{
	push @trueCharNgramTotals, 0;
	push @foundCharNgramTotals, 0;
}
for (my $i = 0; $i < $maxCollocationSize; $i++) 
{
	push @trueTotalCollocations, 0;
	push @foundTotalCollocations, 0;
}


# Handle arguments
getopts('svectn:d:i:b:');

# Check for word delimiter argument
if ($opt_d)
{
	$wordDelimiter = $opt_d;
}

if ($opt_n)
{
	$charWindowSize = $opt_n;
}

die $usage if @ARGV < 2;

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
			updateCharNgramStats($currentTrueWord,\@trueCharNgramCounts,\@trueCharNgramTotals,\%trueLexicon);
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
		print "\n\n  True Words\n" .
			  "  ----------\n" .
			  "  Found just over-segmented: $utteranceTrueWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordOverSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
		print "  Found just under-segmented: $utteranceTrueWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordUnderSegmentedWords / $utteranceTrueTotalWords) * 100) : 0;
		print "  Found both over- and under-segmented: $utteranceTrueWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($utteranceTrueTotalWords > 0) ? (($utteranceTrueWordCrossingBrackets / $utteranceTrueTotalWords) * 100) : 0;
	  	print "\n  Found with over-segmented vowels: $utteranceTrueWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordVowelOverSegmentations / $utteranceTrueTotalWords) * 100);
		print "  Found with over-segmented affixes: $utteranceTrueWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordAffixOverSegmentations / $utteranceTrueTotalWords) * 100);
		print "  Determiners that were found under-segmented: $utteranceTrueWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($utteranceTrueWordDeterminerUnderSegmentations / $utteranceTrueTotalWords) * 100);
		print  "\n  Total: $utteranceTrueTotalWords\n";		
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
	my $utteranceFoundWordFrequentCollocationUnderSegmentation = 0;
	my $previousFoundBoundary = -1;
	my $currentFoundWord = "";
	
	my @trueWordArray = ();
	my $foundLength;
	my $trueLength;
	my $trueSpan;
	
	my $currentWordDeterminerUnderSegmentation = 0;
	my $currentWordFrequentCollocationUnderSegmentation = 0;
	my $collocation;
	
	# Loop through found segmentation to calculate stats with respect to found words.
	for (my $i = 0; $i < scalar(@foundArray); $i++)
	{
		# Check if this is the end/start of a new found word
		if ($foundArray[$i] == 1)
		{
			$foundLength = $i - $previousFoundBoundary;
			$currentFoundWord = substr($unsegTrueLine, $previousFoundBoundary + 1, $foundLength);
			updateCharNgramStats($currentFoundWord,\@foundCharNgramCounts,\@foundCharNgramTotals,\%foundLexicon);
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
						$currentWordDeterminerUnderSegmentation = 1;
					}
				}
				for (my $currentWindowSize = 1; (($currentWindowSize < $maxCollocationSize) && ($currentWindowSize <= scalar(@trueWordArray))); $currentWindowSize++) 
				{
					for (my $currentIndex = 0; $currentIndex < scalar(@trueWordArray) - $currentWindowSize; $currentIndex++) 
					{
						$collocation = join($wordDelimiter, @trueWordArray[$currentIndex..($currentIndex+$currentWindowSize)]);
						# if (($trueCollocations[$currentWindowSize]{$collocation} / ($trueTotalCollocations[$currentWindowSize] + 1)) >= ((1/(scalar(keys %trueLexicon) + 1))**($currentWindowSize + 1)))
						if ($trueCollocations[$currentWindowSize]{$collocation} >= $frequentCollocationThreshold)
						{
							$currentWordFrequentCollocationUnderSegmentation = 1;
						}
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
			$utteranceFoundWordDeterminerUnderSegmentations += $currentWordDeterminerUnderSegmentation;
			$utteranceFoundWordFrequentCollocationUnderSegmentation += $currentWordFrequentCollocationUnderSegmentation;
			$currentWordDeterminerUnderSegmentation = 0;
			$currentWordFrequentCollocationUnderSegmentation = 0;
			$utteranceFoundTotalWords++;
			$previousFoundBoundary = $i;
		} 
	}
	$foundWordCrossingBrackets += $utteranceFoundWordCrossingBrackets;
	$foundWordOverSegmentedWords += $utteranceFoundWordOverSegmentedWords;
	$foundWordUnderSegmentedWords += $utteranceFoundWordUnderSegmentedWords;
	$foundTotalWords += $utteranceFoundTotalWords;
	$foundWordDeterminerUnderSegmentations += $utteranceFoundWordDeterminerUnderSegmentations;
	$foundWordFrequentCollocationUnderSegmentation += $utteranceFoundWordFrequentCollocationUnderSegmentation;
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
	  	print "  \nOver-segmented vowel from a true word: $utteranceFoundWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordVowelOverSegmentations / $utteranceFoundTotalWords) * 100);
		print "  Over-segmented affixes from true words: $utteranceFoundWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordAffixOverSegmentations / $utteranceFoundTotalWords) * 100);
		print "  Contained an under-segmented determiner: $utteranceFoundWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($utteranceFoundWordDeterminerUnderSegmentations / $utteranceFoundTotalWords) * 100);
		print "  Contained an under-segmented collocation: $utteranceFoundWordFrequentCollocationUnderSegmentation (";
		printf "%1.2f\%)\n", (($utteranceFoundWordFrequentCollocationUnderSegmentation / $utteranceFoundTotalWords) * 100);
		print  "  \nTotal: $utteranceFoundTotalWords\n";		
	}
}

sub calculateBoundaryStats
{
	my @trueArray = stringToSegArray($trueLine);
	my @foundArray = stringToSegArray($foundLine);
	my $truePos = 0;
	my $falsePos = 0;
	my $falseNeg = 0;
	my $prec = 0;
	my $recall = 0;
	for (my $i = 0; $i < (scalar(@trueArray) - 1); $i++) # - 1 to miss extra boundary on right end
	{
		if ($foundArray[$i] == 1)
		{
			if ($trueArray[$i] == 1)
			{
				$truePos++;
			}
			else
			{
				$falsePos++;
			}
		}
		elsif ($trueArray[$i] == 1)
		{
			$falseNeg++;
		}
	}
	
	$missingBoundaries += $falseNeg;
	$matchedBoundaries += $truePos;
	$extraBoundaries += $falsePos;
	$prec = (($truePos + $falsePos) > 0) ? ($truePos / ($truePos + $falsePos)) : 0;
	$recall = (($truePos + $falseNeg) > 0) ? ($truePos / ($truePos + $falseNeg)) : 0;
	if ($opt_v)
	{
		print "\n  Boundary Stats\n" .
			  "  --------------\n";
		printf "  Precision: %1.2f\%\n  Recall: %1.2f\%\n  F: %1.2f\%\n", $prec*100, $recall*100, ((($prec + $recall) > 0) ? ((2 * $prec * $recall) / ($prec + $recall)) : 0)*100;	
	}
}

sub updateCharNgramStats
{
	my $word = shift;
	my $wordPlusBoundaries = $wordDelimiter . $word . $wordDelimiter;
	my $ngramCountArrayRef = shift;
	my $ngramCountTotalArrayRef = shift;
	my $lexiconRef = shift;
	my $ngram;
	if (!(exists $$lexiconRef{$word}) || ($opt_t))
	{	
		for (my $currentWindowSize = 0; $currentWindowSize < $charWindowSize; $currentWindowSize++) 
		{
			for (my $currentIndex = 0; $currentIndex < length($wordPlusBoundaries) - $currentWindowSize; $currentIndex++) 
			{
				$ngram = substr($wordPlusBoundaries, $currentIndex, ($currentWindowSize + 1));
				if (exists $$ngramCountArrayRef[$currentWindowSize]{$ngram})
				{
					$$ngramCountArrayRef[$currentWindowSize]{$ngram}++;
				}
				else
				{
					$$ngramCountArrayRef[$currentWindowSize]{$ngram} = 1;
				}
				$$ngramCountTotalArrayRef[$currentWindowSize]++;
			}
		}
	}
	if (exists $$lexiconRef{$word})
	{
		$$lexiconRef{$word}++;
	}
	else
	{
		$$lexiconRef{$word} = 1;
	}
}

sub updateWordCollocationStats
{
	my $utterance = shift;
	$utterance =~ s/^\Q$wordDelimiter\E*(.+[^\Q$wordDelimiter\E])\Q$wordDelimiter\E*/\1/;
	my @words = split /\Q$wordDelimiter\E/,$utterance;
	my $collocationsRef = shift;
	my $collocationTotalsRef = shift;
	my $collocation;
	
	# Collocations of length 1 are just lexical entries, so skip those.
	for (my $currentWindowSize = 1; $currentWindowSize < $maxCollocationSize; $currentWindowSize++) 
	{
		for (my $currentIndex = 0; $currentIndex < scalar(@words) - $currentWindowSize; $currentIndex++) 
		{
			$collocation = $words[$currentIndex] . $wordDelimiter . $words[$currentIndex + 1];
			if (exists $$collocationsRef[$currentWindowSize]{$collocation})
			{
				$$collocationsRef[$currentWindowSize]{$collocation}++;
			}
			else
			{
				$$collocationsRef[$currentWindowSize]{$collocation} = 1;
			}
			$$collocationTotalsRef[$currentWindowSize]++;
		}
	}
}

sub normalizeCharNgramCounts
{
	my $ngramCountArrayRef = shift;
	my $ngramCountTotalArrayRef = shift;

	for (my $currentWindowSize = 0; $currentWindowSize < $charWindowSize; $currentWindowSize++) 
	{
		foreach my $ngram (keys %{$$ngramCountArrayRef[$currentWindowSize]})
		{
			$$ngramCountArrayRef[$currentWindowSize]{$ngram} /= $$ngramCountTotalArrayRef[$currentWindowSize];
		}
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
	my $matched = 0;
	foreach my $word (keys %foundLexicon)
	{
		if (exists $trueLexicon{$word})
		{
			$matched++;
		}
	}
	
	my $lexiconPrecision = $matched / scalar(keys %foundLexicon);
	my $lexiconRecall = $matched / scalar(keys %trueLexicon);
	my $lexiconF = (($lexiconPrecision + $lexiconRecall) > 0) ? ((2 * $lexiconPrecision * $lexiconRecall) / ($lexiconPrecision + $lexiconRecall)) : 0;
	
	my $boundaryPrecision = $matchedBoundaries / ($matchedBoundaries + $extraBoundaries);
	my $boundaryRecall = $matchedBoundaries / ($matchedBoundaries + $missingBoundaries);
	my $boundaryF = (($boundaryPrecision + $boundaryRecall) > 0) ? ((2 * $boundaryPrecision * $boundaryRecall) / ($boundaryPrecision + $boundaryRecall)) : 0;
	if (!$opt_s) # Print long error analysis by default
	{
		if ($utteranceNum)
		{
			print "\n==============" . $utteranceNum . "==============\n";			
		}
		else
		{
			print "============================\n";			
		}
		print "\nFound Words\n" .
			  "-----------\n" .
			  "Over-segmented section of a true word: $foundWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordOverSegmentedWords / $foundTotalWords) * 100) : 0;
		print "Contained multiple whole true words: $foundWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordUnderSegmentedWords / $foundTotalWords) * 100) : 0;
		print "Contained partial true words: $foundWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($foundTotalWords > 0) ? (($foundWordCrossingBrackets / $foundTotalWords) * 100) : 0;
		print "\nOver-segmented vowel from a true word: $foundWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($foundWordVowelOverSegmentations / $foundTotalWords) * 100);
		print "Over-segmented affixes from true words: $foundWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($foundWordAffixOverSegmentations / $foundTotalWords) * 100);
		print "Contained an under-segmented determiner: $foundWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($foundWordDeterminerUnderSegmentations / $foundTotalWords) * 100);
		print "Contained an under-segmented collocation: $foundWordFrequentCollocationUnderSegmentation (";
		printf "%1.2f\%)\n", (($foundWordFrequentCollocationUnderSegmentation / $foundTotalWords) * 100);
		print "\nTotal: $foundTotalWords\n";
		print "\n\nTrue Words\n" .
			  "----------\n" .
			  "Found just over-segmented: $trueWordOverSegmentedWords (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordOverSegmentedWords / $trueTotalWords) * 100) : 0;
		print "Found just under-segmented: $trueWordUnderSegmentedWords (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordUnderSegmentedWords / $trueTotalWords) * 100) : 0;
		print "Found both over- and under-segmented: $trueWordCrossingBrackets (";
		printf "%1.2f\%)\n", ($trueTotalWords > 0) ? (($trueWordCrossingBrackets / $trueTotalWords) * 100) : 0;
		print "\nFound with over-segmented vowels: $trueWordVowelOverSegmentations (";
		printf "%1.2f\%)\n", (($trueWordVowelOverSegmentations / $trueTotalWords) * 100);
		print "Found with over-segmented affixes: $trueWordAffixOverSegmentations (";
		printf "%1.2f\%)\n", (($trueWordAffixOverSegmentations / $trueTotalWords) * 100);
		print "Determiners that were found under-segmented: $trueWordDeterminerUnderSegmentations (";
		printf "%1.2f\%)\n", (($trueWordDeterminerUnderSegmentations / $trueTotalWords) * 100);
		print "\nTotal: $trueTotalWords\n";
		print "\n\nWord Stats\n" .
			  "----------\n";
		print "Correct (true pos.): $perfectWords\n" .
			  "Incorrect found words (false pos.): " . ($foundTotalWords - $perfectWords) . "\n" .
			  "Missing true words (false neg.): " . ($trueTotalWords - $perfectWords) . "\n";
		printf "Precision: %1.2f\%\nRecall: %1.2f\%\nF: %1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100;
		print "\n\nBoundary Stats\n" .
			  "--------------\n";
		printf "Precision: %1.2f\%\nRecall: %1.2f\%\nF: %1.2f\%\n", $boundaryPrecision*100, $boundaryRecall*100, $boundaryF*100;
		print "\n\nLexicon Stats\n" .
			  "-------------\n";
		printf "Precision: %1.2f\%\nRecall: %1.2f\%\nF: %1.2f\%\n", $lexiconPrecision*100, $lexiconRecall*100, $lexiconF*100;
	}
	else
	{
		if ($opt_b)
		{
			if ($opt_b + $opt_i == $lineNumber)
			{
				print "block\tWP\tWR\tWF\tBP\tBR\tBF\tLP\tLR\tLF\n";
			}
			print "$lineNumber\t";
		}
		printf "%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\t%1.2f\%\n", $wordPrecision*100, $wordRecall*100, $wordF*100, $boundaryPrecision*100, $boundaryRecall*100, $boundaryF*100, $lexiconPrecision*100, $lexiconRecall*100, $lexiconF*100;
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
	
	if ($opt_c) # Print character n-gram differences, if specified
	{
		normalizeCharNgramCounts(\@foundCharNgramCounts,\@foundCharNgramTotals);
		normalizeCharNgramCounts(\@trueCharNgramCounts,\@trueCharNgramTotals);
		
		# Normalize frequencies, subtract, and then multiple by true n-gram count totals
		for (my $currentWindowSize = 0; $currentWindowSize < $charWindowSize; $currentWindowSize++) 
		{
			foreach my $ngram (keys %{$trueCharNgramCounts[$currentWindowSize]})
			{
				$differenceCharNgramCounts[$currentWindowSize]{$ngram} = $trueCharNgramCounts[$currentWindowSize]{$ngram} - $foundCharNgramCounts[$currentWindowSize]{$ngram};
				$differenceCharNgramCounts[$currentWindowSize]{$ngram} *= $trueCharNgramTotals[$currentWindowSize];
			}
		}

		for (my $currentWindowSize = 0; $currentWindowSize < $charWindowSize; $currentWindowSize++) 
		{
			print "\nCharacter N-Gram Differences for N = " . ($currentWindowSize + 1) . "\n" .
				  "--------------------------------------\n";
			foreach my $key (sort {abs($differenceCharNgramCounts[$currentWindowSize]{$a}) <=> abs($differenceCharNgramCounts[$currentWindowSize]{$b})}(keys %{$differenceCharNgramCounts[$currentWindowSize]}))
			{
				print "$key\t$differenceCharNgramCounts[$currentWindowSize]{$key}\n";
			}
		}
	}
}

##### MAIN PROGRAM START #####
print "\n\nResults for $ARGV[1]:\n";
if ($opt_i)
{
	print "(ignoring first $opt_i utterances)\n";
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
	
	updateWordCollocationStats($trueLine,\@trueCollocations,\@trueTotalCollocations);
	updateWordCollocationStats($foundLine,\@foundCollocations,\@foundTotalCollocations);
	
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
		
		calculateBoundaryStats();
		
		# Reset any global counters when reaching the end of a block.
		# (As this list is getting rather unwiedly, should probably refactor this to not have global variables.)
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
			%overSegmentedWordFrequencies = ();
			%underSegmentedWordFrequencies = ();
			%bothErrorsWordFrequencies = ();           
			$trueWordDeterminerUnderSegmentations = 0;
			$trueWordVowelOverSegmentations = 0;
			$trueWordAffixOverSegmentations = 0;
			$foundWordDeterminerUnderSegmentations = 0;
			$foundWordVowelOverSegmentations = 0;
			$foundWordAffixOverSegmentations = 0;
			%trueLexicon = ();
			%foundLexicon = ();
			@trueCharNgramTotals = ();
			@foundCharNgramTotals = ();
			for (my $i = 0; $i < $charWindowSize; $i++) 
			{
				push @trueCharNgramTotals, 0;
				push @foundCharNgramTotals, 0;
			}
			@trueCharNgramCounts = ();
			@foundCharNgramCounts = ();
			@differenceCharNgramCounts = ();
			@trueCollocations = ();
			@foundCollocations = ();
			@trueTotalCollocations = ();
			@foundTotalCollocations = ();
			$matchedBoundaries = 0;
			$missingBoundaries = 0;
			$extraBoundaries = 0;
			$foundWordFrequentCollocationUnderSegmentation = 0;
			for (my $i = 0; $i < $maxCollocationSize; $i++) 
			{
				push @trueTotalCollocations, 0;
				push @foundTotalCollocations, 0;
			}
			
		}
	}
}
if (!$opt_b && $opt_s)
{
	print "WP\tWR\tWF\tBP\tBR\tBF\tLP\tLR\tLF\n";
}
printCurrentTotalResults($opt_b ? $lineNumber : 0); 

