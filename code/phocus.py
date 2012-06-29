#!/usr/bin/env python

'''
    PHOCUS: PHOnotactic CUe Segmenter
    Copyright (C) 2007-2012 Dan Blanchard.

    This file is part of PHOCUS.

    PHOCUS is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PHOCUS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.
'''

import argparse
import copy
import cmd
import os
import readline
import sys
from abc import ABCMeta, abstractmethod
from fractions import Fraction
from functools import partial
from itertools import chain, product, imap, ifilter
from operator import mul

import regex as re
from probability import FreqDist, LidstoneProbDist, ConditionalFreqDist, ConditionalProbDist
from ngram import NgramModel
from nltk.util import ingrams
from featurechart import PhonologicalFeatureChartReader


class PartialCountNgramModel(NgramModel):
    def __init__(self, n, train, estimator, freqtype, *estimator_args, **estimator_kw_args):
        """
        Creates an ngram language model to capture patterns in n consecutive
        words of training text.  An estimator smooths the probabilities derived
        from the text and may allow generation of ngrams not seen during
        training.

        @param n: the order of the language model (ngram size)
        @type n: L{int}
        @param train: the training text
        @type train: L{list} of L{str} (or L{list} of L{str} L{list}s)
        @param estimator: a function for generating a probability distribution (must take FreqDist as first argument, and n as second)
        @type estimator: a function that takes a L{ConditionalFreqDist} and
              returns a L{ConditionalProbDist}
        @param freqtype: the type to use to store the counts in the underlying frequency distribution
        @type freqtype: any numeric type
        @param estimator_args: Extra arguments for L{estimator}.
            These arguments are usually used to specify extra
            properties for the probability distributions of individual
            conditions, such as the number of bins they contain.
        @type estimator_args: (any)
        @param estimator_kw_args: Extra keyword arguments for L{estimator}.
        @type estimator_kw_args: (any)
        """

        self._n = n

        cfd = ConditionalFreqDist(counttype=freqtype)
        self._ngrams = set()
        self._padding = ('',) * (n - 1)
        self._estimator = estimator
        self._freqtype = freqtype
        self._estimator_args = estimator_args
        self._estimator_kw_args = estimator_kw_args

        if train:
            for utterance in train:
                for ngram in ingrams(chain(self._padding, utterance, self._padding), n):
                    self._ngrams.add(ngram)
                    context = tuple(ngram[:-1])
                    token = ngram[-1]
                    cfd[context].inc(token)

        self._model = ConditionalProbDist(cfd, estimator, self._freqtype, n, *estimator_args, **estimator_kw_args)

        # recursively construct the lower-order models
        if n > 1:
            self._backoff = PartialCountNgramModel(n - 1, train, estimator, freqtype, *estimator_args, **estimator_kw_args)

    def update(self, samples, increase_amount=1):
        cond_samples = []
        for utterance in samples:
            for ngram in ingrams(chain(self._padding, utterance, self._padding), self._n):
                self._ngrams.add(ngram)
                cond_samples.append((tuple(ngram[:-1]), ngram[-1]))
        self._model.update(cond_samples, increase_amount)

        # Recursively update lower-order models
        if self._n > 1:
            self._backoff.update(samples, increase_amount)


class Cue(object):
    """ Parent class for all PHOCUS Cues/Features """

    __metaclass__ = ABCMeta

    def __init__(self, initial_count, score_combiner=lambda scores: reduce(mul, scores), subseq_counts=None):
        '''
            Initializes any counts to their default values, if necessary
            @param initial_count: the count to assign to unseen instances of this cues
            @type initial_count: L{Fraction}
            @param score_combiner: Function to combine sub-word scores in eval_word
            @type score_combiner: C{function}
            @param subseq_counts: A frequency distribution for storing subsequence counts. Should use the same one for all L{Cues}
                                  of the current L{Segmenter}.
            @type subseq_counts: L{Fraction}
        '''
        self._phonotactic = True
        self._initial_count = initial_count
        self._score_combiner = score_combiner
        self._subseq_counts = subseq_counts

    @property
    def phonotactic(self):
        ''' Whether or not this Cue represents a phonotactic/sub-word feature. '''
        return self._phonotactic

    @property
    def subseq_counts(self):
        return self._subseq_counts

    @abstractmethod
    def eval_word(self, word):
        '''
            Returns probability that proposed word is a word.
        '''
        pass

    @abstractmethod
    def update_evidence(self, word, increase_amount):
        pass

    @abstractmethod
    def dump(self, dump_file):
        pass

    @abstractmethod
    def use_score(self, word):
        '''
            Determines whether or not the score for the current word should be used (or if we should back-off if that's what the
            segmenter's evidence combiner does.)
        '''
        pass

    @property
    def initial_count(self):
        return self._initial_count


class FamiliarWordCue(Cue):
    '''
        Feature that scores words based on their lexical frequency.
    '''

    def __init__(self, mbdp=False, subseq_counts=None, type_denom=False, bad_score=0):
        '''
            Initializes any counts to their default values, if necessary
            @param mbdp: Use MBDP-1 score adjustments when calculating word scores.
            @type mbdp: L{bool}
            @param subseq_counts: A frequency distribution for storing subsequence counts. Should use the same one for all L{Cues}
                                  of the current L{Segmenter}.
            @type subseq_counts: L{FreqDist}
            @param type_denom: When evaluating familiar words, divide word count by total number of word tokens + total number of word types.
                               Should only be on for Venkataraman.
            @type type_denom: L{bool}
        '''
        super(FamiliarWordCue, self).__init__(Fraction(0), subseq_counts=subseq_counts)
        self._phonotactic = False
        self._lexicon = FreqDist(counttype=Fraction)
        self._mbdp = mbdp
        self._type_denom = type_denom
        self._bad_score = bad_score

    def in_lexicon(self, word):
        return word in self._lexicon

    @property
    def total_words(self):
        return self._lexicon.N()

    def eval_word(self, word):
        '''
            Returns probability that proposed word is a word.
            @todo: Implement lexical decay.
        '''
        if word in self._lexicon:
            word_count = Fraction(self._lexicon[word])
            if not self._mbdp:
                word_types = Fraction(self._lexicon.B() - 1)  # Subtract one for initial utterance delimiter addition
                raw_score = word_count / (Fraction(self.subseq_counts[word]) if self.subseq_counts
                                                                        else (Fraction(self.total_words + word_types) if self._type_denom
                                                                                                                      else Fraction(self.total_words)))
            else:
                raw_score = ((word_count + Fraction(1)) / (self.total_words + Fraction(1))) * (((word_count / (word_count + Fraction(1))) ** Fraction(2)))
        else:
            raw_score = self._bad_score
        return raw_score  # lexical decay stuff would need to be added here

    def dump(self, dump_file):
        for word in self._lexicon.iterkeys():
            dump_file.write(word + str(self._lexicon[word]) + '\n')
        if self._subseq_counts:
            for seq in self.subseq_counts.iterkeys():
                dump_file.write(word + str(self.subseq_counts[seq]) + '\n')
        dump_file.close()

    def use_score(self, word):
        return (not args.noLexicon) and (self.in_lexicon(word))

    def update_evidence(self, word, increase_amount):
        self._lexicon.inc(word, increase_amount)


class NgramCue(Cue):
    '''
        Feature that scores words based on their constituent n-grams.
    '''

    def __init__(self, n, initial_count, num_unigrams, hypothetical_phonotactics=False, score_combiner=lambda scores: reduce(mul, scores),
                 subseq_counts=None):
        '''
            @param n: the order of the language model (ngram size)
            @type n: L{int}
            @param initial_count: the initial count used for unseen n-grams
            @type initial_count: L{Fraction}
            @param num_unigrams: the number of possible unigrams types for the current cue
            @type num_unigrams: L{int}
            @param hypothetical_phonotactics: should n-gram counts be temporarily updated to include hypothetical word's n-grams?
            @type hypothetical_phonotactics: L{bool}
            @param score_combiner: Function to combine scores in eval_word
            @type score_combiner: C{function}
            @param subseq_counts: A frequency distribution for storing subsequence counts. Should use the same one for all L{Cues}
                                  of the current L{Segmenter}.
            @type subseq_counts: L{FreqDist}
        '''
        super(NgramCue, self).__init__(initial_count, score_combiner)

        self._n = n
        self._ngram_model = PartialCountNgramModel(n, None, lambda freqdist, n, initial_count, bins:
                                                            LidstoneProbDist(freqdist, initial_count, bins ** n),
                                                   Fraction, initial_count, num_unigrams)
        self._hypothetical_phonotactics = hypothetical_phonotactics

    def eval_word(self, word):
        '''
            Returns probability that proposed word is a word.
        '''
        global subseq_counts

        if self._hypothetical_phonotactics:
            # Make a deep copy of the ngram model so that we can update it without modifying the original
            word_ngram_model = copy.deepcopy(self._ngram_model)
            word_ngram_model.update([word])
        else:
            word_ngram_model = self._ngram_model

        # If I want to duplicate functionality of OCaml code, should probably make a ProbDist that inherits from Lidstone,
        # otherwise this won't do the denominator adjustments and other stuff.
        return self._score_combiner([self._ngram_model.prob(tuple(ngram[:-1]), ngram[-1]) for ngram in
                                     ingrams(chain(self._ngram_model._padding, word, self._ngram_model._padding), self._n)])

    def update_evidence(self, word, increase_amount):
        # Used to have token phonotactics check here in OCaml code, but that really breaks the separation of the different cues.
        # I'll take care of checking that the word isn't in the lexicon (or token phonotactics has been specified) outside of this class.
        self._ngram_model.update([word])
        # print self._ngram_model

    def dump(self, dump_file):
        '''
            @todo: Implement this function.
        '''
        raise NotImplementedError

    @abstractmethod
    def use_score(self, word):
        pass


class SyllableNgramCue(NgramCue):
    """ NgramCue that calculates probabilities for sequences of syllables."""
    def __init__(self, n, initial_count, num_unigrams, feature_chart, hypothetical_phonotactics=False, score_combiner=lambda scores: reduce(mul, scores), subseq_counts=None):
        super(SyllableNgramCue, self).__init__(n, initial_count, num_unigrams, hypothetical_phonotactics, score_combiner, subseq_counts)
        self._vowels = feature_chart.phones_for_features("+syllabic")
        self._vowel_re = re.compile("[" + ''.join([re.escape(vowel) for vowel in self._vowels if len(vowel) == 1]) + "]")
        self._diphthongs = set([vowel for vowel in self._vowels if len(vowel) > 1])

    def syllabify(self, word):
        syllables = None
        if self._vowel_re.search(word):
            syllables = list(word)
            # Find all the diphthongs first
            i = 0
            while i < len(syllables) - 1:
                if ''.join(syllables[i:i + 2]) in self._diphthongs:
                    syllables[i] = ''.join(syllables[i:i + 2])
                    del syllables[i + 1]
                i += 1

            # Keep adding onsets and codas one at a time (onsets before codas) until we can't
            prev_list = None
            while prev_list != syllables:
                prev_list = syllables[:]
                # Onsets
                num_removed = 0
                for i in range(1, len(syllables)):
                    adjusted_index = i - num_removed
                    if self._vowel_re.search(syllables[adjusted_index]) and not self._vowel_re.search(syllables[adjusted_index - 1]):
                        syllables[adjusted_index] = ''.join(syllables[adjusted_index - 1: adjusted_index + 1])
                        del syllables[adjusted_index - 1]
                        num_removed += 1
                # Codas
                num_inserted = 0
                for i in range(0, len(syllables) - 1):
                    adjusted_index = i + num_inserted
                    if adjusted_index < len(syllables) - 1:
                        if self._vowel_re.search(syllables[adjusted_index]) and not self._vowel_re.search(syllables[adjusted_index + 1]):
                            syllables[adjusted_index] = ''.join(syllables[adjusted_index: adjusted_index + 2])
                            del syllables[adjusted_index + 1]
                            num_inserted += 1
        return syllables

    def use_score(self, word):
        return bool(self.syllabify(word))


class PhonemeNgramCue(NgramCue):
    """ NgramCue that calculates probabilities for sequences of phonemes."""

    def use_score(self, word):
        '''
            Determines whether or not the score for the current word should be used (or if we should back-off if that's what the combiner does.)
            @todo: Implement this function.
        '''
        return True


class Segmenter(cmd.Cmd, object):
    """ A PHOCUS word segmentation model"""

    def __init__(self, cues, evidence_combiner, feature_chart, search_func=None, word_delimiter=' ', utterance_limit=0, supervised=0,
                 wait_until_utterance=0, wait_for_stable_phoneme_dist=False, output_channel=sys.stdout, semi_supervised_updating=False,
                 uniform_phonotactics=False, display_line_numbers=False, print_utterance_delimiter=False, utterance_delimiter='$'):
        '''
        Initializes a PHOCUS word segmentation model.

        @param cues: The Cues to use when segmenting in order of precedence.
        @type cues: L{list} of L{Cue}s
        @param evidence_combiner: Function to determine how scores from cues are combined (actually calls L{Cue.eval_word} for each L{Cue}).
        @type evidence_combiner: C{function} that takes two arguments: a list of L{Cue}s and a word
        @param feature_chart: The phonological feature chart for the corpus.
        @type feature_chart: L{PhonologicalFeatureChartReader}
        @param word_delimiter: The word delimiter that may be currently separating words in the sentence. (They will all be removed before segmenting.)
        @type word_delimiter: L{basestring}
        '''
        super(Segmenter, self).__init__()
        self.cues = cues
        self.evidence_combiner = evidence_combiner
        self.word_delimiter = word_delimiter
        self.eval_word = partial(evidence_combiner, self.cues)
        self.subseq_counts = FreqDist(counttype=Fraction)
        self.feature_chart = feature_chart
        self.search_func = self.viterbi_segmentation_search if not search_func else search_func
        self.utterance_limit = utterance_limit
        self.supervised = supervised
        self.wait_until_utterance = wait_until_utterance
        self.wait_for_stable_phoneme_dist = wait_for_stable_phoneme_dist
        self.output_channel = output_channel
        self.semi_supervised_updating = semi_supervised_updating
        self.uniform_phonotactics = uniform_phonotactics
        self.display_line_numbers = display_line_numbers
        self.print_utterance_delimiter = print_utterance_delimiter
        self.utterance_delimiter = utterance_delimiter

    def dump(self, prefix):
        '''
            Dump all of the data in all of the cues to separate files that start with prefix and end with the cue name.

            @param prefix: The prefix to put at the front of the file names.
            @type prefix: L{basestring}
        '''
        for cue in self.cues:
            cue.dump(prefix + type(cue).__name__)

    def viterbi_segmentation_search(self, sentence):
        '''
            Viterbi-style search, as outlined by Brent (1999), for finding the best segmentation of a given sentence.

            @param sentence: The sentence to segment
            @type sentence: L{basestring}

            @return: Pairs of weights and segmentations (in this case all weights will be 1).
            @rtype: 2-L{tuple} of L{Fraction}s and L{int} L{list}s
        '''
        sentence = sentence.replace(self.word_delimiter, '')
        best_products = []
        best_starts = []
        # Fill best_starts list
        for last_char in range(len(sentence)):
            best_products.append(self.eval_word(sentence[0:last_char + 1]))
            best_starts.append(0)
            # After loop, best_starts[last_char] points to beginning of the optimal word ending with last_char
            # best_products[last_char] contains the actual score for the optimal word
            for first_char in range(1, last_char + 1):
                word_score = self.eval_word(sentence[first_char:last_char + 1])
                new_score_product = word_score * best_products[first_char - 1]
                # print >> sys.stderr, "scoreProduct: {}\tlastCharBestProduct: {}".format(float(new_score_product), float(best_products[last_char]))
                if new_score_product > best_products[last_char]:
                    best_products[last_char] = new_score_product
                    best_starts[last_char] = first_char

        # print "Best products: {}\t Best starts: {}".format(best_products, best_starts)

        # Extract segmentation from best_starts list
        segmentation = [False] * len(sentence)
        segmentation[-1] = True
        first_char = best_starts[-1]
        while first_char > 0:
            segmentation[first_char] = True
            first_char = best_starts[first_char - 1]

        return [(Fraction(1), segmentation)]

    def evidence_updater(self, increment_amount, segmentation, sentence, cue_selector=lambda cue: True):
        '''
            Calls update_evidence functions for all Cues in self.cues that evalute True for cue_selector,
            and prints out words to self.output_channel if the increment_amount for this segmentation is 1.
            @todo: Add stuff to take care of subsequence counts
        '''
        last = 0
        for i in xrange(len(segmentation)):
            if segmentation[i]:
                word = sentence[last:i + 1]
                last = i + 1
                for cue in ifilter(cue_selector, self.cues):
                    cue.update_evidence(word, increment_amount)
                if increment_amount == 1 and sentence != self.utterance_delimiter:
                    self.output_channel.write(word)
                    if last < len(segmentation):
                        self.output_channel.write(self.word_delimiter)
        # Commit subsequence counts here...

    def incremental_processor(self, utterance_list):
        for i, segmented_sentence in enumerate(utterance_list):
            # Get scored segmentations
            if not self.utterance_limit or i < self.utterance_limit:
                sentence = segmented_sentence.replace(self.word_delimiter, '').rstrip('\n')
                if self.supervised <= i and i + 1 > self.wait_until_utterance:  # and (not self.wait_for_stable_phoneme_dist or PhonemeNgramCue.use_score(sentence)):
                    scored_segmentations = self.search_func(sentence)
                else:
                    scored_segmentations = [(Fraction(1), segmentation_list_for_segmented_sentence(segmented_sentence))]
                if self.display_line_numbers:
                    self.output_channel.write("{}: ".format(i + 1))
                # Update evidence
                if not self.supervised or self.supervised > i or self.semi_supervised_updating:
                    for score, segmentation in scored_segmentations:
                        if self.uniform_phonotactics:
                            self.evidence_updater(score, segmentation, sentence, lambda cue: not cue.phonotactic)
                        else:
                            self.evidence_updater(score, segmentation, sentence)
                else:
                    for score, segmentation in scored_segmentations:
                        self.print_segmented_utterance(score, segmentation, sentence)
                # Print line-final stuff
                if self.print_utterance_delimiter:
                    self.output_channel.write(self.utterance_delimiter)
                self.output_channel.write(u'\n')
                self.output_channel.flush()
                # Update count of utterance_delimiter in lexicon
                self.evidence_updater(Fraction(1), [True], self.utterance_delimiter, lambda cue: isinstance(cue, FamiliarWordCue))
            else:
                break

    ### Interactive mode functions ####
    def do_score(self, words):
        ''' Returns the score for the given words given the current cues. '''
        for word in words.split():
            backoff_combiner(self.cues, word, verbose=True)

    def do_add(self, arg_string):
        ''' Increases the frequencies of the given words by the given amount. '''
        arg_list = arg_string.split()
        increase_amount = arg_list.pop(0)
        sentence = ' '.join(arg_list)
        self.evidence_updater(Fraction(increase_amount), segmentation_list_for_segmented_sentence(sentence), sentence)

    def do_syllabify(self, words):
        ''' Breaks up the given words into syllables. '''
        for word in words.split():
            print SyllableNgramCue(1, 0, 10, self.feature_chart).syllabify(word)

    def do_exit(self, s):
        ''' Exit the program. '''
        return True

    do_EOF = do_exit


def backoff_combiner(cues, word, verbose=False):
    '''
    Calculates the scores for all the cues in the given list and then chooses the first one where cue.use_score(word) is True.
    '''
    scores = [cue.eval_word(word) for cue in cues]
    if verbose:
        print >> sys.stderr, '\n'.join(["{} score for {} = {}".format(type(cue).__name__, word, scores[i]) for i, cue in enumerate(cues)])
        sys.stderr.flush()
    for i, cue in enumerate(cues):
        if cue.use_score(word):
            return scores[i]


def weighted_sum_combiner(cues, word, weights=None, verbose=False):
    scores = [cue.eval_word(word) for cue in cues]
    if verbose:
        print >> sys.stderr, '\n'.join(["{} score for {} = {}".format(type(cue).__name__, word, scores[i]) for i, cue in enumerate(cues)])
        sys.stderr.flush()
    if not weights:
        num_cues = len(cues)
        weights = [Fraction('1/{}'.format(num_cues))] * num_cues
    return sum(imap(mul, weights, scores))


def segmentation_list_for_segmented_sentence(segmented_sentence, word_delimiter=' '):
    word_count = 0
    segmentation = []
    for char in segmented_sentence:
        if char == word_delimiter:
            word_count += 1
            segmentation[-1] = True
        else:
            segmentation.append(False)
    segmentation[-1] = True
    return segmentation


# Main Program
if __name__ == '__main__':
    # Parse command-line arguments
    parser = argparse.ArgumentParser(description='PHOCUS is a word segmentation system.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("corpus", help="File to segment (any existing occurrences of word delimiter will be removed).",
                        type=argparse.FileType('r'), default='-')
    parser.add_argument("-bs", "--badScore", help="Score assigned when word length is less than window size.",
                        type=Fraction, default="0.0")
    parser.add_argument("-df", "--decayFactor", help="Exponent used to calculate memory decay (0 = no decay).",
                        type=Fraction, default="0.0")
    parser.add_argument("-fc", "--featureChart", help="Feature chart file", default=os.path.join(os.path.dirname(os.path.realpath(__file__)), '..', 'corpora', 'br.tab'))
    parser.add_argument("-fo", "--featureNgramsOut", help="File to dump final feature n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-fw", "--featureWindow", help="Window size for feature n-grams", type=int)
    parser.add_argument("-gp", "--goldPhonotactics",
                        help="Calculate phoneme n-gram scores based on their true frequencies in the gold corpus.",
                        action="store_true")
    parser.add_argument("-hp", "--hypotheticalPhonotactics",
                        help="When evaluating hypothetical words' well-formedness, increment counts of all n-grams within " +
                             "proposed word.",
                        action="store_true")
    parser.add_argument("-iw", "--ignoreWordBoundary",
                        help="When calculating phoneme/syllable/etc. n-gram scores, do not include word boundary.",
                        action="store_true")
    parser.add_argument("-ic", "--initialCount",
                        help="Count assigned to phonotactic largest n-grams before they are seen.",
                        type=Fraction, default="0.0001")
    parser.add_argument("-is", "--initializeSyllables",
                        help="Initialize syllable n-gram by finding all syllables in gold corpus and setting their counts to one " +
                             "in advance.",
                        action="store_true")
    parser.add_argument("-i", "--interactive",
                        help="After reading in corpus, user can specify an utterance number to segment up to, and query scores " +
                             "for possible segmentations.",
                        action="store_true")
    parser.add_argument("-jp", "--jointProbability", help="Use joint probabilities instead of conditional", action="store_true")
    parser.add_argument("-lo", "--lexiconOut", help="File to dump final lexicon to", type=argparse.FileType('w'))
    parser.add_argument("-ln", "--lineNumbers", help="Display line numbers before each segmented utterance", action="store_true")
    parser.add_argument("-mb", "--mbdp",
                        help="Use MBDP-1 (Brent 1999) phoneme and word scores functions.  Implies --initialCount = 0. Should " +
                             "also enable --hypotheticalPhonotactics for true MBDP-1.",
                        action="store_true")
    parser.add_argument("-nb", "--nBest",
                        help="Store scores for N-best segmentations instead of just the best. Should help lessen impact of " +
                             "early errors.",
                        action="store_true")
    parser.add_argument("-nl", "--noLexicon",
                        help="Only score words based on the phonotactics, and don't do 'familiar word' spotting.  Does NOT entail " +
                             "--tokenPhonotactics.",
                        action="store_true")
    parser.add_argument("-pn", "--phonemeNgramsOut", help="File to dump final phoneme n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-pc", "--piecewiseCountsOut", help="File to dump final strictly two-piecewise counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-pw", "--phonemeWindow", help="Window size for phoneme n-grams", type=int, default=1)
    parser.add_argument("-pu", "--printUtteranceDelimiter", help="Print utterance delimiter at the end of each utterance",
                        action="store_true")
    parser.add_argument("-rs", "--requireSyllabic",
                        help="Require each proposed word to contain at least one syllabic sound. " +
                             "(Requires --featureChart that includes 'syllabic' as feature)",
                        action="store_true")
    parser.add_argument("-sp", "--scorePiecewise", help="Score potential words based on their Strictly 2-Piecewise factors " +
                                                        "(i.e., long distance pairs for vowel and consonantal harmony).",
                        action="store_true")
    parser.add_argument("-st", "--stabilityThreshold",
                        help="When --waitForStablePhonemeDist is enabled, all the ratio between all phoneme counts when they are " +
                             "updated must be greater than stabilityThreshold before model will start segmenting.",
                        type=Fraction, default="0.99")
    parser.add_argument("-sd", "--subseqDenominator",
                        help="For all scores, calculate probability sequence of characters is in a word, rather than probability " +
                             "of them occuring in corpus.",
                        action="store_true")
    parser.add_argument("-sf", "--supervisedFor",
                        help="Number of utterances to use given word-boundaries for (0 = unsupervised).",
                        type=int)
    parser.add_argument("-su", "--supervisedUpdating",
                        help="When doing supervised segmenting with the supervisedFor flag, resume learning process after " +
                             "supervised portion of corpus.",
                        action="store_true")
    parser.add_argument("-so", "--syllableNgramsOut", help="File to dump final syllable n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-sw", "--syllableWindow",
                        help="Window size for syllable n-grams (Note: does not entail --initialSyllables)", type=int, default=1)
    parser.add_argument("-tp", "--tokenPhonotactics",
                        help="Update phoneme n-gram counts once per word occurrence, instead of per word type.",
                        action="store_true")
    parser.add_argument("-td", "--typeDenominator",
                        help="When evaluating familiar words, divide word count by total number of word tokens + total number of " +
                             "word types.  Should only be on for Venkataraman.",
                        action="store_true")
    parser.add_argument("-up", "--uniformPhonotactics",
                        help="Never update phonotactic n-gram counts.  Just use initial uniform distribution throughout.",
                        action="store_true")
    parser.add_argument("-ud", "--utteranceDelimiter",
                        help="Utterance delimiter (Note: Utterances are always assumed to be one-per-line, this delimiter is " +
                             "the symbol used when calculating n-grams at utterance boundaries.",
                        default="$")
    parser.add_argument("-ul", "--utteranceLimit",
                        help="Number of utterances in input corpus to process (0 = process all).", type=int,
                        default=0)
    parser.add_argument("-v", "--verbose", help="Print out scores for each possible segmentation of each utterance.",
                        action="store_true")
    parser.add_argument("-wf", "--waitForStablePhonemeDist",
                        help="Do not start attempting to segment until phoneme unigram has stabilized.", action="store_true")
    parser.add_argument("-wu", "--waitUntilUtterance",
                        help="Do not start attempting to segment until we have reached the specified utterance number.",
                        type=int)
    parser.add_argument("-ws", "--weightedSum",
                        help="Instead of using back-off model for score combination, use weighted sum with all cues " +
                             "weighted equally.",
                        action="store_true")
    parser.add_argument("-wd", "--wordDelimiter", help="Word delimiter (default: '%(default)s')", default=" ")
    args = parser.parse_args()

    if args.mbdp:
        args.initialCount = Fraction(0)

    feature_chart = PhonologicalFeatureChartReader(*os.path.split(args.featureChart))

    # TODO: fix this next line so hypothetical phonotactics is actually passed as an argument.
    cues = [FamiliarWordCue(args.initialCount),
            PhonemeNgramCue(args.phonemeWindow, args.initialCount, len([phone for phone in feature_chart._phones_to_features.viewkeys() if len(phone) == 1]), feature_chart)]

    if args.interactive:
        utterance_limit = int(raw_input("Utterance number to process to: "))
        args.utteranceLimit = -1 if utterance_limit == 0 else utterance_limit

    segmenter = Segmenter(cues, backoff_combiner, feature_chart, word_delimiter=args.wordDelimiter, utterance_limit=args.utteranceLimit, supervised=args.supervisedFor,
                 wait_until_utterance=args.waitUntilUtterance, wait_for_stable_phoneme_dist=args.waitForStablePhonemeDist, output_channel=sys.stdout,
                 semi_supervised_updating=args.supervisedUpdating, uniform_phonotactics=args.uniformPhonotactics, display_line_numbers=args.lineNumbers,
                 print_utterance_delimiter=args.printUtteranceDelimiter, utterance_delimiter=args.utteranceDelimiter)

    segmenter.incremental_processor(args.corpus)

    if args.interactive:
        segmenter.cmdloop()
