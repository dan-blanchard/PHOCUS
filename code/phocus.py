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

from __future__ import print_function, unicode_literals

import argparse
import copy
import cmd
import os
import sys
from abc import ABCMeta, abstractmethod
from collections import namedtuple
from decimal import Decimal
from fractions import Fraction
from functools import partial
from itertools import chain, imap, ifilter
from operator import mul, itemgetter

import regex as re
from featurechart import PhonologicalFeatureChartReader
from ngram import NgramModel
from nltk.util import ingrams
from probability import FreqDist, LidstoneProbDist, ConditionalFreqDist, ConditionalProbDist
from pyfst import fst


# Support class used for searching
ScorePointer = namedtuple('ScorePointer', ['prev_index', 'product', 'word_score'])
ScoredEdge = namedtuple('ScoredEdge', ['score', 'graph_index', 'pointer_list_index'])


class PartialCountNgramModel(NgramModel):
    '''
    NgramModel that supports storing counts as types other than int.
    '''

    def __init__(self, n, train, estimator, freqtype, padding, backoff, *estimator_args, **estimator_kw_args):
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
        @type estimator: a function that takes a L{ConditionalFreqDist} and returns a L{ConditionalProbDist}
        @param freqtype: the type to use to store the counts in the underlying frequency distribution
        @type freqtype: any numeric type
        @param backoff: whether or not we should use Katz back-off
        @type backoff: L{bool}
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
        self._padding = (padding,) * (n - 1)
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
        self._backoff = PartialCountNgramModel(n - 1, train, estimator, freqtype, padding, backoff, *estimator_args, **estimator_kw_args) if (backoff and n > 1) else None

    def update(self, samples, increase_amount=1):
        '''
        Update the underlying frequency distributions given the current list of samples.
        '''
        cond_samples = []
        for utterance in samples:
            for ngram in ingrams(chain(self._padding, utterance, self._padding), self._n):
                self._ngrams.add(ngram)
                cond_samples.append((tuple(ngram[:-1]), ngram[-1]))
        self._model.update(cond_samples, increase_amount)

        # Recursively update lower-order models
        if self._backoff:
            self._backoff.update(samples, increase_amount)

    def prob(self, word, context):
        """
        Evaluate the probability of this word in this context using Katz Backoff.

        @param word: the word to get the probability of
        @type word: C{string}
        @param context: the context the word is in
        @type context: C{list} of C{string}
        """

        context = tuple(context)
        if not self._backoff or (context + (word,) in self._ngrams):
            return self[context].prob(word)
        else:
            # print "Alpha: {}\tBackoff prob: {}".format(self._alpha(context), self._backoff.prob(word, context[1:]))
            return self._alpha(context) * self._backoff.prob(word, context[1:])


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
        '''
        The frequency distribution for storing subsequence counts.
        '''
        return self._subseq_counts

    @abstractmethod
    def eval_word(self, word):
        '''
            @return: probability that proposed word is a word.
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


class NgramCue(Cue):
    '''
        Feature that scores words based on their constituent n-grams.
    '''

    def __init__(self, n, initial_count, num_unigrams, **kwargs):
        '''
            @param n: the order of the language model (ngram size)
            @type n: L{int}
            @param initial_count: the initial count used for unseen n-grams
            @type initial_count: L{Fraction}
            @param num_unigrams: the number of possible unigrams types for the current cue
            @type num_unigrams: L{int}
            @param backoff: should Katz back-off be used in underlying n-gram model
            @type backoff: L{bool}
            @param hypothetical_phonotactics: should n-gram counts be temporarily updated to include hypothetical word's n-grams?
            @type hypothetical_phonotactics: L{bool}
            @param score_combiner: Function to combine scores in eval_word
            @type score_combiner: C{function}
            @param subseq_counts: A frequency distribution for storing subsequence counts. Should use the same one for all L{Cues}
                                  of the current L{Segmenter}.
            @type subseq_counts: L{FreqDist}
        '''
        self._hypothetical_phonotactics = kwargs.pop('hypothetical_phonotactics', False)
        prob_estimator = kwargs.pop('prob_estimator', lambda freqdist, curr_n, initial_count, bins: LidstoneProbDist(freqdist,
                                                                                                                     initial_count,
                                                                                                                     num_unigrams))
        self._backoff = kwargs.pop('backoff', False)
        self._word_delimiter = kwargs.pop('word_delimiter', ' ')
        super(NgramCue, self).__init__(initial_count, **kwargs)
        self._n = n
        self._ngram_model = PartialCountNgramModel(n, None, prob_estimator, Fraction, self._word_delimiter, self._backoff, initial_count, num_unigrams)

    def eval_word(self, word):
        '''
            @return: probability that proposed word is a word.
        '''
        if self._hypothetical_phonotactics:
            # Make a deep copy of the ngram model so that we can update it without modifying the original
            word_ngram_model = copy.deepcopy(self._ngram_model)
            word_ngram_model.update([word])
        else:
            word_ngram_model = self._ngram_model

        # If I want to duplicate functionality of OCaml code, should probably make a ProbDist that inherits from Lidstone,
        # otherwise this won't do the denominator adjustments and other stuff.
        # for ngram in ingrams(chain(self._ngram_model._padding, word, self._ngram_model._padding), self._n):
        #     print("Ngram: {}\tProb for {} given {}: {}".format(ngram, ngram[-1], tuple(ngram[:-1]), self._ngram_model.prob(tuple(ngram[:-1]), ngram[-1])), file=sys.stderr)

        if self._n > 1:
            adjustment = Fraction(1)
        else:
            # Need to compensate for the lack of an empty word by getting rel freq of word_delimiter somehow
            # print("word delimiter prob: {}".format(self._ngram_model.prob(self._word_delimiter, self._ngram_model._padding)), file=sys.stderr)
            adjustment = Fraction(1) / (Fraction(1) - self._ngram_model.prob(self._word_delimiter, self._ngram_model._padding))

        # print([self._ngram_model.prob(ngram[-1], tuple(ngram[:-1])) for ngram in
        #                                     ingrams(chain(*([self._word_delimiter, word, self._word_delimiter] if self._n > 1 else [word, self._word_delimiter])),
        #                                                   self._n)], file=sys.stderr)

        raw_score = self._score_combiner([self._ngram_model.prob(ngram[-1], tuple(ngram[:-1])) for ngram in
                                            ingrams(chain(*([self._word_delimiter, word, self._word_delimiter] if self._n > 1 else [word, self._word_delimiter])),
                                                          self._n)])
        # print("adjustment: {:.10e}\traw score: {:.10e}".format(float(adjustment), float(raw_score)), file=sys.stderr)

        return adjustment * raw_score

    def update_evidence(self, word, increase_amount):
        # Used to have token phonotactics check here in OCaml code, but that really breaks the separation of the different cues.
        # I'll take care of checking that the word isn't in the lexicon (or token phonotactics has been specified) outside of this class.
        self._ngram_model.update([word] if self._n > 1 else [word, self._word_delimiter])
        # print self._ngram_model

    def dump(self, dump_file):
        '''
            @todo: Implement this function.
        '''
        raise NotImplementedError

    @abstractmethod
    def use_score(self, word):
        pass


class FamiliarWordCue(Cue):
    '''
        Feature that scores words based on their lexical frequency.
    '''

    def __init__(self, mbdp=False, subseq_counts=None, witten_bell=False, bad_score=0):
        '''
            Initializes any counts to their default values, if necessary
            @param mbdp: Use MBDP-1 score adjustments when calculating word scores.
            @type mbdp: L{bool}
            @param subseq_counts: A frequency distribution for storing subsequence counts. Should use the same one for all L{Cues}
                                  of the current L{Segmenter}.
            @type subseq_counts: L{FreqDist}
            @param witten_bell: Use Witten-Bell smoothing (like Venkataraman's model) for familiar word scores. This also multiplies
                                sub-word scores by Witten-Bell normalizing factor. This is ignored in no lexicon mode.
            @type witten_bell: L{bool}
        '''
        super(FamiliarWordCue, self).__init__(Fraction(0), subseq_counts=subseq_counts)
        self._phonotactic = False
        self._lexicon = FreqDist(counttype=Fraction)
        self._mbdp = mbdp
        self._witten_bell = witten_bell
        self._bad_score = bad_score

    def in_lexicon(self, word):
        '''
            @return: whether or not the given word is in the lexicon.
        '''
        return word in self._lexicon

    @property
    def total_words(self):
        ''' Total number of word tokens in lexicon. '''
        return self._lexicon.N()

    def eval_word(self, word):
        '''
            @return: probability that proposed word is a word.
            @todo: Implement lexical decay.
        '''
        if word in self._lexicon:
            word_count = Fraction(self._lexicon[word])
            if not self._mbdp:
                word_types = self._lexicon.B()  # Unlike OCaml version we're not adding utterance delimiter to lexicon, so no subtraction.
                raw_score = word_count / (Fraction(self.subseq_counts[word]) if self.subseq_counts
                                                                        else (Fraction(self.total_words + word_types) if self._witten_bell
                                                                                                                      else Fraction(self.total_words)))
            else:
                raw_score = ((word_count + Fraction(1)) / (self.total_words + Fraction(1))) * (((word_count / (word_count + Fraction(1))) ** Fraction(2)))
        elif self._witten_bell:
            word_types = Fraction(self._lexicon.B() - 1)  # Subtract one for initial utterance delimiter addition
            raw_score = word_types / Fraction(self.total_words + word_types)
        else:
            raw_score = self._bad_score
        return raw_score  # lexical decay stuff would need to be added here

    def dump(self, dump_file):
        for word in self._lexicon.iterkeys():
            dump_file.write(word + str(self._lexicon[word]) + '\n')
        if self._subseq_counts:
            for seq in self.subseq_counts.iterkeys():
                dump_file.write(seq + str(self.subseq_counts[seq]) + '\n')
        dump_file.close()

    def use_score(self, word):
        return self.in_lexicon(word)

    def update_evidence(self, word, increase_amount):
        self._lexicon.inc(word, increase_amount)


class SyllableNgramCue(NgramCue):
    """ NgramCue that calculates probabilities for sequences of syllables."""
    def __init__(self, n, initial_count, true_words, feature_chart, hypothetical_phonotactics=False,
                 score_combiner=lambda scores: reduce(mul, scores), subseq_counts=None, diphthongs=None):
        self._vowels = feature_chart.phones_for_features("+syllabic")
        self._vowel_re = re.compile("[" + ''.join([re.escape(vowel) for vowel in self._vowels if len(vowel) == 1]) + "]")
        self._diphthongs = set(diphthongs) if diphthongs is not None else set()
        super(SyllableNgramCue, self).__init__(n, initial_count, len(set(chain(*[self.syllabify(word) for word in true_words]))),
              hypothetical_phonotactics=hypothetical_phonotactics, score_combiner=score_combiner, subseq_counts=subseq_counts)

    def syllabify(self, word):
        '''
        Return a list of syllables in a word.
        '''
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
                for i in xrange(1, len(syllables)):
                    adjusted_index = i - num_removed
                    if self._vowel_re.search(syllables[adjusted_index]) and not self._vowel_re.search(syllables[adjusted_index - 1]):
                        syllables[adjusted_index] = ''.join(syllables[adjusted_index - 1: adjusted_index + 1])
                        del syllables[adjusted_index - 1]
                        num_removed += 1
                # Codas
                num_inserted = 0
                for i in xrange(0, len(syllables) - 1):
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


class NBestNode(object):
    """ Simple data structure for storing an N-Best Tree when performing an n-best search """
    def __init__(self, back_score, graph_index):
        super(NBestNode, self).__init__()
        self.back_score = back_score
        self.graph_index = graph_index
        self._children = set()

    def add_child(self, child_node):
        ''' Add a child to this node '''
        self._children.add(child_node)

    def iter_descendants(self):
        ''' Depth-first iteration through all node in subtree starting with the current node. '''
        for child_node in self._children:
            yield child_node
            for grand_child in child_node.iter_descendants():
                yield grand_child
        else:
            yield self

    def iter_tree(self):
        ''' Depth-first iteration through all node in subtree starting with the current node. '''
        yield self
        for child_node in self._children:
            for grand_child in child_node.iter_tree():
                yield grand_child

    def has_edge(self, child_index):
        ''' Check if one of the children of this node has the index child_index. '''
        return NBestNode(0, child_index) in self._children

    @property
    def num_children(self):
        ''' Number of child nodes for the current node. '''
        return len(self._children)

    def __eq__(self, other):
        return self.graph_index == other.graph_index

    def __hash__(self):
        return self.graph_index.__hash__()

    def __repr__(self):
        return "{{{}, {}: {}}}".format(self.graph_index, self.back_score, list(self._children))

    def __len__(self):
        return len(list(self.iter_tree()))


class Segmenter(cmd.Cmd, object):
    """ A PHOCUS word segmentation model"""

    def __init__(self, cues, evidence_combiner, feature_chart, word_delimiter=' ', utterance_limit=0, supervised=0,
                 wait_until_utterance=0, wait_for_stable_phoneme_dist=False, output_channel=sys.stdout, semi_supervised_updating=False,
                 uniform_phonotactics=False, display_line_numbers=False, print_utterance_delimiter=False, utterance_delimiter='$',
                 nbest_window=1):
        '''
        Initializes a PHOCUS word segmentation model.

        @param cues: The Cues to use when segmenting in order of precedence.
        @type cues: L{list} of L{Cue}s
        @param evidence_combiner: Function to determine how scores from cues are combined (actually calls L{Cue.eval_word} for each L{Cue}).
        @type evidence_combiner: C{function} that takes two arguments: a list of L{Cue}s and a word
        @param feature_chart: The phonological feature chart for the corpus.
        @type feature_chart: L{PhonologicalFeatureChartReader}
        @param word_delimiter: The word delimiter that may be currently separating words in the sentence. (They will all be removed before
                               segmenting.)
        @type word_delimiter: L{basestring}
        '''
        super(Segmenter, self).__init__()
        self.cues = cues
        self.evidence_combiner = evidence_combiner
        self.word_delimiter = word_delimiter
        self.eval_word = partial(evidence_combiner, self.cues)
        self.feature_chart = feature_chart
        self.nbest_window = nbest_window
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

    def nbest_search(self, sentence):
        '''
            Backtracking N-best algorithm based on algorithm presented in Tran et al. (1996)


            @param sentence: The sentence to segment
            @type sentence: L{basestring}

            @return: Pairs of weights and segmentations (in this case all weights will be 1).
            @rtype: L{list} of 2-L{tuple}s of L{Fraction}s and L{int} L{list}s

            @todo: Properly handle ties
        '''
        sentence = sentence.replace(self.word_delimiter, '')
        sentence_length = len(sentence)

        # Build word graph
        word_graph = fst.Acceptor()
        for last_char in xrange(1, sentence_length + 1):
            word_graph.add_state()
            word = sentence[0:last_char]
            word_score = -Decimal(float(self.eval_word(word))).ln()  # Use negative log probabilities because OpenFST just adds path weights together
            word_graph.add_arc(0, last_char, word, weight=word_score)
            for first_char in xrange(1, last_char):
                word = sentence[first_char:last_char]
                word_score = -Decimal(float(self.eval_word(word))).ln()
                word_graph.add_arc(first_char, last_char, word, weight=word_score)
        word_graph[sentence_length].final = True

        # Loop through n-best paths
        scored_segmentations = []
        # print("Shortest distance: {}".format(word_graph.shortest_distance()))
        best_score = Decimal(-float(word_graph.shortest_distance(reverse=True)[0])).exp()
        nbest_path_fst = word_graph.shortest_path(self.nbest_window)
        # Iterate through n-best paths
        for path in nbest_path_fst.paths(noeps=True):
            segmentation = [False] * len(sentence)
            segmentation[-1] = True
            seg_score = Decimal('0.0')
            for arc in path:
                seg_score += Decimal(float(arc.weight))
                segmentation[arc.nextstate - 1] = True
            seg_score = Decimal(-seg_score).exp()
            scored_segmentations.append((Fraction(seg_score / best_score) if scored_segmentations else Fraction(1), segmentation))

        # print("Num segmentations: {}".format(len(scored_segmentations)))
        # print("Segmentations: {}".format(scored_segmentations))
        return scored_segmentations

    def evidence_updater(self, increment_amount, segmentation, sentence, cue_selector=lambda cue: True, print_only=False):
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
                if not print_only:
                    for cue in ifilter(cue_selector, self.cues):
                        cue.update_evidence(word, increment_amount)
                if increment_amount == 1 and sentence != self.utterance_delimiter:
                    self.output_channel.write(word)
                    if last < len(segmentation):
                        self.output_channel.write(self.word_delimiter)
        # Commit subsequence counts here...
        if not print_only:
            pass

    def incremental_processor(self, utterance_list):
        '''
        Processes the corpus once, outputting segmentations for a given unsegmented utterance list.
        '''
        for i, segmented_sentence in enumerate(utterance_list):
            # Get scored segmentations
            if not self.utterance_limit or i < self.utterance_limit:
                sentence = segmented_sentence.replace(self.word_delimiter, '').rstrip('\n')
                if self.supervised <= i and i + 1 > self.wait_until_utterance:  # and (not self.wait_for_stable_phoneme_dist or PhonemeNgramCue.use_score(sentence)):
                    scored_segmentations = self.nbest_search(sentence)
                else:
                    scored_segmentations = [(Fraction(1), seg_list_for_segmented_sentence(segmented_sentence))]
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
                        self.evidence_updater(score, segmentation, sentence, print_only=True)
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
        ''' @return: the score for the given words given the current cues. '''
        score = Fraction(1)
        for word in words.split(self.word_delimiter):
            score *= backoff_combiner(self.cues, word, verbose=True)
        print("Combined score: {:.10e}\n".format(float(score)), file=sys.stderr)

    def do_add(self, arg_string):
        ''' Increases the frequencies of the given words by the given amount. '''
        arg_list = arg_string.split()
        increase_amount = arg_list.pop(0)
        sentence = self.word_delimiter.join(arg_list)
        self.evidence_updater(Fraction(increase_amount), seg_list_for_segmented_sentence(sentence), sentence.replace(self.word_delimiter, ''))
        print()

    def do_syllabify(self, words):
        ''' Breaks up the given words into syllables. '''
        for word in words.split(self.word_delimiter):
            print(SyllableNgramCue(1, 0, [], self.feature_chart).syllabify(word))
        print(file=sys.stderr)

    def do_exit(self, s):
        ''' Exit the program. '''
        print()
        return True

    do_EOF = do_exit


def backoff_combiner(cues, word, bad_score=Fraction(0), witten_bell=False, verbose=False):
    '''
    Calculates the scores for all the cues in the given list and then chooses the first one where cue.use_score(word) is True.
    '''
    scores = [cue.eval_word(word) for cue in cues]
    familiar_score = Fraction(1)
    if verbose:
        print('\n'.join(["{} score for {} = {:.10e}".format(type(cue).__name__, word, float(scores[i])) for i, cue in enumerate(cues)]), file=sys.stderr)
        sys.stderr.flush()
    for i, cue in enumerate(cues):
        if isinstance(cue, FamiliarWordCue):
            familiar_score = scores[i]
            if cue.use_score(word):
                return familiar_score
        else:
            if cue.use_score(word):
                return scores[i] * (familiar_score if witten_bell else Fraction(1))
    else:
        return bad_score


def weighted_sum_combiner(cues, word, weights=None, verbose=False):
    '''
    Combines given cues via weighted sum.
    '''
    scores = [cue.eval_word(word) for cue in cues]
    if verbose:
        print('\n'.join(["{} score for {} = {:.10e}".format(type(cue).__name__, word, float(scores[i])) for i, cue in enumerate(cues)]), file=sys.stderr)
        sys.stderr.flush()
    if not weights:
        num_cues = len(cues)
        weights = [Fraction('1/{}'.format(num_cues))] * num_cues
    return sum(imap(mul, weights, scores))


def multiplicative_combiner(cues, word, verbose=False):
    '''
    Multiples cue scores together for a given word.
    '''
    scores = [cue.eval_word(word) for cue in cues]
    if verbose:
        print('\n'.join(["{} score for {} = {:.10e}".format(type(cue).__name__, word, float(scores[i])) for i, cue in enumerate(cues)]), file=sys.stderr)
        sys.stderr.flush()
    return imap(mul, scores)


def seg_list_for_segmented_sentence(segmented_sentence, word_delimiter=' '):
    '''
    @return: a segmentation list for a given segmented sentence.
    '''
    # word_count = 0
    segmentation = []
    for char in segmented_sentence:
        if char == word_delimiter:
            # word_count += 1
            segmentation[-1] = True
        else:
            segmentation.append(False)
    segmentation[-1] = True
    return segmentation


def csv_list(arg_list):
    '''
    Helper function for parsing command-line arguments that are comma-delimited lists.
    '''
    return arg_list.split(',')


def main():
    '''
    Main Program
    '''
    # Parse command-line arguments
    parser = argparse.ArgumentParser(description='PHOCUS is a word segmentation system.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("corpus", help="File to segment (any existing occurrences of word delimiter will be removed before segmenting).",
                        type=argparse.FileType('r'), default='-')
    parser.add_argument("-bs", "--badScore", help="Score assigned when word length is less than window size.",
                        type=Fraction, default="0.0")
    parser.add_argument("-d", "--diphthongs", help="Comma-delimited list of possible diphthongs.", type=csv_list, default='9I,OI,9U')
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
    parser.add_argument("-kb", "--katzBackoff", help="For all n-gram models, use Katz back-off.", action="store_true")
    parser.add_argument("-jp", "--jointProbability", help="Use joint probabilities instead of conditional", action="store_true")
    parser.add_argument("-lo", "--lexiconOut", help="File to dump final lexicon to", type=argparse.FileType('w'))
    parser.add_argument("-ln", "--lineNumbers", help="Display line numbers before each segmented utterance", action="store_true")
    parser.add_argument("-mb", "--mbdp",
                        help="Use MBDP-1 (Brent 1999) phoneme and word scores functions.  Implies --initialCount = 0. Should " +
                             "also enable --hypotheticalPhonotactics for true MBDP-1.",
                        action="store_true")
    parser.add_argument("-nb", "--nBest",
                        help="Number of segmentations to update evidence for for each sentence.", type=int, default=1)
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
                        help="Window size for syllable n-grams (Note: does not entail --initialSyllables)", type=int, default=0)
    parser.add_argument("-tp", "--tokenPhonotactics",
                        help="Update phoneme n-gram counts once per word occurrence, instead of per word type.",
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
    parser.add_argument("-wb", "--wittenBell",
                        help="Use Witten-Bell smoothing (like Venkataraman's model) for familiar word scores. This also multiplies sub-word scores "
                             "by Witten-Bell normalizing factor. This is ignored in no lexicon mode.",
                        action="store_true")
    parser.add_argument("-wd", "--wordDelimiter", help="Word delimiter (default: '%(default)s')", default=" ")
    args = parser.parse_args()

    if args.mbdp:
        args.initialCount = Fraction(0)

    feature_chart = PhonologicalFeatureChartReader(*os.path.split(args.featureChart))
    subseq_counts = FreqDist(counttype=Fraction) if args.subseqDenominator else None

    # Put cue list together
    cues = [FamiliarWordCue(subseq_counts=subseq_counts)] if not args.noLexicon else []
    # Syllables
    if args.syllableWindow:
        corpus_name = args.corpus.name
        true_words = set(args.corpus.read().replace('\n', args.wordDelimiter).split(args.wordDelimiter))
        true_words.remove('')
        args.corpus.close()
        args.corpus = open(corpus_name)
        cues.append(SyllableNgramCue(args.syllableWindow, args.initialCount, true_words, feature_chart, hypothetical_phonotactics=args.hypotheticalPhonotactics,
                                     subseq_counts=subseq_counts, diphthongs=args.diphthongs, backoff=args.katzBackoff))
    # Phonemes
    if args.phonemeWindow:
        cues.append(PhonemeNgramCue(args.phonemeWindow, args.initialCount, len([phone for phone in feature_chart.phones_to_features.viewkeys() if len(phone) == 1]),
                    hypothetical_phonotactics=args.hypotheticalPhonotactics, backoff=args.katzBackoff))

    if args.interactive:
        utterance_limit = int(raw_input("Utterance number to process to: "))
        args.utteranceLimit = -1 if utterance_limit == 0 else utterance_limit

    segmenter = Segmenter(cues, partial(backoff_combiner if not args.weightedSum else weighted_sum_combiner, verbose=args.verbose, witten_bell=args.wittenBell), feature_chart,
                 word_delimiter=args.wordDelimiter, utterance_limit=args.utteranceLimit, supervised=args.supervisedFor, wait_until_utterance=args.waitUntilUtterance,
                 wait_for_stable_phoneme_dist=args.waitForStablePhonemeDist, output_channel=sys.stdout, semi_supervised_updating=args.supervisedUpdating,
                 uniform_phonotactics=args.uniformPhonotactics, display_line_numbers=args.lineNumbers, print_utterance_delimiter=args.printUtteranceDelimiter,
                 utterance_delimiter=args.utteranceDelimiter, nbest_window=args.nBest)
    segmenter.incremental_processor(args.corpus)

    if args.interactive:
        import readline  # pylint: disable=W0611
        segmenter.cmdloop()

    return segmenter

# Main Program
if __name__ == '__main__':
    segmenter = main()
