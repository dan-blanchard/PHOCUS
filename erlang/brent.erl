% Dan Blanchard
% Brent '99 Segmentation Model

-module (brent).
-export ([start/1]).
-export ([start/3]).

% Main function used to run segmenter
start(Input) ->
	start(Input, "#", "$").

% Main function used to run segmenter (with optional arguments)
start(Input, WordDelimiter, UtteranceDelimiter) ->
	{ok, Data} = file:read_file(Input),
	Utterances = string:tokens(binary_to_list(Data), "\n"),
	mdbp(Utterances, WordDelimiter, UtteranceDelimiter).

	
% Does the preliminary stuff and then runs the utterance loop
mdbp(Utterances, WordDelimiter, UtteranceDelimiter) ->
	ets:new(phoneme_counts, [named_table]),
	ets:new(lexicon, [named_table]),
	ets:insert(lexicon, {UtteranceDelimiter, 0}),
	ets:insert(phoneme_counts, {WordDelimiter, 0}),
	mdbp_utterance_loop(Utterances, WordDelimiter, UtteranceDelimiter, 0, 0),
	ets:delete(lexicon),
	ets:delete(phoneme_counts).

all_possible_words(Utterance) ->
	UtteranceLength = length(Utterance),
	FirstCharSeq = lists:seq(1, UtteranceLength),
	lists:flatmap(
				fun(FirstChar) ->				
					LastCharSeq = lists:seq(FirstChar,UtteranceLength),
					lists:map(
							fun(LastChar) ->
								string:sub_string(Utterance, FirstChar, LastChar)
							end,
							LastCharSeq)
				end,
				FirstCharSeq).
	
% Loops through all utterances in list, running the various parts of the MBDP algorithm
mdbp_utterance_loop([_|_]=Utterances, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) ->
	[First | Rest] = Utterances,
	Pids = [r(self(), Word, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) || Word <- all_possible_words(First)],
	RScores = lists:foldl(
						fun (_Process, OldDict) ->
							receive {PossWord, RScore} ->
								dict:store(PossWord, RScore, OldDict)
							end
						end,
						dict:new(),
						Pids),
	BestStart = mdbp_outer(First, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, RScores),
	Segmentation = path_search(BestStart, length(First), []),
	{NewTotalWords, NewTotalPhonemes} = lexicon_updater(lists:sort([length(First) + 1] ++ Segmentation),
																						First,
																						WordDelimiter,
																						TotalWords,
																						TotalPhonemes),
	io:format("~s~n", [UtteranceDelimiter]),
	ets:update_counter(lexicon, UtteranceDelimiter, 1),
	mdbp_utterance_loop(Rest, WordDelimiter, UtteranceDelimiter,
						NewTotalWords + 1, NewTotalPhonemes);
	
mdbp_utterance_loop(Utterances, _WordDelimiter, _UtteranceDelimiter, _TotalWords, _TotalPhonemes) ->
	Utterances.
	
	
% Retrieves the segmentation from BestStart list
path_search(BestStart, FirstChar, Path) when FirstChar > 1 ->
	NewFirstChar = lists:nth(FirstChar - 1, BestStart),
	path_search(BestStart, NewFirstChar, [NewFirstChar] ++ Path);

path_search(_BestStart, _FirstChar, Path) ->
	lists:reverse(Path).


% Adds the new words to the lexicon, and updates phoneme counts
lexicon_updater([_,_|_]=Segmentation, Utterance, WordDelimiter, TotalWords, TotalPhonemes) ->
	[StartChar, EndChar | Rest] = Segmentation,
	NewWord = lists:sublist(Utterance, StartChar, EndChar - StartChar),
	io:format("~s", [NewWord ++ [WordDelimiter]]), 	
	lists:foreach(
				fun (Phoneme) ->
					IsMember = ets:member(phoneme_counts, [Phoneme]),
					if 
						IsMember ->
							ets:update_counter(phoneme_counts, [Phoneme], 1); % need to make phoneme a list so it's considered a string
						true ->
							ets:insert(phoneme_counts, {[Phoneme], 1})
					end
				end,
				NewWord),
	IsMember = ets:member(lexicon, NewWord),
	if
		IsMember ->
			ets:update_counter(lexicon, NewWord, 1);
		true ->
			ets:insert(lexicon, {NewWord, 1})
	end,
	ets:update_counter(phoneme_counts, WordDelimiter, 1),
	lexicon_updater([EndChar] ++ Rest,
					Utterance,
					WordDelimiter,
					TotalWords + 1,
					TotalPhonemes + length(NewWord) + 1);

lexicon_updater(_Segmentation, _Utterance, _WordDelimiter, TotalWords, TotalPhonemes) ->
	{TotalWords, TotalPhonemes}.


% MBDP outer loop
mdbp_outer(Utterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, RScores) ->
	LastCharSeq = lists:seq(1, length(Utterance)),
	BestList = lists:foldl(
						fun (LastChar, OldBestList) ->
							SubUtterance = lists:sublist(Utterance, LastChar),
							NewBestList = OldBestList ++ [{dict:fetch(SubUtterance, RScores), 1}],
							mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, NewBestList, 2, LastChar, RScores)
						end,
						[],
						LastCharSeq),
	lists:map(
			fun (BestTuple) ->
				{_Product, Start } = BestTuple,
				Start
			end,
			BestList).


% MBDP inner loop
mdbp_inner([_First, Second | Rest], WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, BestList, FirstChar, LastChar, RScores) when FirstChar =< LastChar -> 
	SubUtterance = [Second] ++ Rest,
	WordScore = dict:fetch(SubUtterance, RScores),
	{OldBestProduct, _} = lists:nth(FirstChar - 1, BestList),
	{LastCharBestProduct, _} = lists:nth(LastChar, BestList),
	ScoreProduct = WordScore * OldBestProduct,
	if 
		ScoreProduct > LastCharBestProduct ->
			NewBestList = lists:sublist(BestList, LastChar - 1) ++ [{ScoreProduct, FirstChar}] ++ lists:nthtail(LastChar, BestList);
		true ->
			NewBestList = BestList
	end,
	mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, NewBestList, FirstChar + 1, LastChar, RScores);

mdbp_inner(_SubUtterance, _WordDelimiter, _UtteranceDelimiter, _TotalWords, _TotalPhonemes, BestList, _FirstChar, _LastChar, _RScores) ->
	BestList.


% Finds R value for word as new process
r(Parent, Word, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) ->
	spawn(
		fun () ->
			% io:fwrite("Started new R process~n"),
			Score = r(Word, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes),
			Parent ! {Word, Score}
		end).

% Finds R value for word
r(Word, WordDelimiter, _UtteranceDelimiter, TotalWords, TotalPhonemes) ->
	IsMember = ets:member(lexicon, Word),
	if 
		IsMember ->
			WordCount = ets:lookup_element(lexicon, Word, 2),
 			(WordCount / (TotalWords + 1)) * math:pow(((WordCount - 1) - 1) / WordCount, 2);
		true ->
			WordTypes = ets:info(lexicon, size),
			if
				WordTypes > 0 ->
					WordWithBoundary = Word ++ WordDelimiter,
					WordPhonemeCounts = ets:new(word_phoneme_counts,[]),
					lists:foreach(
								fun (Phoneme) ->
									IsPhonemeWordMember = ets:member(WordPhonemeCounts, [Phoneme]),
									IsPhonemeMember = ets:member(phoneme_counts, [Phoneme]),
									if									
										IsPhonemeWordMember ->
											ets:update_counter(WordPhonemeCounts, [Phoneme], 1);
										IsPhonemeMember ->
											ets:insert(WordPhonemeCounts, {[Phoneme], ets:lookup_element(phoneme_counts, [Phoneme], 2) + 1});
										true ->
											ets:insert(WordPhonemeCounts, {[Phoneme], 1})
									end
								end,
								WordWithBoundary), 					
					WordTotalPhonemes = TotalPhonemes + length(WordWithBoundary),
					% io:format("OriginalTotalPhonemes: ~p~nWord length: ~p~n", [TotalPhonemes, length(WordWithBoundary)]),
					CurrentWordScore = prob_phonemes(WordWithBoundary, WordDelimiter, WordPhonemeCounts, WordTotalPhonemes),
					ets:delete(WordPhonemeCounts),
					%	0.607927101854027 = 6 / math:pow(math:pi(), 2) 
					FirstTerm = 0.607927101854027,
					SecondTerm = (WordTypes / (TotalWords + 1)),
					ThirdTerm = CurrentWordScore,
					FourthTerm = math:pow(((WordTypes - 1) / WordTypes), 2),
					% io:format("Word: ~s~nFirst: ~f~nSecond: ~f~nThird-Top: ~f~nThird-Bottom: ~f~nThird: ~f~nFourth: ~f~n", [WordWithBoundary, FirstTerm, SecondTerm, ThirdTop, ThirdBottom, ThirdTerm, FourthTerm]),
					% io:format("Lexicon: ~p~nActual Phoneme Counts: ~p~nWord Phoneme Counts: ~p~nTotal Phonemes: ~w~nScore: ~w~n", [ets:tab2list(lexicon), ets:tab2list(phoneme_counts), ets:tab2list(WordPhonemeCounts), WordTotalPhonemes, FirstTerm * SecondTerm * ThirdTerm * FourthTerm]),
					FirstTerm * SecondTerm * ThirdTerm * FourthTerm;
				true ->
					0
			end
	end.


% Calculates scores for all phonemes in a word
prob_phonemes(Word, WordDelimiter, WordPhonemeCounts, TotalPhonemes) ->
	if 
		TotalPhonemes > 0 ->
			lists:foldl(
						fun(Phoneme, Score) ->
							IsWordMember = ets:member(WordPhonemeCounts, [Phoneme]),
							IsMember = ets:member(phoneme_counts, [Phoneme]),
							if 
								IsWordMember ->
									Score * (ets:lookup_element(WordPhonemeCounts, [Phoneme], 2) / TotalPhonemes);
								IsMember ->
									Score * (ets:lookup_element(phoneme_counts, [Phoneme], 2) / TotalPhonemes);
								true ->
									0
							end
						end,
						1 / (1 - (ets:lookup_element(WordPhonemeCounts, WordDelimiter, 2) / TotalPhonemes)),
						Word);
		true ->
			0
	end.
