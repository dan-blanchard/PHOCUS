% Dan Blanchard
% Brent '99 Segmentation Model

-module (brent).
-export ([start/1]).
-export ([start/3]).

% Until I can figure out a way around it, use actual ascii values for # (35) and $ (36)

start(Input) ->
	start(Input, "#", "$").

start(Input, WordDelimiter, UtteranceDelimiter) ->
	{ok, Data} = file:read_file(Input),
	Utterances = string:tokens(binary_to_list(Data), "\n"),
	mdbp(Utterances, WordDelimiter, UtteranceDelimiter).
	
mdbp(Utterances, WordDelimiter, UtteranceDelimiter) ->
	ets:new(phoneme_counts, [named_table]),
	ets:new(lexicon, [named_table]),
	ets:insert(lexicon, {UtteranceDelimiter, 0}),
	ets:insert(phoneme_counts, {WordDelimiter, 0}),
	mdbp_utterance_loop(Utterances, WordDelimiter, UtteranceDelimiter, 0, 0).
		
mdbp_utterance_loop(Utterances, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) when length(Utterances) > 0 ->
	[First | Rest] = Utterances,
	BestStart = mdbp_outer(First, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes),
	Segmentation = path_search(BestStart, length(First), []),
	{NewTotalWords, NewTotalPhonemes} = lexicon_updater(lists:sort(Segmentation ++ [length(First) + 1]),
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
	
path_search(BestStart, FirstChar, Path) when FirstChar > 1 ->
	NewFirstChar = lists:nth(FirstChar - 1, BestStart),
	path_search(BestStart, NewFirstChar, Path ++ [NewFirstChar]);

path_search(_BestStart, _FirstChar, Path) ->
	Path.

lexicon_updater(Segmentation, Utterance, WordDelimiter, TotalWords, TotalPhonemes) when length(Segmentation) > 1 ->
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

mdbp_outer(Utterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) ->
	LastCharSeq = lists:seq(1, length(Utterance)),
	BestList = lists:foldl(
		fun (LastChar, OldBestList) ->
			SubUtterance = lists:sublist(Utterance, LastChar),
			NewBestList = OldBestList ++ [{r(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes), 1}],
			mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, NewBestList, 2, LastChar)
		end,
		[],
		LastCharSeq),
	lists:map(
		fun (BestTuple) ->
			{_Product, Start } = BestTuple,
			Start
		end,
		BestList).

mdbp_inner([_First, Second | Rest], WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, BestList, FirstChar, LastChar) when FirstChar =< LastChar -> 
	SubUtterance = [Second] ++ Rest,
	WordScore = r(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes),
	{OldBestProduct, _} = lists:nth(FirstChar - 1, BestList),
	{LastCharBestProduct, _} = lists:nth(LastChar, BestList),
	ScoreProduct = WordScore * OldBestProduct,
	if 
		ScoreProduct > LastCharBestProduct ->
			NewBestList = lists:sublist(BestList, LastChar - 1) ++ [{ScoreProduct, FirstChar}] ++ lists:nthtail(LastChar, BestList);
		true ->
			NewBestList = BestList
	end,
	mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes, NewBestList, FirstChar + 1, LastChar);

mdbp_inner(_SubUtterance, _WordDelimiter, _UtteranceDelimiter, _TotalWords, _TotalPhonemes, BestList, _FirstChar, _LastChar) ->
	BestList.

r(Word, WordDelimiter, UtteranceDelimiter, TotalWords, TotalPhonemes) ->
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
					io:format("OriginalTotalPhonemes: ~p~nWord length: ~p~n", [TotalPhonemes, length(WordWithBoundary)]),
					Pids = [prob_phonemes(self(), Key, WordDelimiter, WordPhonemeCounts, WordTotalPhonemes) || Key <- (lists:map(
																																fun ({Key,_Value}) ->
																																	Key
																																end,
						 																										ets:tab2list(lexicon)) 
																													-- [UtteranceDelimiter])],
					PhonScore = lists:foldl(
											fun (_Process, Total) ->
												receive WordScore ->
													Total + WordScore
												end
											end,
											0,
											Pids), 				
					CurrentWordScore = prob_phonemes(WordWithBoundary, WordDelimiter, WordPhonemeCounts, WordTotalPhonemes),
					%	0.607927101854027 = 6 / math:pow(math:pi(), 2) 
					FirstTerm = 0.607927101854027,
					SecondTerm = (WordTypes / (TotalWords + 1)),
					ThirdTop = 	CurrentWordScore,
					ThirdBottom = 1 - ((WordTypes - 1) / WordTypes) * (CurrentWordScore + PhonScore),
					ThirdTerm = ThirdTop / ThirdBottom, 					
					FourthTerm = math:pow(((WordTypes - 1) / WordTypes), 2),
					io:format("Word: ~s~nFirst: ~f~nSecond: ~f~nThird-Top: ~f~nThird-Bottom: ~f~nThird: ~f~nFourth: ~f~n", [WordWithBoundary, FirstTerm, SecondTerm, ThirdTop, ThirdBottom, ThirdTerm, FourthTerm]),
					io:format("Lexicon: ~p~nActual Phoneme Counts: ~p~nWord Phoneme Counts: ~p~nTotal Phonemes: ~w~nScore: ~w~n", [ets:tab2list(lexicon), ets:tab2list(phoneme_counts), ets:tab2list(WordPhonemeCounts), WordTotalPhonemes, FirstTerm * SecondTerm * ThirdTerm * FourthTerm]),
					FirstTerm * SecondTerm * ThirdTerm * FourthTerm;
				true ->
					0
			end
	end.

phoneme_score(Parent, Phoneme, WordPhonemeCounts, TotalPhonemes) ->
	spawn(
		fun() ->
			IsWordMember = ets:member(WordPhonemeCounts, [Phoneme]),
			IsMember = ets:member(phoneme_counts, [Phoneme]),
			if 
				IsWordMember ->
					Score = ets:lookup_element(WordPhonemeCounts, [Phoneme], 2) / TotalPhonemes;
				IsMember ->
					Score = ets:lookup_element(phoneme_counts, [Phoneme], 2) / TotalPhonemes;
				true ->
					Score = 0
			end,
			Parent ! Score				
		end).

prob_phonemes(Parent, Word, WordDelimiter, WordPhonemeCounts, TotalPhonemes) ->
	spawn(
		fun () ->
			Score = prob_phonemes(Word, WordDelimiter, WordPhonemeCounts, TotalPhonemes),
			Parent ! Score
		end).

prob_phonemes(Word, WordDelimiter, WordPhonemeCounts, TotalPhonemes) ->
	if 
		TotalPhonemes > 0 ->
			Score = 1 / (1 - (ets:lookup_element(WordPhonemeCounts, WordDelimiter, 2) / TotalPhonemes)),
			Me = self(),
			Pids = [phoneme_score(Me, P, WordPhonemeCounts, TotalPhonemes) || P <- Word],
			lists:foldl(
						fun (_Process, Total) ->
							receive PhonScore ->
								Total * PhonScore
							end
						end,
						Score,
						Pids);
		true ->
			0
	end.
