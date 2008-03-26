% Dan Blanchard
% Brent '99 Segmentation Model

-module (brent).
-export ([start/1]).
-export ([start/3]).



start(Input) ->
	start(Input, "#", "$").

start(Input, WordDelimiter, UtteranceDelimiter) ->
	{ok, Data} = file:read_file(Input),
	Utterances = string:tokens(binary_to_list(Data), "\n"),
	mdbp(Utterances, WordDelimiter, UtteranceDelimiter).
	
mdbp(Utterances, WordDelimiter, UtteranceDelimiter) ->
	mdbp_utterance_loop(Utterances, WordDelimiter, UtteranceDelimiter, orddict:update_counter(UtteranceDelimiter,0,orddict:new()), 0, orddict:update_counter(WordDelimiter,0,orddict:new()), 0).
		
mdbp_utterance_loop(Utterances, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes) when length(Utterances) > 0 ->
	[First | Rest] = Utterances,
	BestStart = mdbp_outer(First, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes),
	Segmentation = path_search(BestStart, length(First), []),
	{NewLexicon, NewTotalWords, NewPhonemeCounts, NewTotalPhonemes} = lexicon_updater(lists:sort(Segmentation ++ [length(First) + 1]), 
																						First, 
																						WordDelimiter,
																						Lexicon, 
																						TotalWords, 
																						PhonemeCounts, 
																						TotalPhonemes),
	io:format("~s~n",[UtteranceDelimiter]),
	mdbp_utterance_loop(Rest, WordDelimiter, UtteranceDelimiter, 
						orddict:update_counter(UtteranceDelimiter,1,NewLexicon),
						NewTotalWords + 1, NewPhonemeCounts, NewTotalPhonemes);
	
mdbp_utterance_loop(Utterances, _WordDelimiter, _UtteranceDelimiter, _Lexicon, _TotalWords, _PhonemeCounts, _TotalPhonemes) ->
	Utterances.
	
path_search(BestStart, FirstChar, Path) when FirstChar > 1 ->
	NewFirstChar = lists:nth(FirstChar - 1, BestStart),
	path_search(BestStart, NewFirstChar, Path ++ [NewFirstChar]);

path_search(_BestStart, _FirstChar, Path) ->
	Path.

lexicon_updater(Segmentation, Utterance, WordDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes) when length(Segmentation) > 1 ->
	[StartChar, EndChar | Rest] = Segmentation,
	NewWord = lists:sublist(Utterance, StartChar, EndChar - StartChar),
	io:format("~s",[NewWord ++ WordDelimiter]),	
	NewPhonemeCounts = 
		lists:foldl(
			fun (Phoneme, OldCounts) ->
				orddict:update_counter(Phoneme, 1, OldCounts)
			end,
			PhonemeCounts,
			NewWord),
	lexicon_updater([EndChar] ++ Rest, 
					Utterance, 
					WordDelimiter,
					orddict:update_counter(NewWord, 1, Lexicon), 
					TotalWords + 1, 
					orddict:update_counter(WordDelimiter, 1, NewPhonemeCounts), 
					TotalPhonemes + length(NewWord));

lexicon_updater(_Segmentation, _Utterance, _WordDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes) ->
	{Lexicon, TotalWords, PhonemeCounts, TotalPhonemes}.

mdbp_outer(Utterance, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes) ->
	LastCharSeq = lists:seq(1,length(Utterance)),
	BestList = lists:foldl(
		fun (LastChar, OldBestList) ->
			SubUtterance = lists:sublist(Utterance, LastChar),
			NewBestList = OldBestList ++ [{r(SubUtterance, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes), 1}],
			mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes, NewBestList, 2, LastChar)
		end,
		[],
		LastCharSeq),
	lists:map(
		fun (BestTuple) ->
			{_Product, Start } = BestTuple,
			Start
		end,
		BestList).

mdbp_inner([_First, Second | Rest], WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes, BestList, FirstChar, LastChar) when FirstChar =< LastChar -> 
	SubUtterance = [Second] ++ Rest,
	WordScore = r(SubUtterance, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes),
	{OldBestProduct, _} = lists:nth(FirstChar - 1, BestList),
	{LastCharBestProduct, _} = lists:nth(LastChar, BestList),
	ScoreProduct = WordScore * OldBestProduct,
	if 
		ScoreProduct > LastCharBestProduct ->
			NewBestList = lists:sublist(BestList, LastChar - 1) ++ [{ScoreProduct, FirstChar}] ++ lists:nthtail(LastChar, BestList);
		true ->
			NewBestList = BestList
	end,
	mdbp_inner(SubUtterance, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes, NewBestList, FirstChar + 1, LastChar);

mdbp_inner(_SubUtterance, _WordDelimiter, _UtteranceDelimiter, _Lexicon, _TotalWords, _PhonemeCounts, _TotalPhonemes, BestList, _FirstChar, _LastChar) ->
	BestList.

r(Word, WordDelimiter, UtteranceDelimiter, Lexicon, TotalWords, PhonemeCounts, TotalPhonemes) ->
	IsKey = orddict:is_key(Word, Lexicon),
	if 
		IsKey ->
			WordCount = orddict:fetch(Word, Lexicon),
 			(WordCount / (TotalWords + 1)) * math:pow(((WordCount - 1) - 1) / WordCount, 2);
		true ->
			WordTypes = length(Lexicon),
			if
				WordTypes > 0 ->
					WordPhonemeCounts =	lists:foldl(
													fun (Phoneme, OldCounts) ->
														orddict:update_counter(Phoneme, 1, OldCounts)
													end,
													PhonemeCounts,
													Word ++ WordDelimiter),
					WordTotalPhonemes = TotalPhonemes + length(Word) + 1,
					Pids = [prob_phonemes(self(), Key, WordDelimiter, WordPhonemeCounts, WordTotalPhonemes) || Key <- (orddict:fetch_keys(Lexicon) -- [UtteranceDelimiter])],
					PhonScore = lists:foldl(
											fun (_Process, Total) ->
												receive WordScore ->
													Total + WordScore
												end
											end,
											0,
											Pids),				
					CurrentWordScore = prob_phonemes(Word, WordDelimiter, WordPhonemeCounts, WordTotalPhonemes),
					%	0.607927101854027 = 6 / math:pow(math:pi(), 2) 
					FirstTerm = 0.607927101854027,
					SecondTerm = (WordTypes / (TotalWords + 1)),
					ThirdTop = 	CurrentWordScore,
					ThirdBottom = 1 - ((WordTypes - 1) / WordTypes) * (CurrentWordScore + PhonScore),
					ThirdTerm = ThirdTop / ThirdBottom,					
					FourthTerm = math:pow(((WordTypes - 1) / WordTypes),2),
					io:format("Word: ~s~nFirst: ~f~nSecond: ~f~nThird-Top: ~f~nThird-Bottom: ~f~nThird: ~f~nFourth: ~f~n",[Word, FirstTerm, SecondTerm, ThirdTop, ThirdBottom, ThirdTerm, FourthTerm]),
					io:format("Lexicon: ~p~nActual Phoneme Counts: ~p~nWord Phoneme Counts: ~p~nTotal Phonemes: ~w~nScore: ~w~n",[Lexicon,PhonemeCounts, WordPhonemeCounts,WordTotalPhonemes,FirstTerm * SecondTerm * ThirdTerm * FourthTerm]),
					FirstTerm * SecondTerm * ThirdTerm * FourthTerm;
				true ->
					0
			end
	end.

phoneme_score(Parent, Phoneme, PhonemeCounts, TotalPhonemes) ->
	spawn(
		fun() ->
			IsKey = orddict:is_key(Phoneme, PhonemeCounts),
			if 
				IsKey ->
					Score = orddict:fetch(Phoneme, PhonemeCounts) / TotalPhonemes;
				true ->
					Score = 0
			end,
			Parent ! Score				
		end).

prob_phonemes(Parent, Word, WordDelimiter, PhonemeCounts, TotalPhonemes) ->
	spawn(
		fun () ->
			Score = prob_phonemes(Word, WordDelimiter, PhonemeCounts, TotalPhonemes),
			Parent ! Score
		end).

prob_phonemes(Word, WordDelimiter, PhonemeCounts, TotalPhonemes) ->
	if 
		TotalPhonemes > 0 ->
			Score = 1 / (1 - (orddict:fetch(WordDelimiter, PhonemeCounts) / TotalPhonemes)),
			Me = self(),
			Pids = [phoneme_score(Me, P, PhonemeCounts, TotalPhonemes) || P <- Word],
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
