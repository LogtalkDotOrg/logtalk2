
:- object(sentence,
	implements(parsep)).


	parse(List, Result) :-
		phrase(sentence, List) ->
			Result = true
			;
			Result = false.


	sentence --> noun_phrase, verb_phrase.

	noun_phrase --> determiner, noun.
	noun_phrase --> noun.

	verb_phrase --> verb.
	verb_phrase --> verb, noun_phrase.


	determiner --> [the].
	determiner --> [a].

	noun --> [boy].
	noun --> [girl].

	verb --> [likes].
	verb --> [hates].


:- end_object.
