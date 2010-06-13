
:- object(bench_dcg, 
	implements(databasep)).

	%%Parsing natural language with a DCG.

	sentence(A, C) <-
		noun_phrase(A, B) &
		verb_phrase(B, C).

	noun_phrase(A, B) <-
		noun_phrase2(A, B).
	noun_phrase(A, C) <-
		determiner(A, B) &
		noun_phrase2(B, C).

	verb_phrase(A, C) <-
		verb(A, B) &
		noun_phrase(B, C).
	verb_phrase(A, B) <-
		verb(A, B).

	verb([contains|A], A) <- true.
	verb([eats|A], A) <- true.

	noun([pieplate|A], A) <- true.
	noun([surprise|A], A) <- true.
	noun([man|A], A) <- true.

	adjective([decorated|A], A) <- true.
	adjective([corpulent|A], A) <- true.

	determiner([the|A], A) <- true.
	determiner([a|A], A) <- true.

	noun_phrase2(A, C) <-
		adjective(A, B) &
		noun_phrase2(B, C).
	noun_phrase2(A, B) <-
		noun(A, B).

	bench_goal(sentence([the, corpulent, man, contains, a, decorated, pieplate], [])).
	bench_goal(sentence([the, corpulent, man, contains, a, decorated, platepie], [])).

:- end_object.
