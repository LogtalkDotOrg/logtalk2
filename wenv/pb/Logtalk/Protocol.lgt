
:- protocol(Protocol).

	:- info([
		version is 1.0,
		author is 'ÇFULLUSERNAMEÈ',
		date is ÇYEARÈ/02/01,
		comment is '']).

	:- public(Functor/Arity).
	:- mode(Functor(), Solutions).
	:- info(Functor/Arity, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3']]).

	:- protected(Functor/Arity).

	:- private(Functor/Arity).


:- end_protocol.
