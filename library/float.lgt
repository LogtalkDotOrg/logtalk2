
:- object(float,
	extends(number)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2010/04/26,
		comment is 'Floating point numbers data type predicates.']).

	:- public(op(700, xfx, '=~=')).
	:- public('=~='/2).
	:- mode('=~='(+float, +float), zero_or_one).
	:- info('=~='/2, [
		comment is 'Compares two float values for approximate equality.',
		argnames is ['Float1', 'Float2']]).

	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			abs(Float1 - Float2) < 0.0001 ->
			true
		;	% if that fails, test the relative error (protected by a catch/3 to avoid division errors)
		 	% by using as the divisor the larger float in order to make argument order irrelevant:
			abs(Float1) > abs(Float2) ->
			catch(abs((Float1 - Float2) / Float1) < 0.0001, _, fail)
		;	catch(abs((Float1 - Float2) / Float2) < 0.0001, _, fail)
		).

	valid(Float) :-
		float(Float).

	check(Term) :-
		this(This),
		sender(Sender),
		(	float(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
