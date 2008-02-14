
:- object(fft(_Threads)).

	:- info([
		version is 1.0,
		date is 2008/2/13,
		author is 'Paul Crocker, adapted from original code by Colin Barker',
		comment is 'Simple multi-threaded version of the Fast Fourier Transform.',
		source is 'Original code available from http://pagesperso-orange.fr/colin.barker/lpa/fft.htm',
		parameters is ['Threads'- 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']]).

	:- threaded.

	:- public(fft/3).
	:- mode(fft(+integer, +list, -list), one).
	:- info(fft/3, [
		comment is 'Returns a list of complex numbers the FFT given a List of Complex Numbers and N the size of that list a power of two',
		argnames is ['Number', 'List', 'FFT']]).

	:- private(w_cache/2).
	:- dynamic(w_cache/2).
	:- mode(w_cache(+integer, -complex), zero_or_one).
	:- mode(w_cache(?integer, -complex), zero_or_more).

	fft(N, L, Ft) :-
		parameter(1, Threads),
		mt_fft(Threads, N, L, Ft).

	mt_fft(_, 1, Ft, Ft) :-
		!.
	mt_fft(1, N, L, Ft) :-
		!,
		st_fft(N, L, Ft).
	mt_fft(Threads, N, L, Ft) :-
		Threads2 is Threads//2,
		N2 is N // 2,
		evens_and_odds(L, E, O),
		threaded((
			mt_fft(Threads2, N2, E, Et),
			mt_fft(Threads2, N2, O, Ot)
		)),
		w(1, W1),
		w(2, W2),
		w(N, Wn),
		product_and_sum(Et, Ot, W2, Wn, Gt, []),
		product_and_sum(Et, Ot, W1, Wn, Ft, Gt).

	% The (Cooley-Tukey) Algorithm - recursive, 1-d, unordered radix 2 fft

	/* fft(N, F, Ft) is true if the list Ft contains the Fourier transform of        */
	/*   the N -- a power of two -- samples in the list F.  Each sample is a         */
	/*   complex number represented by c(RealPart, ImaginaryPart).			         */

	st_fft(1, Ft, Ft) :-
		!.
	st_fft(N, F, Ft) :-
		N > 1,
		N2 is N // 2,
		evens_and_odds(F, E, O),
		st_fft(N2, E, Et),
		st_fft(N2, O, Ot),
		w(1, W1),
		w(2, W2),
		w(N, Wn),
		product_and_sum(Et, Ot, W2, Wn, Gt, []),
		product_and_sum(Et, Ot, W1, Wn, Ft, Gt).

	/* Multiply and Add vectors */
	product_and_sum([], [], _, _, Ft, Ft).
	product_and_sum([E| Et], [O| Ot], Wk, Wn, [F| Ft], Fu) :-
		product(O, Wk, Temp),
		sum(E, Temp, F),
		product(Wk, Wn, Wk1),
		product_and_sum(Et, Ot, Wk1, Wn, Ft, Fu).

	/* evens_and_odds(Xs, Evens, Odds) is true if Evens is the list of the	        */
	/*   even-positioned elements of the list Xs, and Odds is the list of the       */
	/*   odd-positioned elements of the list Xs, where the first element of Xs      */
	/*   is considered to be at an even position.							        */
	evens_and_odds([], [], []).
	evens_and_odds([X| Xs], [X| Ys], Zs) :-
		evens_and_odds(Xs, Zs, Ys).

	/* sum(A, B, C) is true if C is the sum of the complex numbers A and B.	        */
	sum(c(Ra, Ia), c(Rb, Ib), c(Rc, Ic)) :-
		Rc is Ra + Rb,
		Ic is Ia + Ib.

	/* product(A, B, C) is true if C is the product of the complex numbers A and B. */
	product(c(Ra, Ia), c(Rb, Ib), c(Rc, Ic)) :-
		Rc is Ra*Rb - Ia*Ib,
		Ic is Ra*Ib + Ia*Rb.

	twiddle(N, c(R, I)) :-
		R is sin(2.0*pi/N),
		I is cos(2.0*pi/N),
		assertz(w_cache(N, c(R, I))).

	w(N, C) :-
		(	w_cache(N, C) ->
			true
		;	twiddle(N, C)
		).

	w_cache(	1, c( 1.0, 0.0)).
	w_cache(	2, c(-1.0, 0.0)).
	w_cache(	4, c( 0.0, 1.0)).
	w_cache(	8, c( 0.70710678119, 0.70710678119)).
	w_cache(   16, c( 0.92387953251, 0.38268343237)).
	w_cache(   32, c( 0.98078528040, 0.19509032202)).
	w_cache(   64, c( 0.99518472667, 0.09801714033)).
	w_cache(  128, c( 0.99879545621, 0.049067674327)).
	w_cache(  256, c( 0.99969881870, 0.024541228523)).
	w_cache(  512, c( 0.99992470184, 0.012271538286)).
	w_cache( 1024, c( 0.99998117528, 0.0061358846492)).
	w_cache( 2048, c( 0.99999529381, 0.003067956763)).
	w_cache( 4096, c( 0.99999882345, 0.0015339801863)).
	w_cache( 8192, c( 0.99999970586, 0.00076699031874)).
	w_cache(16384, c( 0.99999992647, 0.00038349516882)).

:- end_object.
