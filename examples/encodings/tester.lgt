
:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

	% only test UTF-32 encoding on Prolog dialects supporting it
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(library(lgtunit_loader)),
			logtalk_load(loader),
			logtalk_load([tests_iso_8859_1, tests_utf_8, tests_utf_16], [hook(lgtunit)]),
			tests_iso_8859_1::run,
			tests_utf_8::run,
			tests_utf_16::run
		)).

	:- else.

		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(library(lgtunit_loader)),
			logtalk_load(loader),
			logtalk_load([tests_iso_8859_1, tests_utf_8, tests_utf_16, tests_utf_32], [hook(lgtunit)]),
			tests_iso_8859_1::run,
			tests_utf_8::run,
			tests_utf_16::run,
			tests_utf_32::run
		)).

	:- endif.

:- endif.
