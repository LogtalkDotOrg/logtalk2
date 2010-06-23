" Vim syntax file
"
" Language:	Logtalk
" Maintainer:	Paulo Moura <pmoura@logtalk.org>
" Last Change:	June 23, 2010


" Quit when a syntax file was already loaded:

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif


" Logtalk is case sensitive:

syn case match


" Logtalk variables

syn match   logtalkVariable		"\<\(\u\|_\)\(\w\)*\>"


" Logtalk clause functor

syn match	logtalkOperator		":-"


" Logtalk quoted atoms and strings

syn region	logtalkString		start=+"+	skip=+\\"+	end=+"+
syn region	logtalkAtom		start=+'+	skip=+\\'+	end=+'+		contains=logtalkEscapeSequence

syn match	logtalkEscapeSequence	contained	"\\\([\\abfnrtv\"\']\|\(x[a-fA-F0-9]\+\|[0-7]\+\)\\\)"


" Logtalk message sending operators

syn match	logtalkOperator		"::"
syn match	logtalkOperator		":"
syn match	logtalkOperator		"\^\^"


" Logtalk external call

syn region	logtalkExtCall		matchgroup=logtalkExtCallTag		start="{"		matchgroup=logtalkExtCallTag		end="}"		contains=ALL


" Logtalk opening entity directives

syn region	logtalkOpenEntityDir	matchgroup=logtalkOpenEntityDirTag	start=":- object("	matchgroup=logtalkOpenEntityDirTag	end=")\."	contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom,logtalkEntityRel,logtalkLineComment
syn region	logtalkOpenEntityDir	matchgroup=logtalkOpenEntityDirTag	start=":- protocol("	matchgroup=logtalkOpenEntityDirTag	end=")\."	contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkEntityRel,logtalkLineComment
syn region	logtalkOpenEntityDir	matchgroup=logtalkOpenEntityDirTag	start=":- category("	matchgroup=logtalkOpenEntityDirTag	end=")\."	contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkEntityRel,logtalkLineComment


" Logtalk closing entity directives

syn match	logtalkCloseEntityDir	":- end_object\."
syn match	logtalkCloseEntityDir	":- end_protocol\."
syn match	logtalkCloseEntityDir	":- end_category\."


" Logtalk entity relations

syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="instantiates("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained
syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="specializes("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained
syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="extends("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained
syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="imports("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained
syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="implements("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained
syn region	logtalkEntityRel	matchgroup=logtalkEntityRelTag	start="complements("	matchgroup=logtalkEntityRelTag	end=")"		contains=logtalkEntity,logtalkVariable,logtalkNumber,logtalkOperator,logtalkString,logtalkAtom	contained


" Logtalk directives

syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- if("			matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- elif("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn match	logtalkDirTag		":- else\."
syn match	logtalkDirTag		":- endif\."
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- alias("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- calls("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- encoding("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- initialization("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- info("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- mode("		matchgroup=logtalkDirTag	end=")\."	contains=logtalkOperator, logtalkAtom
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- dynamic("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn match	logtalkDirTag		":- dynamic\."
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- discontiguous("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- multifile("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- public("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- protected("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- private("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- meta_predicate("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- op("			matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- set_logtalk_flag("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- synchronized("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn match	logtalkDirTag		":- synchronized\."
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- uses("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn match	logtalkDirTag		":- threaded\."


" Prolog directives

syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- ensure_loaded("	matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- set_prolog_flag("	matchgroup=logtalkDirTag	end=")\."	contains=ALL


" Module directives

syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- module("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- export("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- reexport("		matchgroup=logtalkDirTag	end=")\."	contains=ALL
syn region	logtalkDir		matchgroup=logtalkDirTag	start=":- use_module("		matchgroup=logtalkDirTag	end=")\."	contains=ALL


" Logtalk built-in predicates

syn match	logtalkBuiltIn		"\<\(abolish\|c\(reate\|urrent\)\)_\(object\|protocol\|category\)\ze("

syn match	logtalkBuiltIn		"\<\(object\|protocol\|category\)_property\ze("

syn match	logtalkBuiltIn		"\<complements_object\ze("
syn match	logtalkBuiltIn		"\<extends_\(object\|protocol\|category\)\ze("
syn match	logtalkBuiltIn		"\<imp\(orts_category\|lements_protocol\)\ze("
syn match	logtalkBuiltIn		"\<\(instantiat\|specializ\)es_class\ze("

syn match	logtalkBuiltIn		"\<\(abolish\|define\)_events\ze("
syn match	logtalkBuiltIn		"\<current_event\ze("

syn match	logtalkBuiltIn		"\<\(curren\|se\)t_logtalk_flag\ze("

syn match	logtalkBuiltIn		"\<logtalk_\(compile\|l\(ibrary_path\|oad\)\)\ze("

syn match	logtalkBuiltIn		"\<\(for\|retract\)all\ze("

syn match	logtalkBuiltIn		"\<threaded\(_\(call\|once\|ignore\|exit\|peek\|wait\|notify\)\)\?\ze("


" Logtalk built-in methods

syn match	logtalkBuiltInMethod	"\<parameter\ze("
syn match	logtalkBuiltInMethod	"\<se\(lf\|nder\)\ze("
syn match	logtalkBuiltInMethod	"\<this\ze("

syn match	logtalkBuiltInMethod	"\<current_predicate\ze("
syn match	logtalkBuiltInMethod	"\<predicate_property\ze("

syn match	logtalkBuiltInMethod	"\<a\(bolish\|ssert\(a\|z\)\)\ze("
syn match	logtalkBuiltInMethod	"\<clause\ze("
syn match	logtalkBuiltInMethod	"\<retract\(all\)\?\ze("

syn match	logtalkBuiltInMethod	"\<\(bag\|set\)of\ze("
syn match	logtalkBuiltInMethod	"\<f\(ind\|or\)all\ze("

syn match	logtalkBuiltInMethod	"\<before\ze("
syn match	logtalkBuiltInMethod	"\<after\ze("

syn match	logtalkBuiltInMethod	"\<expand_\(goal\|term\)\ze("
syn match	logtalkBuiltInMethod	"\<\(goal\|term\)_expansion\ze("
syn match	logtalkBuiltInMethod	"\<phrase\ze("


" Mode operators

syn match	logtalkOperator		"?"
syn match	logtalkOperator		"@"


" Control constructs

syn match	logtalkKeyword		"\<true\>"
syn match	logtalkKeyword		"\<fail\>"
syn match	logtalkKeyword		"\<ca\(ll\|tch\)\ze("
syn match	logtalkOperator		"!"
" syn match	logtalkOperator		","
syn match	logtalkOperator		";"
syn match	logtalkOperator		"-->"
syn match	logtalkOperator		"->"
syn match	logtalkKeyword		"\<throw\ze("


" Term unification

syn match	logtalkOperator		"="
syn match	logtalkKeyword		"\<unify_with_occurs_check\ze("
syn match	logtalkOperator		"\\="


" Term testing

syn match	logtalkKeyword		"\<var\ze("
syn match	logtalkKeyword		"\<atom\(ic\)\?\ze("
syn match	logtalkKeyword		"\<integer\ze("
syn match	logtalkKeyword		"\<float\ze("
syn match	logtalkKeyword		"\<c\(allable\|ompound\)\ze("
syn match	logtalkKeyword		"\<n\(onvar\|umber\)\ze("


" Term comparison

syn match	logtalkKeyword		"\<compare\ze("
syn match	logtalkOperator		"@=<"
syn match	logtalkOperator		"=="
syn match	logtalkOperator		"\\=="
syn match	logtalkOperator		"@<"
syn match	logtalkOperator		"@>"
syn match	logtalkOperator		"@>="


" Term creation and decomposition

syn match	logtalkKeyword		"\<functor\ze("
syn match	logtalkKeyword		"\<arg\ze("
syn match	logtalkOperator		"=\.\."
syn match	logtalkKeyword		"\<copy_term\ze("


" Arithemtic evaluation

syn match	logtalkOperator		"\<is\>"


" Arithemtic comparison

syn match	logtalkOperator		"=:="
syn match	logtalkOperator		"=\\="
syn match	logtalkOperator		"<"
syn match	logtalkOperator		"=<"
syn match	logtalkOperator		">"
syn match	logtalkOperator		">="


" Stream selection and control

syn match	logtalkKeyword		"\<\(curren\|se\)t_\(in\|out\)put\ze("
syn match	logtalkKeyword		"\<open\ze("
syn match	logtalkKeyword		"\<close\ze("
syn match	logtalkKeyword		"\<flush_output\ze("
syn match	logtalkKeyword		"\<flush_output\>"
syn match	logtalkKeyword		"\<stream_property\ze("
syn match	logtalkKeyword		"\<at_end_of_stream\ze("
syn match	logtalkKeyword		"\<at_end_of_stream\>"
syn match	logtalkKeyword		"\<set_stream_position\ze("


" Character and byte input/output

syn match	logtalkKeyword		"\<\(get\|p\(eek\|ut\)\)_\(c\(har\|ode\)\|byte\)\ze("
syn match	logtalkKeyword		"\<nl\ze("
syn match	logtalkKeyword		"\<nl\>"


" Term input/output

syn match	logtalkKeyword		"\<read\(_term\)\?\ze("
syn match	logtalkKeyword		"\<write\(q\|_\(canonical\|term\)\)\?\ze("
syn match	logtalkKeyword		"\<\(current_\)\?op\ze("
syn match	logtalkKeyword		"\<\(current_\)\?char_conversion\ze("


" Logic and control

syn match	logtalkOperator		"\\+"
syn match	logtalkKeyword		"\<once\ze("
syn match	logtalkKeyword		"\<repeat\>"


" Atomic term processing

syn match	logtalkKeyword		"\<atom_\(length\|c\(hars\|o\(ncat\|des\)\)\)\ze("
syn match	logtalkKeyword		"\<sub_atom\ze("
syn match	logtalkKeyword		"\<char_code\ze("
syn match	logtalkKeyword		"\<number_c\(har\|ode\)s\ze("


" Implementation defined hooks functions

syn match	logtalkKeyword		"\<\(curren\|se\)t_prolog_flag\ze("
syn match	logtalkKeyword		"\<halt\ze("
syn match	logtalkKeyword		"\<halt\>"


" Evaluable functors

syn match	logtalkOperator		"+"
syn match	logtalkOperator		"-"
syn match	logtalkOperator		"\*"
syn match	logtalkOperator		"//"
syn match	logtalkOperator		"/"
syn match	logtalkKeyword		"\<r\(ound\|em\)\ze("
syn match	logtalkKeyword		"\<e\>"
syn match	logtalkKeyword		"\<pi\>"
syn match	logtalkKeyword		"\<rem\>"
syn match	logtalkKeyword		"\<mod\ze("
syn match	logtalkKeyword		"\<mod\>"
syn match	logtalkKeyword		"\<abs\ze("
syn match	logtalkKeyword		"\<sign\ze("
syn match	logtalkKeyword		"\<flo\(or\|at\(_\(integer\|fractional\)_part\)\?\)\ze("
syn match	logtalkKeyword		"\<truncate\ze("
syn match	logtalkKeyword		"\<ceiling\ze("


" Other arithemtic functors

syn match	logtalkOperator		"\*\*"
syn match	logtalkKeyword		"\<s\(in\|qrt\)\ze("
syn match	logtalkKeyword		"\<cos\ze("
syn match	logtalkKeyword		"\<atan\ze("
syn match	logtalkKeyword		"\<exp\ze("
syn match	logtalkKeyword		"\<log\ze("


" Bitwise functors

syn match	logtalkOperator		">>"
syn match	logtalkOperator		"<<"
syn match	logtalkOperator		"/\\"
syn match	logtalkOperator		"\\/"
syn match	logtalkOperator		"\\"


" Logtalk list operator

syn match	logtalkOperator		"|"


" Logtalk existential quantifier operator

syn match	logtalkOperator		"\^"


" Logtalk numbers 

syn match	logtalkNumber		"\<\d\+\>"
syn match	logtalkNumber		"\<\d\+\.\d\+\>"
syn match	logtalkNumber		"\<\d\+[eE][-+]\=\d\+\>"
syn match	logtalkNumber		"\<\d\+\.\d\+[eE][-+]\=\d\+\>"
syn match	logtalkNumber		"\<0'.\|0''\|0'\"\>"
syn match	logtalkNumber		"\<0b[0-1]\+\>"
syn match	logtalkNumber		"\<0o\o\+\>"
syn match	logtalkNumber		"\<0x\x\+\>"


" Logtalk end-of-clause

syn match	logtalkOperator		"\."


" Logtalk comments

syn region	logtalkBlockComment	start="/\*"	end="\*/"	fold
syn match	logtalkLineComment	"%.*"


" Logtalk entity folding

syn region logtalkEntity transparent fold keepend start=":- object(" end=":- end_object\." contains=ALL
syn region logtalkEntity transparent fold keepend start=":- protocol(" end=":- end_protocol\." contains=ALL
syn region logtalkEntity transparent fold keepend start=":- category(" end=":- end_category\." contains=ALL


syn sync ccomment logtalkBlockComment maxlines=50


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet

if version >= 508 || !exists("did_logtalk_syn_inits")
	if version < 508
		let did_logtalk_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink	logtalkBlockComment	Comment
	HiLink	logtalkLineComment	Comment

	HiLink	logtalkOpenEntityDir	Normal
	HiLink	logtalkOpenEntityDirTag	PreProc

	HiLink	logtalkEntity		Normal

	HiLink	logtalkEntityRel	Normal
	HiLink	logtalkEntityRelTag	PreProc

	HiLink	logtalkCloseEntityDir	PreProc

	HiLink	logtalkDir		Normal
	HiLink	logtalkDirTag		PreProc

	HiLink	logtalkAtom		String
	HiLink	logtalkString		String
	HiLink	logtalkEscapeSequence	SpecialChar

	HiLink	logtalkNumber		Number

	HiLink	logtalkKeyword		Keyword

	HiLink	logtalkBuiltIn		Keyword
	HiLink	logtalkBuiltInMethod	Keyword

	HiLink	logtalkOperator		Operator

	HiLink	logtalkExtCall		Normal
	HiLink	logtalkExtCallTag	Operator

	HiLink	logtalkVariable		Identifier

	delcommand HiLink

endif


let b:current_syntax = "logtalk"
