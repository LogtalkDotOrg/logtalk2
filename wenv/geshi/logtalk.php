<?php

/*************************************************************************************
 * logtalk.php
 * -----------
 * 
 * Author: Clara Dimene <c_dimene@hotmail.com>
 *
 * Copyright: (c) 2008 Clara Dimene
 *
 * Logtalk language file for GeSHi.
 *
 *
 * Based on GeSHi release 1.0.7.21
 * Last Change:	July 18, 2008
 *
 *************************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/
 
$language_data = array (
	'LANG_NAME' => 'Logtalk',
	'COMMENT_SINGLE' => array(1 => '%'),
	'COMMENT_MULTI' => array('/*' => '*/'),
	'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
	'QUOTEMARKS' => array('"'),
	'ESCAPE_CHAR' => '',
	'KEYWORDS' => array(),
	'SYMBOLS' => array('//'
		),
	'CASE_SENSITIVE' => array(
		GESHI_COMMENTS => false
		),
	'STYLES' => array(
		'KEYWORDS' => array(
		),
		'COMMENTS' => array(
			1 => 'color: #60a0b0; font-style: italic;',
			'MULTI' => 'color: #60a0b0; font-style: italic;'
			),
		'ESCAPE_CHAR' => array(
			0 => 'color: #000000;'
			),	
		'SYMBOLS' => array(
			0 => 'color: #666666;font-weight: bold;'
			),	
		'BRACKETS' => array(),
		'STRINGS' => array(
			0 => 'color: #4070a0;'
			),
		'NUMBERS' => array(
			0 => 'color: #40a070;'
			),
		'METHODS' => array(
			),

		'REGEXPS' => array(
		    1 => 'color: #bb60d5; font-weight: bold;',
			2 => 'color: #40a070; font-weight: bold;', 
			3 => 'color: #40a070; font-weight: bold;',
			4 => 'color: #007020; font-weight: bold;',
			5 => 'color: #007020; font-weight: bold;',
			6 => 'color: #007020; font-weight: bold;',
			7 => 'color: #007020; font-weight: bold;',
			8 => 'color: #007020; font-weight: bold;',
		    9 => 'color: #007020; font-weight: bold;',
			10 => 'color: #007020; font-weight: bold;',
		    11 => 'color: #007020; font-weight: bold;',
			12 => 'color: #007020; font-weight: bold;',
			13 => 'color: #007020; font-weight: bold;',
		    14 => 'color: #007020; font-weight: bold;',
		    15 => 'color: #007020; font-weight: bold;',
		    16 => 'color: #007020; font-weight: bold;',
		    17 => 'color: #007020; font-weight: bold;',	   
		    18 => 'color: #007020; font-weight: bold;',
		    19 => 'color: #007020; font-weight: bold;',
		    20 => 'color: #007020; font-weight: bold;',
		    21 => 'color: #007020; font-weight: bold;',
		    22 => 'color: #007020; font-weight: bold;',
		    23 => 'color: #007020; font-weight: bold;',
		    24 => 'color: #007020; font-weight: bold;',
		    25 => 'color: #007020; font-weight: bold;',
		    26 => 'color: #007020; font-weight: bold;',
		    27 => 'color: #007020; font-weight: bold;',
		    28 => 'color: #007020; font-weight: bold;',
		    29 => 'color: #007020; font-weight: bold;',
		    30 => 'color: #007020; font-weight: bold;',
	    	31 => 'color: #40a070; font-weight: bold;',
	    	32 => 'color: #007020; font-weight: bold;',
			33 => 'color: #007020; font-weight: bold;',
			34 => 'color: #007020; font-weight: bold;',
			35 => 'color: #007020; font-weight: bold;',
			36 => 'color: #007020; font-weight: bold;',
			37 => 'color: #007020; font-weight: bold;',
			38 => 'color: #007020; font-weight: bold;',
			39 => 'color: #40a070; font-weight: bold;',
			40 => 'color: #666666; ',
			41 => 'color: #007020; font-weight: bold;',
			42 => 'color: #666666;',
			43 => 'color: #666666;',
			44 => 'color: #000000;',
			45 => 'color: #666666;',
			46 => 'color: #666666;',
			47 => 'color: #666666;',
		    ),
		'SCRIPT' => array(
		)
		),
	'URLS' => array(),
	'OOLANG' => false,
	'OBJECT_SPLITTERS' => array( 1 => '::'
	),

	'REGEXPS' => array( 

			//variables
		1 => array(
					GESHI_SEARCH => "(?<!0'|')(\b[A-Z_]\w*)",
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => '\\2'
					),

			// Numbers
		2 => array(
					GESHI_SEARCH => "(0'.)",
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	
		
			//Strings
		3 => array(
					GESHI_SEARCH => "('.*')",
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => '\\2'
					),	
			//Event handlers
		4 => array(
					GESHI_SEARCH => '(before|after)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),
		
			//Execution-context methods
		5 => array(
					GESHI_SEARCH => '(this|se(lf|nder)|parameter)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Reflection
		6 => array(
					GESHI_SEARCH => '(predicate_property|current_predicate)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//DCGs and term expansion
		7 => array(
					GESHI_SEARCH => '(phrase|expand_term|((goal|term)_expansion))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

		   //Entity
		8 => array(
					GESHI_SEARCH => '((abolish|c(reate|urrent))_(object|protocol|category)|(object|protocol|category)_property)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Entity relations
		9 => array(
					GESHI_SEARCH => '(complements_object|extends_(object|protocol|category)|imp(lements_protocol|orts_category)|((instantiat|specializ)es_class))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Events
		10 => array(
					GESHI_SEARCH => '(((abolish|define)_events)|current_event)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Flags
		11 => array(
					GESHI_SEARCH => '((set|current)_prolog_flag|((set|current)_logtalk_flag))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Compiling, loading, and library paths
		12 => array(
					GESHI_SEARCH => '(logtalk_(compile|l(oad|ibrary_path)))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Database
		13 => array(
					GESHI_SEARCH => '(abolish|clause|retract|retract(all)?|assert(a|z)|threaded_call)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Control Constructs
		14 => array(
					GESHI_SEARCH => '((ca(ll|tch)|throw))(?=[(])|(true|fail)',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//All solutions
		15 => array(
					GESHI_SEARCH => '(((bag|set)of)|(f(ind|or)all))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	

			//Multi-threading meta-predicates
		16 => array(
					GESHI_SEARCH => '(threaded|threaded_once|threaded_ignore|threaded_exit|threaded_peek|threaded_wait|threaded_notify)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Term unification
		17 => array(
					GESHI_SEARCH => '(unify_with_occurs_check)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),		

			//Atomic term processing 
		18 => array(
					GESHI_SEARCH => '(atom_(length|c(hars|o(ncat|des)))|number_(c(har|ode)s)|char_code)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

		19 => array(
					GESHI_SEARCH => '(functor|arg|copy_term)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Evaluable functors
		20 => array(
					GESHI_SEARCH => '((float_(integer|fractional)_part)|rem|mod|abs|sign|(floor|truncate|round|ceiling))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Other arithmetic functors
		21 => array(
					GESHI_SEARCH => '(cos|atan|exp|log|s(in|qrt))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Term testing
		22 => array(
					GESHI_SEARCH => '(sub_atom|var|atom(ic)?|integer|float|compound|n(onvar|umber))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Stream selection and control
		23 => array(
					GESHI_SEARCH => '(((current|set)_(in|out)put)|open|close|flush_output|stream_property|at_end_of_stream|set_stream_position)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

		24 => array(
					GESHI_SEARCH => '\b(at_end_of_stream|flush_output)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	

			//Character and byte input/output
		25 => array(
					GESHI_SEARCH => '((get|p(eek|ut))_(byte|c(har|ode))|nl)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

		26 => array(
					GESHI_SEARCH => '\b(nl)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	

			// Implementation defined hooks functions 		
		27 => array(
					GESHI_SEARCH => '\b(halt)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

		28  => array(
					GESHI_SEARCH => '(halt)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Logic and control	
		29 => array(
					GESHI_SEARCH => '(once)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	

		30 => array(
					GESHI_SEARCH => '\b(repeat)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Arithemtic evaluation
		31 => array(
					GESHI_SEARCH => '\b(is)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),	

		32 => array(
					GESHI_SEARCH => '\b(mod|rem)\b',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),
		
			//Term input/output		
		33 => array(
					GESHI_SEARCH => '((current_)?op|(write(q|_(canonical|term))?)|(read(_term)?)|((current_)?char_conversion))(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Entity directives
		34 => array(
					GESHI_SEARCH => '(\s*:-\s)(protocol|category|object)(?=[(])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),

			//Predicate scope directives
		35 => array(
					GESHI_SEARCH => '(\s*:-\s)(p(ublic|r(otected|ivate)))(?=[(])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),

			//Other directives
		36 => array(
					GESHI_SEARCH => '(\s*:-\s)(mode|alias|(e(ncoding|xport))|synchronized|alias|initialization|info|module|d(ynamic|iscontiguous)|op|(m(eta_predicate|ultifile))|calls|(use(s|_module)))(?=[(])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),

			//Entity directives			
		37 => array(
					GESHI_SEARCH => '(\s*:-\s)(end_(object|protocol|category)|threaded|synchronized|dynamic)(?=[.])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),
		
			//Entity Relations
		38 => array(
					GESHI_SEARCH => '((i(mp(orts|lements)|nstantiates))|complements|extends|specializes)(?=[(])',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),
					
			// Numbers
		39 => array(
					GESHI_SEARCH => '(\b(0b[0-1]+|0o[0-7]+|0x[0-9a-fA-F]+|\d+\.?\d*((e|E)(\+|-)?\d+)?))',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => ''
					),

			//Message sending operators	
		40 => array(
					GESHI_SEARCH => '([^A-Z0-9]|[^a-z0-9])(::|:-|:|\^\^|//|/\\|--|-|\\\|\?)',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),

			//External call		
		41 => array(
					GESHI_SEARCH => '([^a-zA-Z])(\{|\})',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => ''
					),					

			//Arithemtic comparison	
		42 => array(
					GESHI_SEARCH => '([^a-zA-Z])(,|;|=\.\.|=:=|=\\=|==|\\==|\\=|@|\*\*)',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => '\\3'
					),

			//Term comparison && Bitwise functors	
		43 => array(
					GESHI_SEARCH => '([^a-zA-Z])(&lt;&lt;|&gt;&gt;|&lt;|=&lt;|&gt;=|&gt;|@=&lt;|@&lt;|@&gt;=|@&gt;)',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => '\\3'
					),

			//Mode operators
		44 => array(
					GESHI_SEARCH => '(\[|\]|\(|\))',
					GESHI_REPLACE => '\\1',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '',
					GESHI_AFTER => '\\2'
					),	

			//Evaluable functors	
		45 => array(
					GESHI_SEARCH => '([^a-zA-Z0-9])([+*/-])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => '\\3'
					),

			//Logic and control
		46 => array(
					GESHI_SEARCH => '([^a-zA-Z])(\\\+)',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => '\\3'
					),

			//Control constructs
		47 => array(
					GESHI_SEARCH => '([^\|])(!)([^\>])',
					GESHI_REPLACE => '\\2',
					GESHI_MODIFIERS => '',
					GESHI_BEFORE => '\\1',
					GESHI_AFTER => '\\3'
					),

		),
	'STRICT_MODE_APPLIES' => GESHI_NEVER,
	'SCRIPT_DELIMITERS' => array(),
	'HIGHLIGHT_STRICT_BLOCK' => array(
	)
);

?>
