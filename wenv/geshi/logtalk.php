<?php

/*************************************************************************************
 * logtalk.php
 * -----------
 * 
 * Author: Paulo Moura <pmoura@logtalk.org>
 *
 * Copyright: (c) 2009 Paulo Moura
 *
 * Logtalk language file for GeSHi.
 *
 *
 * Based on GeSHi release 1.0.8.4
 * Last Change:	October 25, 2009
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
 
$language_data = array(
	'LANG_NAME' => 'Logtalk',
	'COMMENT_SINGLE' => array(1 => '%'),
	'COMMENT_MULTI' => array('/*' => '*/'),
	'COMMENT_REGEXP' => array(2 => "/0'./sim"),
	'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
	'QUOTEMARKS' => array('"'),
    'HARDQUOTE' => array("'", "'"),
    'HARDESCAPE' => array("'", "\\"),
	'ESCAPE_CHAR' => array(),
//    'NUMBERS' => GESHI_NEVER,
	'NUMBERS' => GESHI_NUMBER_INT_BASIC | GESHI_NUMBER_FLT_SCI_ZERO,
	'KEYWORDS' => array(
		// Directives (with arguments)
		1 => array(
			// file directives
			'encoding', 'ensure_loaded',
			// entity opening directives
			'category', 'object', 'protocol',
			// predicate scope directives
			'public', 'protected', 'pivate',
			// conditional compilation directives
			'elif', 'if',
			// entity directives
			'calls', 'initialization', 'op', 'uses',
			// predicate directives			
			'alias', 'discontiguous', 'dynamic', 'mode', 'info', 'meta_predicate', 'multifile', 'synchronized',
			// module directives
			'export', 'module', 'reexport', 'use_module'
			),
		// Directives (no arguments)
		2 => array(
			// entity directives
			'dynamic',
			// multi-threading directives
			'synchronized', 'threaded',
			// entity closing directives
			'end_category', 'end_object', 'end_protocol',
			// conditional compilation directives
			'else', 'endif',
			// flag directives
			'set_logtalk_flag', 'set_prolog_flag'
			),
		// Entity relations
		3 => array(
			'complements', 'extends', 'imports', 'implements','instantiates', 'specializes'
			),
		// Built-in predicates (with arguments)
		4 => array(
			// event handlers
			'after', 'before',
			// execution-context methods
			'parameter', 'self', 'sender', 'this',
			// predicate reflection
			'current_predicate', 'predicate_property',
			// DCGs and term expansion
			'expand_goal', 'expand_term', 'goal_expansion', 'phrase', 'term_expansion',
			// entity
			'abolish_category', 'abolish_object', 'abolish_protocol',
			'create_category', 'create_object', 'create_protocol',
			'current_category', 'current_object', 'current_protocol',
			'category_property', 'object_property', 'protocol_property',
			// entity relations
			'complements_object',
			'extends_category', 'extends_object', 'extends_protocol',
			'implements_protocol', 'imports_category',
			'instantiates_class', 'specializes_class',
			// events
			'abolish_events', 'current_event', 'define_events',
			// flags
			'current_logtalk_flag', 'set_logtalk_flag',
			'current_prolog_flag', 'set_prolog_flag',
			// compiling, loading, and library path
			'logtalk_compile', 'logtalk_library_path', 'logtalk_load',
			// database
			'abolish', 'asserta', 'assertz', 'clause', 'retract', 'retractall',
			// control
			'call', 'catch', 'once', 'throw',
			// all solutions predicates
			'bagof', 'findall', 'forall', 'setof',
			// multi-threading meta-predicates
			'threaded',
			'threaded_call', 'threaded_once', 'threaded_ignore', 'threaded_exit', 'threaded_peek',
			'threaded_wait', 'threaded_notify',
			// term unification
			'unify_with_occurs_check',
			// atomic term processing 
			'atom_chars', 'atom_codes', 'atom_concat', 'atom_length',
			'number_chars', 'number_codes',
			'char_code',
			// term creation and decomposition
			'arg', 'copy_term', 'functor',
			// term testing
			'atom', 'atomic', 'compound', 'float', 'integer', 'nonvar', 'number', 'sub_atom', 'var',
			// stream selection and control
			'current_input', 'current_output', 'set_input', 'set_output',
			'open', 'close', 'flush_output', 'stream_property',
			'at_end_of_stream', 'set_stream_position',
			// stream selection and control predicates
			'at_end_of_stream', 'flush_output',
			// character and byte input/output predicates
			'get_byte', 'get_char', 'get_code',
			'peek_byte', 'peek_char', 'peek_code',
			'put_byte', 'put_char', 'put_code',
			'nl',
			// term input/output predicates
			'current_op', 'op',
			'write', 'writeq', 'write_canonical', 'write_term',
			'read', 'read_term',
			'char_conversion', 'current_char_conversion',
			//
			'halt'
			),
		// Built-in predicates (no arguments)
		5 => array(
			// control
			'fail', 'repeat', 'true',
			// character and byte input/output predicates
			'nl',
			// implementation defined hooks functions 		
			'halt',
			// arithemtic evaluation
			'is',
			// stream selection and control
			'at_end_of_stream', 'flush_output'
			),
		// Evaluable functors (with arguments)
		6 => array(
			'float_integer_part', 'float_fractional_part',
			'rem', 'mod', 'abs', 'sign', 'floor', 'truncate', 'round', 'ceiling',
			'cos', 'atan', 'exp', 'log', 'sin', 'sqrt'
			),
		// Evaluable functors (no arguments)
		7 => array(
			'mod', 'rem'
			),
		),
    'SYMBOLS' => array(
		// external call
		'{', '}',
		// arithemtic comparison
		'=:=', '=\=', '<', '=<', '>=', '>',
		// term comparison
		'<<', '>>', '/\\', '\\/', '\\',
		// bitwise functors
		'==', '\==', '@<', '@=<', '@>=', '@>',
		// evaluable functors	
		'+', '-', '*', '/', '**',
		// logic and control
		'!', '\\+', ';',
		// message sending operators	
		'::', '^^', ':',
		// clause and directive functors
		'-->', '->', ':-',
		// mode operators
		'@', '?',
		// term to list predicate
		'=..',
		// unification
		'=', '\\='
        ),
	'CASE_SENSITIVE' => array(
		GESHI_COMMENTS => false
		),
	'STYLES' => array(
        'KEYWORDS' => array(
			1 => 'color: #186895;',
			2 => 'color: #186895;',
			3 => 'color: #186895;',
			4 => 'color: #ff4e18;',
			5 => 'color: #ff4e18;',
			6 => 'color: #9d4f37;',
			7 => 'color: #9d4f37;'
			),
		'NUMBERS' => array(
			0 => 'color: #40a070;'
			),
		'COMMENTS' => array(
			1 => 'color: #60a0b0; font-style: italic;',
			2 => 'color: #40a070;',
			'MULTI' => 'color: #60a0b0; font-style: italic;'
			),
		'ESCAPE_CHAR' => array(
            'HARD' => 'color: #0000ff; font-weight: bold;'
			),
		'SYMBOLS' => array(
			0 => 'color: #666666;font-weight: bold;'
			),
		'BRACKETS' => array(
			),
		'STRINGS' => array(
			0 => 'color: #0000ff;',
            'HARD' => 'color: #0000ff;'
			),
		'METHODS' => array(
			),
		'REGEXPS' => array(
			0 => 'color: #40a070;',
			1 => 'color: #45b3e6;'
			),
		'SCRIPT' => array()
		),
	'URLS' => array(),
	'OOLANG' => false,
	'OBJECT_SPLITTERS' => array(
		1 => '::'
		),
	'REGEXPS' => array(
		// numbers (binary, octal, hexadecimal, and decimal)
		0 => array(
			GESHI_SEARCH => '(\b(0b[0-1]+|0o[0-7]+|0x[0-9a-fA-F]+))',
//			GESHI_SEARCH => '(\b(0b[0-1]+|0o[0-7]+|0x[0-9a-fA-F]+|\d+\.?\d*((e|E)(\+|-)?\d+)?))',
			GESHI_REPLACE => '\\1',
			GESHI_MODIFIERS => '',
			GESHI_BEFORE => '',
			GESHI_AFTER => ''
			),
		// variables
		1 => '\b(?!(?:PIPE|SEMI|REG3XP\d*)[^a-zA-Z0-9_])[A-Z_][a-zA-Z0-9_]*(?![a-zA-Z0-9_])'
		),
	'STRICT_MODE_APPLIES' => GESHI_NEVER,
	'SCRIPT_DELIMITERS' => array(),
	'HIGHLIGHT_STRICT_BLOCK' => array(),
    'TAB_WIDTH' => 4,
    'PARSER_CONTROL' => array(
        'KEYWORDS' => array(
            1 => array(
                'DISALLOWED_BEFORE' => '(?<=:-\s)',
                'DISALLOWED_AFTER' => '(?=\s*\()'
            ),
            2 => array(
                'DISALLOWED_BEFORE' => '(?<=:-\s)',
                'DISALLOWED_AFTER' => '(?=\.)'
            ),
            3 => array(
                'DISALLOWED_BEFORE' => '(?<=[^a-z])',
                'DISALLOWED_AFTER' => '(?=\s*\()'
            ),
            4 => array(
                'DISALLOWED_BEFORE' => '(?<=[^a-z])',
                'DISALLOWED_AFTER' => '(?=\s*\()'
            ),
            5 => array(
                'DISALLOWED_BEFORE' => '(?<=[^a-z])',
                'DISALLOWED_AFTER' => '(?=[^a-z])'
            ),
            6 => array(
                'DISALLOWED_BEFORE' => '',
                'DISALLOWED_AFTER' => '(?=\s*\()'
            ),
            7 => array(
                'DISALLOWED_BEFORE' => '(?<=[^a-z])',
                'DISALLOWED_AFTER' => '(?=[^a-z])'
            )
        )
    ),
);

?>
