if (! this.sh_languages) {
  this.sh_languages = {};
}
sh_languages['logtalk'] = [
  [
    [
      /%/g,
      'sh_comment',
      1
    ],
    [
      /\/\*/g,
      'sh_comment',
      2
    ],
    [
      /"/g,
      'sh_string',
      3
    ],
    [
      /'/g,
      'sh_string',
      4
    ],
    [
      /\b(?:abolish|c(?:urrent|reate))_(?:object|protocol|category)(?=\()|\b(?:object|protocol|category)_property(?=\()|\bco(?:mplements_object|nforms_to_protocol)(?=\()|\bextends_(?:object|protocol|category)(?=\()|\bimp(?:lements_protocol|orts_category)(?=\()|\b(?:instantiat|specializ)es_class(?=\()|\b(?:current_event|(?:abolish|define)_events)(?=\()|\b(?:current|set)_logtalk_flag(?=\()|\blogtalk_(?:compile|l(?:oad|oad_context|ibrary_path))(?=\()|\b(?:after|before)(?=\()|\b(?:parameter|this|se(?:lf|nder))(?=\()|\b(?:current_predicate|predicate_property)(?=\()|\b(?:expand_(?:goal|term)|(?:goal|term)_expansion|phrase)(?=\()|\b(?:clause|retract(?:all)?)(?=\()|\ba(?:bolish|ssert(?:a|z))(?=\()|\b(?:ca(?:ll|tch)|throw)(?=\()|\b(?:(?:bag|set)of|f(?:ind|or)all)(?=\()|\bthreaded(?:_(?:call|once|ignore|exit|peek|wait|notify))?(?=\()|\bunify_with_occurs_check(?=\()|\b(?:functor|arg|copy_term|numbervars)(?=\()|\b(?:rem|mod|abs|sign)(?=\()|\b(?:float_(?:integer|fractional)_part|float)(?=\()|\b(?:floor|truncate|round|ceiling)(?=\()|\b(?:cos|atan|exp|log|s(?:in|qrt))(?=\()|\b(?:var|atom(?:ic)?|integer|float|c(?:allable|ompound)|n(?:onvar|umber)|ground)(?=\()|\bcompare(?=\()|\b(?:curren|se)t_(?:in|out)put(?=\()|\b(?:open|close)(?=\()|\bflush_output(?=\()|\b(?:flush_output|at_end_of_stream)\b|\b(?:stream_property|at_end_of_stream|set_stream_position)(?=\()|\b(?:get|p(?:eek|ut))_(?:byte|c(?:har|ode))(?=\()|\bnl(?=\()|\b(?:nl)\b|\b(?:read(?:_term)?)(?=\()|\b(?:write(?:q|_(?:canonical|term))?)(?=\()|\b(?:op|current_op)(?=\()|\b(?:(?:current_)?char_conversion)(?=\()|\batom_(?:length|c(?:hars|o(?:ncat|des)))(?=\()|\b(?:char_code|sub_atom)(?=\()|\bnumber_(?:c(?:har|ode)s)(?=\()|\b(?:set|current)_prolog_flag(?=\()|\bhalt(?=\()|\b(?:halt)\b|\bonce(?=\()|\b(?:(?:key)?sort)(?=\()|\b(?:true|fail|repeat)\b|\b(?:e|pi|is|rem|mod)\b/g,
      'sh_keyword',
      -1
    ],
    [
      /\b[A-Z_][A-Za-z0-9_]*/g,
      'sh_variable',
      -1
    ],
    [
      /\{|\}/g,
      'sh_cbracket',
      -1
    ],
    [
      /^[ \t]*:-[ \t](?:c(?:a(?:lls|tegory)|oinductive)|p(?:ublic|r(?:ot(?:ocol|ected)|ivate))|e(?:l(?:if|se)|n(?:coding|sure_loaded)|xport)|i(?:f|n(?:fo|itialization))|alias|d(?:ynamic|iscontiguous)|m(?:eta_predicate|od(?:e|ule)|ultifile)|reexport|s(?:et_(?:logtalk|prolog)_flag|ynchronized)|o(?:bject|p)|use(?:s|_module))(?=\()|^[ \t]*:-[ \t](?:end(?:if|_(?:category|object|protocol))|dynamic|synchronized|threaded)\.|\b(?:complements|extends|i(?:nstantiates|mp(?:lements|orts))|specializes)(?=\()/g,
      'sh_preproc',
      -1
    ],
    [
      /\b[a-z][A-Za-z0-9_]*/g,
      'sh_normal',
      -1
    ],
    [
      /0'.|0b[0-1]+|0o[0-7]+|0x[0-9a-fA-F]+|[0-9]+(?:\.[0-9]+)?(?:[eE](?:[-+])?[0-9]+)?/g,
      'sh_number',
      -1
    ],
    [
      /\^|::|\^\^|:|>>|<<|\/\\|\\\/|\\|=:=|=\\=|<|=<|>|>=|=\.\.|\\==|==|@=<|@<|@>=|@>|=|\\=|\+|-|\*\*|\*|\/\/|\/|-->|!|->|;|\\\+|\?|@|:-/g,
      'sh_symbol',
      -1
    ]
  ],
  [
    [
      /$/g,
      null,
      -2
    ]
  ],
  [
    [
      /\*\//g,
      'sh_comment',
      -2
    ],
    [
      /\/\*/g,
      'sh_comment',
      2
    ]
  ],
  [
    [
      /$/g,
      null,
      -2
    ],
    [
      /\\(?:\\|")/g,
      null,
      -1
    ],
    [
      /"/g,
      'sh_string',
      -2
    ]
  ],
  [
    [
      /'/g,
      'sh_string',
      -2
    ],
    [
      /\\(?:[\\abfnrtv"']|(?:x[a-fA-F0-9]+|[0-7]+)\\)/g,
      'sh_specialchar',
      -1
    ]
  ]
];
