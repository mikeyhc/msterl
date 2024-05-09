-module(partials_tests).
-include_lib("eunit/include/eunit.hrl").

-import(mst, [render_string/3]).

% expected: 12

basic_behaviour_test_() ->
    ?_assertEqual("\"from partial\"",
                  render_string("\"{{>text}}\"",
                                #{},
                                #{text => "from partial"})).

failed_lookup_test_() ->
    ?_assertEqual("\"\"", render_string("\"{{>text}}\"", #{}, #{})).

context_test_() ->
    ?_assertEqual("\"*content*\"",
                  render_string("\"{{>partial}}\"",
                                #{text => <<"content">>},
                                #{partial => "*{{text}}*"})).

recursion_test_() ->
    ?_assertEqual("X<Y<>>",
                  render_string("{{>node}}",
                                #{content => <<"X">>,
                                  nodes => [#{content => <<"Y">>,
                                              nodes => []}]},
                                #{node => "{{content}}<{{#nodes}}{{>node}}"
                                  "{{/nodes}}>"})).

nested_test_() ->
    ?_assertEqual("*hello world!*",
                  render_string("{{>outer}}",
                                #{a => <<"hello">>, b => <<"world">>},
                                #{outer => "*{{a}} {{>inner}}*",
                                  inner => "{{b}}!"})).

% whitespace sensitivity

surrounding_whitespace_test_() ->
    ?_assertEqual("| \t|\t |",
                  render_string("| {{>partial}} |",
                                #{},
                                #{partial => "\t|\t"})).

inline_indentation_test_() ->
    ?_assertEqual("  |  >\n>\n",
                  render_string("  {{data}}  {{> partial}}\n",
                                #{data => <<"|">>},
                                #{partial => ">\n>" })).

standalone_line_endings_test_() ->
    ?_assertEqual("|\r\n>|",
                  render_string("|\r\n{{>partial}}\r\n|",
                                #{},
                                #{partial => ">"})).

standalone_without_previous_line_test_() ->
    ?_assertEqual("  >\n  >>",
                  render_string("  {{>partial}}\n>",
                                #{},
                                #{partial => ">\n>"})).

standalone_without_newline_test_() ->
    ?_assertEqual(">\n  >\n  >",
                  render_string(">\n  {{>partial}}",
                                #{},
                                #{partial => ">\n>" })).

standalone_indentation_test_() ->
    ?_assertEqual("\\\n"
                  " |\n"
                  " <\n"
                  "->\n"
                  " |\n"
                  "/",
                  render_string("\\\n"
                                " {{>partial}}\n"
                                "/",
                                #{content => <<"<\n->">> },
                                #{partial => "|\n{{{content}}}\n|\n"})).

% whitespace insensitivity

padding_whitespace_test_() ->
    ?_assertEqual("|[]|", render_string("|{{> partial }}|",
                                        #{boolean => true},
                                        #{partial => "[]"})).
