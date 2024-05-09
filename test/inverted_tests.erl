-module(inverted_tests).
-include_lib("eunit/include/eunit.hrl").

-import(templaterl, [render_string/2]).

% expected: 22

falsey_test_() ->
    ?_assertEqual("\"This should be rendered.\"",
                  render_string("\"{{^boolean}}This should be rendered."
                                "{{/boolean}}\"",
                                #{boolean => false})).

truthy_test_() ->
    ?_assertEqual("\"\"",
                  render_string("\"{{^boolean}}This should not be rendered."
                                "{{/boolean}}\"",
                                #{boolean => true})).

null_is_falsey_test_() ->
    ?_assertEqual("\"This should be rendered.\"",
                  render_string("\"{{^null}}This should be rendered."
                                "{{/null}}\"",
                                #{null => null})).

context_test_() ->
    ?_assertEqual("\"\"",
                  render_string("\"{{^context}}Hi {{name}}.{{/context}}\"",
                                #{context => #{name => "Joe"}})).

list_test_() ->
    ?_assertEqual("\"\"",
                  render_string("\"{{^list}}{{n}}{{/list}}\"",
                                #{list =>[#{n => 1 }, #{n => 2}, #{n => 3}]})).

empty_list_test_() ->
    ?_assertEqual("\"Yay lists!\"",
                  render_string("\"{{^list}}Yay lists!{{/list}}\"",
                                #{list => []})).

doubled_test_() ->
    ?_assertEqual("* first\n"
                  "* second\n"
                  "* third\n",
                  render_string("{{^bool}}\n"
                                "* first\n"
                                "{{/bool}}\n"
                                "* {{two}}\n"
                                "{{^bool}}\n"
                                "* third\n"
                                "{{/bool}}",
                                #{bool => false, two => <<"second">>})).

nested_falsey_test_() ->
    ?_assertEqual("| A B C D E |",
                  render_string("| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} "
                                "E |",
                                #{bool => false})).

nested_truthy_test_() ->
    ?_assertEqual("| A  E |",
                  render_string("| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} "
                                "E |",
                                #{bool => true})).

context_misses_test_() ->
    ?_assertEqual("[Cannot find key 'missing'!]",
                  render_string("[{{^missing}}Cannot find key 'missing'!"
                                "{{/missing}}]",
                                #{})).

% dotted names

dotted_names_truthy_test_() ->
    ?_assertEqual("\"\" == \"\"",
                  render_string("\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\"",
                                #{a => #{b => #{c => true}}})).

dotted_names_falsey_test_() ->
    ?_assertEqual("\"Not Here\" == \"Not Here\"",
                  render_string("\"{{^a.b.c}}Not Here{{/a.b.c}}\" == "
                                "\"Not Here\"",
                                #{a => #{b => #{c => false}}})).

dotted_names_broken_chains_test_() ->
    ?_assertEqual("\"Not Here\" == \"Not Here\"",
                  render_string("\"{{^a.b.c}}Not Here{{/a.b.c}}\" == "
                                "\"Not Here\"",
                                #{a => #{}})).

% whitespace sensitivity

surrounding_whitespace_test_() ->
    ?_assertEqual(" | \t|\t | \n",
                  render_string(" | {{^boolean}}\t|\t{{/boolean}} | \n",
                                #{boolean => false})).

internal_whitespace_test_() ->
    ?_assertEqual(" |  \n  | \n",
                  render_string(" | {{^boolean}} {{! Important Whitespace }}\n "
                                "{{/boolean}} | \n",
                                #{boolean => false})).

indented_inline_sections_test_() ->
    ?_assertEqual(" NO\n WAY\n",
                  render_string(" {{^boolean}}NO{{/boolean}}\n {{^boolean}}WAY"
                                "{{/boolean}}\n",
                                #{boolean => false})).

standalone_lines_test_() ->
    ?_assertEqual("| This Is\n"
                  "|\n"
                  "| A Line\n",
                  render_string("| This Is\n"
                                "{{^boolean}}\n"
                                "|\n"
                                "{{/boolean}}\n"
                                "| A Line\n",
                                #{boolean => false})).

standalone_indented_lines_test_() ->
    ?_assertEqual("| This Is\n"
                  "|\n"
                  "| A Line\n",
                  render_string("| This Is\n"
                                "  {{^boolean}}\n"
                                "|\n"
                                "  {{/boolean}}\n"
                                "| A Line\n",
                                #{boolean => false})).

standalone_line_endings_test_() ->
    ?_assertEqual("|\r\n|",
                  render_string("|\r\n{{^boolean}}\r\n{{/boolean}}\r\n|",
                                #{boolean => false})).

standalone_without_previous_line_test_() ->
    ?_assertEqual("^\n/",
                  render_string("  {{^boolean}}\n^{{/boolean}}\n/",
                                #{boolean => false})).

standalone_without_newline_test_() ->
    ?_assertEqual("^\n/\n",
                  render_string("^{{^boolean}}\n/\n  {{/boolean}}",
                                #{boolean => false})).

% whitespace insensitivity

padding_test_() ->
    ?_assertEqual("|=|",
                  render_string("|{{^ boolean }}={{/ boolean }}|",
                                #{boolean => false})).
