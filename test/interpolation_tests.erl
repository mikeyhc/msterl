-module(interpolation_tests).
-include_lib("eunit/include/eunit.hrl").

-import(mst, [render_string/1, render_string/2]).

% expected: 40

no_interpolation_test_() ->
    ?_assertEqual("Hello from {Mustache}!",
                  render_string("Hello from {Mustache}!")).

basic_interpolation_test_() ->
    ?_assertEqual("Hello, world!",
                  render_string("Hello, {{subject}}!",
                                #{subject => <<"world">>})).

no_reinterpolation_test_() ->
    ?_assertEqual("{{planet}}: Earth",
                  render_string("{{template}}: {{planet}}",
                                #{template => <<"{{planet}}">>,
                                  planet => <<"Earth">>})).

html_escaping_test_() ->
    ?_assertEqual("These characters should be HTML escaped: &amp; &quot; &lt; "
                  "&gt;",
                  render_string("These characters should be HTML escaped: "
                                "{{forbidden}}",
                                #{forbidden => <<"& \" < >">>})).

triple_mustache_test_() ->
    ?_assertEqual("These characters should not be HTML escaped: & \" < >",
                  render_string("These characters should not be HTML escaped: "
                                "{{{forbidden}}}",
                                #{forbidden => <<"& \" < >">>})).

ampersand_test_() ->
    ?_assertEqual("These characters should not be HTML escaped: & \" < >",
                  render_string("These characters should not be HTML escaped: "
                                "{{&forbidden}}",
                                #{forbidden => <<"& \" < >">>})).

basic_integer_interpolation_test_() ->
    ?_assertEqual("85 miles an hour!",
                  render_string("{{mph}} miles an hour!",
                                #{mph => 85})).

triple_mustache_integer_interpolation_test_() ->
    ?_assertEqual("85 miles an hour!",
                  render_string("{{{mph}}} miles an hour!",
                                #{mph => 85})).

ampersand_integer_interpolation_test_() ->
    ?_assertEqual("85 miles an hour!",
                  render_string("{{&mph}} miles an hour!",
                                #{mph => 85})).

basic_decimal_interpolation_test_() ->
    ?_assertEqual("1.21 jiggawatts!",
                  render_string("{{power}} jiggawatts!",
                                #{power => 1.210})).

triple_mustache_decimal_interpolation_test_() ->
    ?_assertEqual("1.21 jiggawatts!",
                  render_string("{{{power}}} jiggawatts!",
                                #{power => 1.210 })).

ampersand_decimal_interpolation_test_() ->
    ?_assertEqual("1.21 jiggawatts!",
                  render_string("{{&power}} jiggawatts!",
                                #{power => 1.210 })).

basic_null_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{cannot}}) be seen!",
                                #{cannot => null })).

triple_mustache_null_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{{cannot}}}) be seen!",
                                #{cannot => null })).

ampersand_null_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{&cannot}}) be seen!",
                                #{cannot => null })).

% context misses

basic_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{cannot}}) be seen!", #{})).

triple_mustache_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{{cannot}}}) be seen!", #{})).

ampersand_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{&cannot}}) be seen!", #{})).

% dotted names

dotted_names_basic_interpolation_test_() ->
    ?_assertEqual("\"Joe\" == \"Joe\"",
                  render_string("\"{{person.name}}\" == \"{{#person}}"
                                "{{name}}{{/person}}\"",
                                #{person => #{ name => <<"Joe">>}})).

dotted_names_triple_mustache_interpolation_test_() ->
    ?_assertEqual("\"Joe\" == \"Joe\"",
                  render_string("\"{{{person.name}}}\" == \"{{#person}}"
                                "{{{name}}}{{/person}}\"",
                                #{person => #{ name => <<"Joe">>}})).

dotted_names_ampersand_interpolation_test_() ->
    ?_assertEqual("\"Joe\" == \"Joe\"",
                  render_string("\"{{&person.name}}\" == \"{{#person}}"
                                "{{&name}}{{/person}}\"",
                                #{person => #{ name => <<"Joe">>}})).

dotted_names_arbitrary_depth_test_() ->
    ?_assertEqual("\"Phil\" == \"Phil\"",
                  render_string("\"{{a.b.c.d.e.name}}\" == \"Phil\"",
                                #{a =>
                                  #{b =>
                                    #{c =>
                                      #{d =>
                                        #{e => #{name => <<"Phil">>}}}}}})).

dotted_names_broken_chains_test_() ->
    ?_assertEqual("\"\" == \"\"",
                  render_string("\"{{a.b.c}}\" == \"\"",
                                #{a => #{}})).

dotted_names_broken_chains_resolution_test_() ->
    ?_assertEqual("\"\" == \"\"",
                  render_string("\"{{a.b.c.name}}\" == \"\"",
                                #{a => #{b => #{}},
                                  c => #{name => <<"Jim">>}})).

dotted_names_initial_resolution_test_() ->
    ?_assertEqual("\"Phil\" == \"Phil\"",
                  render_string("\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\"",
                                #{a =>
                                  #{b =>
                                    #{c =>
                                      #{d => #{e => #{name => <<"Phil">>}}}}},
                                  b =>
                                   #{c =>
                                     #{d => #{e => #{name => <<"Wrong">>}}}}})).

dotted_names_context_precedence_test_() ->
    ?_assertEqual("", render_string("{{#a}}{{b.c}}{{/a}}",
                                    #{a => #{b => #{}},
                                      b => #{c => <<"ERROR">>}})).

% implicit iterators

implicit_iterators_basic_interpolation_test_() ->
    ?_assertEqual("Hello, world!",
                  render_string("Hello, {{.}}!",
                                <<"world">>)).

implicit_iterators_html_escaping_test_() ->
    ?_assertEqual("These characters should be HTML escaped: &amp; &quot; &lt; "
                  "&gt;",
                  render_string("These characters should be HTML escaped: "
                                "{{.}}",
                                <<"& \" < >">>)).

implicit_iterators_triple_mustache_test_() ->
    ?_assertEqual("These characters should not be HTML escaped: & \" < >",
                  render_string("These characters should not be HTML escaped: "
                                "{{{.}}}",
                                <<"& \" < >">>)).

implicit_iterators_ampersand_test_() ->
    ?_assertEqual("These characters should not be HTML escaped: & \" < >",
                  render_string("These characters should not be HTML escaped: "
                                "{{&.}}",
                                <<"& \" < >">>)).

implicit_iterators_basic_integer_interpolation_test_() ->
    ?_assertEqual("\"85 miles an hour!\"",
                  render_string("\"{{.}} miles an hour!\"", 85)).

% whitespace sensitivity

interpolation_surrounding_whitespace_test_() ->
    ?_assertEqual("| --- |",
                  render_string("| {{string}} |",
                                #{string => <<"---">>})).

triple_mustache_surrounding_whitespace_test_() ->
    ?_assertEqual("| --- |",
                  render_string("| {{{string}}} |",
                                #{string => <<"---">>})).

ampersand_surrounding_whitespace_test_() ->
    ?_assertEqual("| --- |",
                  render_string("| {{&string}} |",
                                #{string => <<"---">>})).

interpolation_standalone_test_() ->
    ?_assertEqual("  ---\n",
                  render_string("  {{string}}\n",
                                #{string => <<"---">>})).

triple_mustache_standalone_test_() ->
    ?_assertEqual("  ---\n",
                  render_string("  {{&string}}\n",
                                #{string => <<"---">>})).

ampersand_standalone_test_() ->
    ?_assertEqual("  ---\n",
                  render_string("  {{&string}}\n",
                                #{string => <<"---">>})).

% whitespace insensitivity

interpolation_with_padding_test_() ->
    ?_assertEqual("|---|",
                  render_string("|{{ string }}|",
                                #{string => <<"---">>})).

triple_mustache_with_padding_test_() ->
    ?_assertEqual("|---|",
                  render_string("|{{{ string }}}|",
                                #{string => <<"---">>})).

ampersand_with_padding_test_() ->
    ?_assertEqual("|---|",
                  render_string("|{{& string }}|",
                                #{string => <<"---">>})).
