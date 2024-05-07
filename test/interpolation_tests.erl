-module(interpolation_tests).
-include_lib("eunit/include/eunit.hrl").

-import(templaterl, [render_string/1, render_string/2]).

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

%% context misses

basic_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{cannot}}) be seen!", #{})).

triple_mustache_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{{cannot}}}) be seen!", #{})).

ampersand_context_miss_interpolation_test_() ->
    ?_assertEqual("I () be seen!",
                  render_string("I ({{&cannot}}) be seen!", #{})).

% TODO: pick it up here
