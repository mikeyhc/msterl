-module(partials_tests).
-include_lib("eunit/include/eunit.hrl").

-import(templaterl, [render_string/3]).

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
