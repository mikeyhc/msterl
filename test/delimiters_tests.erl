-module(delimiters_tests).
-include_lib("eunit/include/eunit.hrl").

-import(mst, [render_string/2]).

% expected: 14

pair_behaviour_test_() ->
    ?_assertEqual("(Hey!)",
                  render_string("{{=<% %>=}}(<%text%>)",
                                #{text => <<"Hey!">>})).
