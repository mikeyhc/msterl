-module(delimiters_tests).
-include_lib("eunit/include/eunit.hrl").

-import(mst, [render_string/2, render_string/3]).

% expected: 14

pair_behaviour_test_() ->
    ?_assertEqual("(Hey!)",
                  render_string("{{=<% %>=}}(<%text%>)",
                                #{text => <<"Hey!">>})).

special_characters_test_() ->
    ?_assertEqual("(It worked!)",
                  render_string("({{=[ ]=}}[text])",
                                #{text => <<"It worked!">>})).

sections_test_() ->
    ?_assertEqual("[\n"
                  "  I got interpolated.\n"
                  "  |data|\n"
                  "\n"
                  "  {{data}}\n"
                  "  I got interpolated.\n"
                  "]\n",
                  render_string("[\n"
                                "{{#section}}\n"
                                "  {{data}}\n"
                                "  |data|\n"
                                "{{/section}}\n"
                                "\n"
                                "{{= | | =}}\n"
                                "|#section|\n"
                                "  {{data}}\n"
                                "  |data|\n"
                                "|/section|\n"
                                "]\n",
                                #{section => true,
                                  data => <<"I got interpolated.">>
                                 })).

inverted_sections_test_() ->
    ?_assertEqual("[\n"
                  "  I got interpolated.\n"
                  "  |data|\n"
                  "\n"
                  "  {{data}}\n"
                  "  I got interpolated.\n"
                  "]\n",
                  render_string("[\n"
                                "{{^section}}\n"
                                "  {{data}}\n"
                                "  |data|\n"
                                "{{/section}}\n"
                                "\n"
                                "{{= | | =}}\n"
                                "|^section|\n"
                                "  {{data}}\n"
                                "  |data|\n"
                                "|/section|\n"
                                "]\n",
                                #{section => false,
                                  data => <<"I got interpolated.">>
                                 })).

partial_inheritence_test_() ->
    ?_assertEqual("[ .yes. ]\n"
                  "[ .yes. ]\n",
                  render_string("[ {{>include}} ]\n"
                                "{{= | | =}}\n"
                                "[ |>include| ]\n",
                                #{value => <<"yes">>},
                                #{include => ".{{value}}."})).

post_partial_behaviour_test_() ->
    ?_assertEqual("[ .yes.  .yes. ]\n"
                  "[ .yes.  .|value|. ]\n",
                  render_string("[ {{>include}} ]\n"
                                "[ .{{value}}.  .|value|. ]\n",
                                #{value => <<"yes">>},
                                #{include =>
                                  ".{{value}}. {{= | | =}} .|value|."})).

% whitespace sensitivity

surrounding_whitespace_test_() ->
    ?_assertEqual("|  |",
                  render_string("| {{=@ @=}} |", #{})).

outlying_whitespace_inline_test_() ->
    ?_assertEqual(" | \n",
                  render_string(" | {{=@ @=}}\n", #{})).

standalone_tag_test_() ->
    ?_assertEqual("Begin.\n"
                  "End.\n",
                  render_string("Begin.\n"
                                "{{=@ @=}}\n"
                                "End.\n",
                                #{})).

indented_standalone_tag_test_() ->
    ?_assertEqual("Begin.\n"
                  "End.\n",
                  render_string("Begin.\n"
                                "  {{=@ @=}}\n"
                                "End.\n",
                                #{})).

standalone_line_endings_test_() ->
    ?_assertEqual("|\r\n|",
                  render_string("|\r\n{{= @ @ =}}\r\n|", #{})).

standalone_without_previous_line_test_() ->
    ?_assertEqual("=", render_string("  {{=@ @=}}\n=", #{})).

standalone_without_newline_test_() ->
    ?_assertEqual("=\n", render_string("=\n  {{=@ @=}}", #{})).

% whitespace insensitivity

pair_with_padding_test_() ->
    ?_assertEqual("||", render_string("|{{= @   @ =}}|", #{})).
