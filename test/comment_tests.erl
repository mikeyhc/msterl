-module(comment_tests).
-include_lib("eunit/include/eunit.hrl").

-import(mst, [render_string/1, render_string/2]).

% expected: 12

inline_test_() ->
    ?_assertEqual("1234567890",
                  render_string("12345{{! Comment Block! }}67890")).

multiline_test_() ->
    ?_assertEqual("1234567890",
                  render_string("12345{{!\nThis is a\nmulti-line comment...\n"
                                "\n}}67890")).

standalone_test_() ->
    ?_assertEqual("Begin.\nEnd.",
                  render_string("Begin.\n{{! Comment Block! }}\nEnd.")).

indented_standalone_test_() ->
    ?_assertEqual("Begin.\nEnd.",
                  render_string("Begin.\n  {{! Indented Comment Block! }}"
                                "\nEnd.")).

standalone_line_endings_test_() ->
    ?_assertEqual("|\r\n|",
                  render_string("|\r\n{{! Standalone Comment }}\r\n|")).

standalone_without_previous_line_test_() ->
    ?_assertEqual("!", render_string("  {{! I'm Still Standalone }}\n!")).

standalone_without_newline_test_() ->
    ?_assertEqual("!\n", render_string("!\n  {{! I'm Still Standalone }}")).

multiline_standalone_test_() ->
    ?_assertEqual("Begin.\nEnd.",
                  render_string("Begin.\n{{!\n"
                                "Something's going on here...\n"
                                "}}\nEnd.")).

indented_multiline_standalone_test_() ->
    ?_assertEqual("Begin.\nEnd.",
                  render_string("Begin.\n  {{!\n"
                                "    Something's going on here...\n"
                                "  }}\nEnd.")).

indented_inline_test_() ->
    ?_assertEqual("  12 \n", render_string("  12 {{! 34 }}\n")).

surrounding_whitespace_test_() ->
    ?_assertEqual("12345  67890",
                  render_string("12345 {{! Comment Block! }} 67890")).

variable_name_collision_test_() ->
    ?_assertEqual("comments never show: ><",
                  render_string("comments never show: >{{! comment }}<",
                                #{"! comment" => 1,
                                  "! comment " => 2,
                                  "!comment" => 3,
                                  "comment" => 4})).
