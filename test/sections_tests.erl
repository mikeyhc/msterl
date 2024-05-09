-module(sections_tests).
-include_lib("eunit/include/eunit.hrl").

-import(templaterl, [render_string/2]).

% expected: 34

truthy_test_() ->
    ?_assertEqual("This should be rendered.",
                  render_string("{{#boolean}}This should be rendered."
                                "{{/boolean}}",
                                #{boolean => true})).

falsey_test_() ->
    ?_assertEqual("", render_string("{{#boolean}}This should not be rendered."
                                    "{{/boolean}}",
                                    #{boolean => false})).

null_is_falsey_test_() ->
    ?_assertEqual("", render_string("{{#boolean}}This should not be rendered."
                                    "{{/boolean}}",
                                    #{boolean => null})).

context_test_() ->
    ?_assertEqual("Hi Joe.",
                  render_string("{{#context}}Hi {{name}}.{{/context}}",
                                #{context => #{name => <<"Joe">>}})).

parent_contexts_test_() ->
    ?_assertEqual("foo, bar, baz",
                  render_string("{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}",
                                #{a => <<"foo">>, b => <<"wrong">>,
                                  sec => #{b => <<"bar">>},
                                  c => #{d => <<"baz">>}})).
variable_test_() ->
    ?_assertEqual("bar is bar",
                  render_string("{{#foo}}{{.}} is {{foo}}{{/foo}}",
                                #{foo => <<"bar">>})).

list_contexts_test_() ->
    ?_assertEqual("a1.A1x.A1y.",
                  render_string("{{#tops}}{{#middles}}{{tname.lower}}{{mname}}."
                                "{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}."
                                "{{/bottoms}}{{/middles}}{{/tops}}",
                                #{tops =>
                                  [#{tname => #{upper => <<"A">>,
                                                lower => <<"a">>
                                               },
                                     middles =>
                                     [#{mname => <<"1">>,
                                        bottoms => [#{bname => <<"x">>},
                                                    #{bname => <<"y">>}]
                                       }]
                                    }]
                                 })).

deeply_nexsts_contexts_test_() ->
    T = "{{#a}}\n"
    "{{one}}\n"
    "{{#b}}\n"
    "{{one}}{{two}}{{one}}\n"
    "{{#c}}\n"
    "{{one}}{{two}}{{three}}{{two}}{{one}}\n"
    "{{#d}}\n"
    "{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n"
    "{{#five}}\n"
    "{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n"
    "{{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}\n"
    "{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n"
    "{{/five}}\n"
    "{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n"
    "{{/d}}\n"
    "{{one}}{{two}}{{three}}{{two}}{{one}}\n"
    "{{/c}}\n"
    "{{one}}{{two}}{{one}}\n"
    "{{/b}}\n"
    "{{one}}\n"
    "{{/a}}\n",
    ?_assertEqual("1\n"
                  "121\n"
                  "12321\n"
                  "1234321\n"
                  "123454321\n"
                  "12345654321\n"
                  "123454321\n"
                  "1234321\n"
                  "12321\n"
                  "121\n"
                  "1\n",
                  render_string(T, #{a => #{one => 1},
                                     b => #{two => 2},
                                     c => #{three => 3,
                                            d => #{four => 4,
                                                   five => 5
                                                  }}})).

list_test_test_() ->
    ?_assertEqual("123",
                  render_string("{{#list}}{{item}}{{/list}}",
                                #{list => [#{item => 1},
                                           #{item => 2},
                                           #{item => 3 }]})).

empty_list_test_() ->
    ?_assertEqual("",
                  render_string("{{#list}}Yay lists!{{/list}}", #{list => []})).

doubled_test_() ->
    ?_assertEqual("* first\n* second\n* third\n",
                  render_string("{{#bool}}\n"
                                "* first\n"
                                "{{/bool}}\n"
                                "* {{two}}\n"
                                "{{#bool}}\n"
                                "* third\n"
                                "{{/bool}}\n",
                                #{bool => true, two => <<"second">>})).

nested_truthy_test_() ->
    ?_assertEqual("| A B C D E |",
                  render_string("| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} "
                                "E |",
                                #{bool => true})).

nested_falsey_test_() ->
    ?_assertEqual("| A  E |",
                  render_string("| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} "
                                "E |",
                                #{bool => false})).

context_misses_test_() ->
    ?_assertEqual("[]",
                  render_string("[{{#missing}}Found key 'missing'!"
                                "{{/missing}}]", [])).

% implicit iterators

implicit_iterator_string_test_() ->
    ?_assertEqual("(a)(b)(c)(d)(e)",
                  render_string("{{#list}}({{.}}){{/list}}",
                                [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>])).

implicit_iterator_integer_test_() ->
    ?_assertEqual("(1)(2)(3)(4)(5)",
                  render_string("{{#list}}({{.}}){{/list}}",
                                [1, 2, 3, 4, 5])).

implicit_iterator_decimal_test_() ->
    ?_assertEqual("(1.1)(2.2)(3.3)(4.4)(5.5)",
                  render_string("{{#list}}({{.}}){{/list}}",
                                [1.10, 2.20, 3.30, 4.40, 5.50])).

implicit_iterator_array_test_() ->
    ?_assertEqual("(123)(abc)",
                  render_string("{{#list}}({{#.}}{{.}}{{/.}}){{/list}}",
                                [[1, 2, 3], [<<"a">>, <<"b">>, <<"c">>]])).

implicit_iterator_html_escaping_test_() ->
    ?_assertEqual("(&amp;)(&quot;)(&lt;)(&gt;)",
                  render_string("{{#list}}({{.}}){{/list}}",
                                [<<"&">>, <<"\"">>, <<"<">>, <<">">>])).

implicit_iterator_triple_mustache_test_() ->
    ?_assertEqual("(&)(\")(<)(>)",
                  render_string("{{#list}}({{{.}}}){{/list}}",
                                [<<"&">>, <<"\"">>, <<"<">>, <<">">>])).

implicit_iterator_ampersand_test_() ->
    ?_assertEqual("(&)(\")(<)(>)",
                  render_string("{{#list}}({{&.}}){{/list}}",
                                [<<"&">>, <<"\"">>, <<"<">>, <<">">>])).

implicit_iterator_root_level_test_() ->
    ?_assertEqual("(a)(b)",
                  render_string("{{#.}}({{value}}){{/.}}",
                                [#{value => <<"a">>}, #{value => <<"b">>}])).

% dotted names

dotted_names_truthy_test_() ->
    ?_assertEqual("\"Here\" == \"Here\"",
                  render_string("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\"",
                                #{a => #{b => #{c => true}}})).

dotted_names_falsey_test_() ->
    ?_assertEqual("\"\" == \"\"",
                  render_string("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"",
                                #{a => #{b => #{c => false}}})).

dotted_names_broken_chains_test_() ->
    ?_assertEqual("\"\" == \"\"",
                  render_string("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"",
                                #{a => #{}})).

% whitespace sensitivity

surrounding_whitespace_test_() ->
    ?_assertEqual(" | \t|\t | \n",
                  render_string(" | {{#boolean}}\t|\t{{/boolean}} | \n",
                                #{boolean => true})).

internal_whitespace_test_() ->
    ?_assertEqual(" |  \n  | \n",
                  render_string(" | {{#boolean}} {{! Important Whitespace }}\n "
                                "{{/boolean}} | \n",
                                #{boolean => true})).

indented_inline_sections_test_() ->
    ?_assertEqual(" YES\n GOOD\n",
                  render_string(" {{#boolean}}YES{{/boolean}}\n"
                                " {{#boolean}}GOOD{{/boolean}}\n",
                                #{ boolean => true })).

standalone_lines_test_() ->
    ?_assertEqual("|\n"
                  "| This Is\n"
                  "|\n"
                  "| A Line",
                  render_string("|\n"
                                "| This Is\n"
                                "{{#boolean}}\n"
                                "|\n"
                                "{{/boolean}}\n"
                                "| A Line",
                                #{boolean => true})).


indented_standalone_lines_test_() ->
    ?_assertEqual("| This Is\n"
                  "|\n"
                  "| A Line",
                  render_string("| This Is\n"
                                "  {{#boolean}}\n"
                                "|\n"
                                "  {{/boolean}}\n"
                                "| A Line",
                                #{boolean => true})).

standalone_line_endings_test_() ->
    ?_assertEqual("|\r\n|",
                  render_string("|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|",
                                #{boolean => true})).

standalone_without_previous_line_test_() ->
    ?_assertEqual("#\n/",
                  render_string("  {{#boolean}}\n#{{/boolean}}\n/",
                                #{boolean => true})).

standalone_without_newline_test_() ->
    ?_assertEqual("#\n/\n",
                  render_string("#{{#boolean}}\n/\n  {{/boolean}}",
                                #{boolean => true})).

% whitespace insensitivity

padding_test_() ->
    ?_assertEqual("|=|", render_string("|{{# boolean }}={{/ boolean }}|",
                                       #{boolean => true})).
