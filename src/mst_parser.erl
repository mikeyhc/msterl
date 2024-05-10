-module(mst_parser).

-export([parse/1]).

-record(parsed_template, {literal_buffer=[] :: string(),
                          standalone=true :: boolean(),
                          last_block=none :: none | comment | block | section |
                                             partial | delimiter,
                          template=[],
                          start_delimiter="{{",
                          end_delimiter="}}"
                         }).

parse(Template) ->
    {[], ParsedTemplate0} = parse(Template, #parsed_template{}),
    ParsedTemplate1 = finalize(ParsedTemplate0, true),
    lists:reverse(ParsedTemplate1#parsed_template.template).

section_recur(Template, ParseState) ->
    NPS = ParseState#parsed_template{last_block=section,
                                     literal_buffer=[],
                                     template=[]
                                    },
    parse(Template, NPS).

parse(Template, ParsedTemplate=#parsed_template{start_delimiter=Start}) ->
    MatchFn = fun([$!|Rest], State) -> parse_comment(Rest, State);
                 ([$>|Rest], State) -> parse_partial(Rest, State);
                 ([$#|Rest], State) -> parse_section(Rest, State, false);
                 ([$^|Rest], State) -> parse_section(Rest, State, true);
                 ([$/|Rest], State) -> parse_section_close(Rest, State);
                 ([${|Rest], State) -> parse_triple(Rest, State);
                 ([$&|Rest], State) -> parse_ampersand(Rest, State);
                 ([$=|Rest], State) -> parse_delimiter(Rest, State);
                 (Rest, State) -> parse_basic(Rest, State)
              end,
    NoMatchFn = fun(C, State) -> push_literal(C, State) end,
    read_loop(Template, Start, MatchFn, NoMatchFn, ParsedTemplate).

parse_comment(Rest0, PT0) ->
    Rest1 = drop_til_close(Rest0, PT0),
    {PT1, Rest2} = case is_standalone(PT0) of
                       true -> {drop_buffer(PT0), drop_newline(Rest1)};
                       false -> {PT0, Rest1}
                   end,
    {Rest2, PT1#parsed_template{last_block=comment}}.

parse_partial(Rest0, ParsedTemplate0) ->
    {PartialName, Rest1} = read_til_close(Rest0, ParsedTemplate0, false),
    Indent = get_indent(ParsedTemplate0),
    ParsedTemplate1 = flush_buffer(ParsedTemplate0, Rest1),
    {Rest1, push_block({partial, PartialName, Indent}, ParsedTemplate1)}.

parse_section_close(Rest0, ParsedTemplate0) ->
    {SectionName, Rest1} = read_til_close(Rest0, ParsedTemplate0, false),
    ParsedTemplate1 = flush_buffer(ParsedTemplate0, Rest1),
    {break, {section, SectionName, ParsedTemplate1}, Rest1}.

parse_triple(Rest0, ParsedTemplate) ->
    {Block, Rest1} = read_til_close(Rest0, ParsedTemplate, $}),
    {Rest1, push_block({unsanitized_block, Block}, ParsedTemplate)}.

parse_basic(Rest0, ParsedTemplate) ->
    {Block, Rest1} = read_til_close(Rest0, ParsedTemplate, false),
    {Rest1, push_block({sanitized_block, Block}, ParsedTemplate)}.

parse_ampersand(Rest0, ParsedTemplate) ->
    {Block, Rest1} = read_til_close(Rest0, ParsedTemplate, false),
    {Rest1, push_block({unsanitized_block, Block}, ParsedTemplate)}.

parse_section(Rest0, ParsedTemplate0, Invert) ->
    {SectionName, Rest1} = read_til_close(Rest0, ParsedTemplate0, false),
    ParsedTemplate1 = flush_buffer(ParsedTemplate0, Rest1),
    {Section, Rest2} = section_recur(Rest1, ParsedTemplate1),
    {section, SectionName, SectionTemplate} = Section,
    ParsedTemplate2 = ParsedTemplate1#parsed_template{
                        standalone=SectionTemplate#parsed_template.standalone
                       },
    Body = lists:reverse(SectionTemplate#parsed_template.template),
    Block = if Invert -> {inverted_section, SectionName, Body};
               true -> {section, SectionName, Body}
            end,
    {Rest2, push_block(Block, ParsedTemplate2)}.

parse_delimiter(Rest0, ParsedTemplate0) ->
    {BothDelimiters, Rest1} = read_til_close(Rest0, ParsedTemplate0, $=),
    [Start, End] = lists:filter(fun([]) -> false; (_) -> true end,
                                string:split(BothDelimiters, " ", all)),
    {change, Start, Rest1,
     ParsedTemplate0#parsed_template{last_block=delimiter,
                                     start_delimiter=Start,
                                     end_delimiter=End}}.

%%====================================================================
%% helper methods
%%====================================================================

is_standalone(#parsed_template{standalone=I, last_block=B}) ->
    I andalso (B =:= comment orelse B =:= section orelse B =:= partial orelse
               B =:= delimiter).

drop_buffer(ParsedTemplate) ->
    ParsedTemplate#parsed_template{literal_buffer=[]}.

push_literal($\n, PT=#parsed_template{last_block=none,
                                      standalone=true,
                                      literal_buffer=LB,
                                      template=T}) ->
    PT#parsed_template{literal_buffer=[],
                       standalone=true,
                       template=[{literal, lists:reverse([$\n|LB])}|T]};
push_literal($\n, PT=#parsed_template{standalone=true, last_block=LB}) when
      LB =:= comment orelse LB =:= section orelse LB =:= partial orelse
      LB =:= delimiter orelse LB =:= none ->
    PT#parsed_template{literal_buffer=[], last_block=none};
push_literal($\n, PT=#parsed_template{literal_buffer=LB, template=T}) ->
    PT#parsed_template{literal_buffer=[],
                       standalone=true,
                       last_block=none,
                       template=[{literal, lists:reverse([$\n|LB])}|T]};
push_literal(C, PT=#parsed_template{literal_buffer=B}) ->
    Standalone = case is_whitespace(C) of
                     true -> PT#parsed_template.standalone;
                     false -> false
                 end,
    PT#parsed_template{literal_buffer=[C|B], standalone=Standalone}.

push_block(Block, PT=#parsed_template{literal_buffer=[], template=T}) ->
    BlockType = block_type(Block),
    Standalone = if BlockType =:= block -> false;
                    true -> PT#parsed_template.standalone
                 end,
    PT#parsed_template{template=[Block|T],
                       standalone=Standalone,
                       last_block=BlockType};
push_block(Block, PT=#parsed_template{literal_buffer=LB, template=T}) ->
    BlockType = block_type(Block),
    Standalone = if BlockType =:= block -> false;
                    true -> PT#parsed_template.standalone
                 end,
    PT#parsed_template{literal_buffer=[],
                       standalone=Standalone,
                       last_block=BlockType,
                       template=[Block, {literal, lists:reverse(LB)}|T]}.

finalize(PT=#parsed_template{literal_buffer=[]}, _End) -> PT;
finalize(PT=#parsed_template{standalone=true}, true) -> PT;
finalize(PT=#parsed_template{literal_buffer=LB, template=T}, _End) ->
    PT#parsed_template{literal_buffer=[],
                       template=[{literal, lists:reverse(LB)}|T]}.

drop_til_close(Template, #parsed_template{end_delimiter=End}) ->
    {Rest, _} = read_loop(Template, End,
                          fun(List, State) -> {break, List, State} end,
                          fun(_C, State) -> State end,
                          undefined),
    Rest.

read_til_close(Template, #parsed_template{end_delimiter=End0}, Extra) ->
    End1 = case Extra of
               false -> End0;
               Extra -> [Extra|End0]
           end,
    {Rest, Name} = read_loop(Template, End1,
                             fun(List, State) -> {break, List, State} end,
                             fun(C, State) -> [C|State] end,
                             []),
    {string:trim(lists:reverse(Name), both), Rest}.

drop_newline([$\n|T]) -> T;
drop_newline([$\r, $\n|T]) -> T;
drop_newline(L) -> L.

block_type({section, _, _}) -> section;
block_type({inverted_section, _, _}) -> section;
block_type({unsanitized_block, _}) -> block;
block_type({sanitized_block, _}) -> block;
block_type({partial, _, _}) -> partial.

is_whitespace($ ) -> true;
is_whitespace($\t) -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace(_) -> false.

flush_buffer(PT=#parsed_template{literal_buffer=[]}, _Rest) -> PT;
flush_buffer(PT=#parsed_template{standalone=false}, Rest) ->
    finalize(PT, length(Rest) =:= 0);
flush_buffer(PT=#parsed_template{literal_buffer=Buffer}, Rest) ->
    End = length(Rest) =:= 0,
    case lists:all(fun is_whitespace/1, Buffer) of
        false -> finalize(PT, End);
        true ->
            case should_flush(Rest) of
                true -> PT#parsed_template{literal_buffer=[]};
                false -> finalize(PT, End)
            end;
        _ -> finalize(PT, End)
    end.

should_flush([]) -> true;
should_flush([$\n|_]) -> true;
should_flush([H|T]) ->
    case is_whitespace(H) of
        true -> should_flush(T);
        false -> false
    end.

get_indent(#parsed_template{standalone=false}) -> "";
get_indent(#parsed_template{literal_buffer=Buffer}) ->
    case lists:all(fun is_whitespace/1, Buffer) of
        true -> Buffer;
        false -> ""
    end.

read_loop([], _Match, _MatchFn, _NoMatchFn, State) -> {[], State};
read_loop(List0=[H|T], Match0, MatchFn, NoMatchFn, State0) ->
    case string:prefix(List0, Match0) of
        nomatch ->
            read_loop(T, Match0, MatchFn, NoMatchFn, NoMatchFn(H, State0));
        List1 ->
            case MatchFn(List1, State0) of
                {change, Match1, List2, State1} ->
                    read_loop(List2, Match1, MatchFn, NoMatchFn, State1);
                {break, List2, State1} -> {List2, State1};
                {List2, State1} ->
                    read_loop(List2, Match0, MatchFn, NoMatchFn, State1)
            end
    end.
