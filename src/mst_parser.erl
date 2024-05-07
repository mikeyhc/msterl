-module(mst_parser).

-export([parse/2]).

-record(parsed_template, {literal_buffer=[] :: string(),
                          standalone=true :: boolean(),
                          last_block=none :: none | comment | block | section,
                          partials :: #{string() => string()},
                          template=[]
                         }).

parse(Template, Partials) ->
    ParsedTemplate0 = parse_(Template, #parsed_template{
                                          partials=stringify_keys(Partials)
                                         }),
    ParsedTemplate1 = finalize(ParsedTemplate0, true),
    ParsedTemplate1#parsed_template.template.

section_recur(Template, ParseState) ->
    NPS = ParseState#parsed_template{literal_buffer=[],
                                     template=[]
                                    },
    parse_(Template, NPS).

parse_([], ParsedTemplate) -> ParsedTemplate;
% comment
parse_([${, ${, $!|Rest0], PT0) ->
    Rest1 = drop_til_close(Rest0),
    {PT1, Rest2} = case is_standalone(PT0) of
                       true -> {drop_buffer(PT0), drop_newline(Rest1)};
                       false -> {PT0, Rest1}
                   end,
    parse_(Rest2, PT1#parsed_template{last_block=comment});
% partial
parse_([${, ${, $>|Rest0],
       ParsedTemplate=#parsed_template{partials=Partials}) ->
    {PartialName, Rest1} = read_til_close(Rest0, false),
    Rest2 = case maps:find(PartialName, Partials) of
                {ok, Partial} -> Partial ++ Rest1;
                error -> Rest1
            end,
    parse_(Rest2, ParsedTemplate);
% section begin
parse_([${, ${, $#|Rest0], ParsedTemplate0) ->
    {SectionName, Rest1} = read_til_close(Rest0, false),
    {Section, Rest2} = section_recur(Rest1, ParsedTemplate0),
    {section, SectionName, SectionTemplate} = Section,
    ParsedTemplate1 = ParsedTemplate0#parsed_template{
                        standalone=SectionTemplate#parsed_template.standalone
                       },
    ParsedTemplate2 = flush_buffer(ParsedTemplate1, Rest1),
    Block = {section, SectionName, SectionTemplate#parsed_template.template},
    parse_(Rest2, push_block(Block, ParsedTemplate2));
% section close
parse_([${, ${, $/|Rest0], ParsedTemplate0) ->
    {SectionName, Rest1} = read_til_close(Rest0, false),
    ParsedTemplate1 = finalize(ParsedTemplate0, length(Rest1) =:= 0),
    {{section, SectionName, ParsedTemplate1}, Rest1};
% triple moustache
parse_([${, ${, ${|Rest0], ParsedTemplate) ->
    {Block, Rest1} = read_til_close(Rest0, true),
    parse_(Rest1, push_block({unsanitized_block, Block}, ParsedTemplate));
% moustache block
parse_([${, ${|Rest0], ParsedTemplate) ->
    {Block, Rest1} = read_til_close(Rest0, false),
    parse_(Rest1, push_block({sanitized_block, Block}, ParsedTemplate));
% string literal
parse_([H|T], ParsedTemplate) ->
    parse_(T, push_literal(H, ParsedTemplate)).


%%====================================================================
%% helper methods
%%====================================================================

is_standalone(#parsed_template{standalone=I, last_block=B}) ->
    I andalso (B =:= comment orelse B =:= section).

drop_buffer(ParsedTemplate) ->
    ParsedTemplate#parsed_template{literal_buffer=[]}.

push_literal($\n, PT=#parsed_template{standalone=true, last_block=LB}) when
      LB =:= comment orelse LB =:= section orelse LB =:= none ->
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
    PT#parsed_template{template=[Block|T], last_block=block_type(Block)};
push_block(Block, PT=#parsed_template{literal_buffer=LB, template=T}) ->
    PT#parsed_template{literal_buffer=[],
                       last_block=block_type(Block),
                       template=[Block, {literal, lists:reverse(LB)}|T]}.

finalize(PT=#parsed_template{literal_buffer=[]}, _End) -> PT;
finalize(PT=#parsed_template{standalone=true}, true) -> PT;
finalize(PT=#parsed_template{literal_buffer=LB, template=T}, _End) ->
    PT#parsed_template{literal_buffer=[],
                       template=[{literal, lists:reverse(LB)}|T]}.

drop_til_close([$}, $}|T]) -> T;
drop_til_close([_|T]) ->
    drop_til_close(T).

read_til_close(Input, Triple) -> read_til_close(Input, Triple, []).

read_til_close([$}, $}|T], false, Acc) ->
    {string:trim(lists:reverse(Acc), both), T};
read_til_close([$}, $}, $}|T], true, Acc) ->
    {string:trim(lists:reverse(Acc), both), T};
read_til_close([H|T], Triple, Acc) -> read_til_close(T, Triple, [H|Acc]).

drop_newline([$\n|T]) -> T;
drop_newline([$\r, $\n|T]) -> T;
drop_newline(L) -> L.

block_type({section, _, _}) -> section;
block_type({unsanitized_block, _}) -> block;
block_type({sanitized_block, _}) -> block.

is_whitespace($ ) -> true;
is_whitespace($\t) -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace(_) -> false.

flush_buffer(PT=#parsed_template{literal_buffer=[]}, _Rest) -> PT;
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

should_flush([]) -> false;
should_flush([$\n|_]) -> true;
should_flush([H|T]) ->
    case is_whitespace(H) of
        true -> should_flush(T);
        false -> false
    end.

stringify_keys(Partials) ->
    maps:from_list(lists:map(fun stringify_key/1, maps:to_list(Partials))).

stringify_key({K, V}) when is_atom(K) -> {atom_to_list(K), V};
stringify_key(V) -> V.
