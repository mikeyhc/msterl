-module(mst_render).

-export([render/3]).

-record(render_opts, {ctx, partials, indent="", last=none}).

%%====================================================================
%% API
%%====================================================================

render(Template, Data, Partials) ->
    ParsedTemplate = mst_parser:parse(Template),
    ParsedPartials = mst_partials:parse(Partials),
    render_(ParsedTemplate,
            #render_opts{ctx=mst_context:new(Data), partials=ParsedPartials},
            []).

%%====================================================================
%% helper functions
%%====================================================================

render_([], _Opts, Acc) -> lists:flatten(lists:reverse(Acc));
render_([{literal, L}|T], Opts, Acc) ->
    render_(T, set_last(literal, Opts), [apply_indent(L, Opts)|Acc]);
render_([{sanitized_block, Key}|T], Opts=#render_opts{ctx=Context}, Acc) ->
    try
        Value = coerce(mst_context:get_value(Key, Context), true),
        render_(T, set_last(block, Opts), [apply_indent(Value, Opts)|Acc])
    catch
        E={invalid_dot_context, _, _} -> throw({E, {block, Key}, {template, T},
                                                {rendered, Acc}})
    end;
render_([{unsanitized_block, B}|T], Opts=#render_opts{ctx=Context}, Acc) ->
    Output = apply_indent(coerce(mst_context:get_value(B, Context), false),
                          Opts),
    render_(T, set_last(block, Opts), [Output|Acc]);
render_([{section, SectionName, Section}|T], Opts=#render_opts{ctx=Context},
       Acc) ->
    NewContext = mst_context:get_value(SectionName, Context),
    Render = case is_falsy(NewContext) of
                 true -> [];
                 false ->
                     Ctx = mst_context:push_context(NewContext, Context),
                     eval_section(Section, Opts#render_opts{ctx=Ctx})
             end,
    render_(T, set_last(block, Opts), [Render|Acc]);
render_([{inverted_section, SectionName, Section}|T],
       Opts=#render_opts{ctx=Context},
       Acc) ->
    NewContext = mst_context:get_value(SectionName, Context),
    Render = case not is_falsy(NewContext) of
                 true -> [];
                 false ->
                     Ctx = mst_context:push_context(NewContext, Context),
                     eval_section(Section, Opts#render_opts{ctx=Ctx})
             end,
    render_(T, set_last(block, Opts), [Render|Acc]);
render_([{partial, PartialName, Indent}|T],
       Opts=#render_opts{partials=Partials},
       Acc) ->
    Render = case maps:find(PartialName, Partials) of
                 {ok, Body} ->
                     render_(Body, Opts#render_opts{indent=Indent}, []);
                 error -> ""
             end,
    render_(T, set_last(block, Opts), [Render|Acc]).

eval_section(Section, Opts) -> eval_section(Section, Opts, []).

eval_section(Section, Opts=#render_opts{ctx=Context}, Acc) ->
    Render = render_(Section, Opts, []),
    case mst_context:next_context(Context) of
        {true, NextContext} ->
            eval_section(Section,
                         Opts#render_opts{ctx=NextContext},
                         [Render|Acc]);
        false -> lists:flatten(lists:reverse([Render|Acc]))
    end.

is_falsy(false) -> true;
is_falsy(null) -> true;
is_falsy(undefined) -> true;
is_falsy([]) -> true;
is_falsy(_) -> false.

sanitize_char($&) -> "&amp;";
sanitize_char($") -> "&quot;";
sanitize_char($<) -> "&lt;";
sanitize_char($>) -> "&gt;";
sanitize_char(C) -> [C].

sanitize(Input) ->
    lists:flatten(lists:map(fun sanitize_char/1, Input)).

coerce(null, _Sanitize) -> "";
coerce(undefined, _Sanitize) -> "";
coerce(Value, _Sanitize) when is_integer(Value) -> integer_to_list(Value);
coerce(Value, _Sanitize) when is_float(Value) ->
    % float_to_list has rounding errors
    io_lib:format("~p", [Value]);
coerce(Value, false) when is_binary(Value) -> binary_to_list(Value);
coerce(Value, true) when is_binary(Value) -> sanitize(binary_to_list(Value)).

set_last(Type, Opts) -> Opts#render_opts{last=Type}.

apply_indent(Literal, #render_opts{last=Last, indent=Indent})
  when Last =:= none orelse Last =:= literal ->
    Indent ++ Literal;
apply_indent(Literal, _Opts) -> Literal.
