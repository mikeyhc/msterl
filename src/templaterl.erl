-module(templaterl).

%% API
-export([render_file/1, render_file/2, render_file/3,
         render_string/1, render_string/2, render_string/3]).

%%====================================================================
%% API
%%====================================================================

render_file(TemplatePath) ->
    render_file(TemplatePath, #{}, #{}).

render_file(TemplatePath, Data) ->
    render_file(TemplatePath, Data, #{}).

render_file(TemplatePath, Data, Partials) ->
    {ok, Template} = file:read_file(TemplatePath),
    render_string(Template, Data, Partials).

render_string(Template) ->
    render_string(Template, #{}, #{}).

render_string(Template, Data) ->
    render_string(Template, Data, #{}).

render_string(Template, Data, Partials) ->
    ParsedTemplate = mst_parser:parse(Template, Partials),
    render(ParsedTemplate, mst_context:new(Data), []).

%%====================================================================
%% helper functions
%%====================================================================

render([], _Context, Acc) -> lists:flatten(Acc);
render([{literal, L}|T], Context, Acc) -> render(T, Context, [L|Acc]);
render([{sanitized_block, B}|T], Context, Acc) ->
    try
        Value = case B of
                    [$&|Key] -> coerce(mst_context:get_value(Key, Context),
                                       false);
                    Key -> coerce(mst_context:get_value(Key, Context), true)
                end,
        render(T, Context, [Value|Acc])
    catch
        E={invalid_dot_context, _, _} -> throw({E, {block, B}, {template, T},
                                                {rendered, Acc}})
    end;
render([{unsanitized_block, B}|T], Context, Acc) ->
    render(T, Context, [coerce(mst_context:get_value(B, Context), false)|Acc]);
render([{section, SectionName, Section}|T], Context, Acc) ->
    NewContext = mst_context:get_value(SectionName, Context),
    Render = case is_falsy(NewContext) of
                 true -> [];
                 false ->
                     eval_section(Section,
                                  mst_context:push_context(NewContext, Context))
             end,
    render(T, Context, [Render|Acc]).

eval_section(Section, Context) ->
    eval_section(Section, Context, []).

eval_section(Section, Context, Acc) ->
    Render = render(Section, Context, []),
    case mst_context:next_context(Context) of
        {true, NextContext} ->
            eval_section(Section, NextContext, [Render|Acc]);
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
