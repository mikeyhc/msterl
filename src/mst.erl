-module(mst).

%% API
-export([render/1, render/2, render/3,
         render_file/1, render_file/2, render_file/3,
         render_string/1, render_string/2, render_string/3,

         compile/1, compile_file/1, compile_string/1]).

%%====================================================================
%% API
%%====================================================================

render(Id) -> render(Id, #{}, #{}).

render(Id, Data) -> render(Id, Data, #{}).

render({compiled, Template}, Data, Partials) ->
    mst_render:render(Template, Data, Partials);
render(Id, Data, Partials) ->
    FileName = filename:join("templates/", atom_to_list(Id) ++ ".mustache"),
    render_file(FileName, Data, Partials).

render_file(TemplatePath) ->
    render_file(TemplatePath, #{}, #{}).

render_file(TemplatePath, Data) ->
    render_file(TemplatePath, Data, #{}).

render_file(TemplatePath, Data, Partials) ->
    {ok, Template} = file:read_file(TemplatePath),
    render_string(binary_to_list(Template), Data, Partials).

render_string(Template) ->
    render_string(Template, #{}, #{}).

render_string(Template, Data) ->
    render_string(Template, Data, #{}).

render_string(Template, Data, Partials) ->
    ParsedTemplate = mst_parser:parse(Template),
    mst_render:render(ParsedTemplate, Data, Partials).

compile(Id) ->
    FileName = filename:join("templates/", atom_to_list(Id) ++ ".mustache"),
    compile_file(FileName).

compile_file(TemplatePath) ->
    {ok, Template} = file:read_file(TemplatePath),
    compile_string(binary_to_list(Template)).

compile_string(Template) ->
    {compiled, mst_parser:parse(Template)}.
