-module(mst_context).

-export([new/1, push_context/2, pop_context/1, next_context/1]).
-export([get_value/2]).

-record(context, {context}).

new(Map) -> #context{context=[stringify_keys(Map)]}.

push_context(New, #context{context=C}) ->
    #context{context=[stringify_keys(New)|C]}.

pop_context(#context{context=[_|C]}) ->
    #context{context=C}.

next_context(#context{context=[Context|Rest]}) ->
    case Context of
        [_] -> false;
        [_|T] -> {true, #context{context=[T|Rest]}};
        _ -> false
    end.

% handle implicit list iterator
get_value("list", #context{context=[C=[_|_]]}) -> C;
% handle root . implicit iterator
get_value(".", #context{context=[C=[_|_]]}) -> C;
get_value(".", #context{context=[[V|_]|_]}) -> V;
get_value(".", #context{context=[Context|Stack]}) ->
    case is_scalar(Context) of
        true -> Context;
        false ->throw({invalid_dot_context, Context, Stack})
    end;
get_value(Word, #context{context=Context}) ->
    Parts = string:split(Word, ".", all),
    walk_key(Parts, Context).


walk_key(_Keys, []) -> undefined;
walk_key(Keys, [Context|Contexts]) ->
    case walk_key_context(Keys, Context) of
        {ok, Value} -> Value;
        error -> walk_key(Keys, Contexts)
    end.

walk_key_context([], Context) -> {ok, Context};
walk_key_context([Key|Keys], Context) when is_map(Context) ->
    case maps:find(Key, Context) of
        {ok, Value} -> walk_key_context(Keys, Value);
        error -> error
    end;
walk_key_context(Keys, [Context|Contexts]) ->
    case walk_key_context(Keys, Context) of
        {ok, Value} -> {ok, Value};
        error -> walk_key_context(Keys, Contexts)
    end;
walk_key_context(_Key, _Context) -> error.

is_scalar(V) ->
    is_binary(V) orelse
    is_integer(V) orelse
    is_float(V).

stringify_keys(M) when is_map(M) ->
    maps:from_list(lists:map(fun stringify_kv/1, maps:to_list(M)));
stringify_keys(L=[H|_]) when is_map(H) ->
    lists:map(fun stringify_keys/1, L);
stringify_keys(V) -> V.

stringify_kv({K, V}) when is_atom(K) -> {atom_to_list(K), stringify_keys(V)};
stringify_kv({K, V}) -> {K, stringify_keys(V)}.
