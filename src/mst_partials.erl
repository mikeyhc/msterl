-module(mst_partials).

-export([apply_partials/2]).

apply_partials(Template, Partials) ->
    apply_partials(Template, stringify_keys(Partials), []).

apply_partials([], _Partials, Acc) -> lists:reverse(Acc);
apply_partials([{partial, Partial}|Rest], Partials, Acc) ->
    case maps:find(Partial, Partials) of
        {ok, Body} ->
            Parsed = mst_parser:parse(Body),
            apply_partials(Rest, Partials, lists:reverse(Parsed) ++ Acc);
        error ->
            apply_partials(Rest, Partials, Acc)
    end;
apply_partials([H|T], Partials, Acc) ->
    apply_partials(T, Partials, [H|Acc]).

stringify_keys(Partials) ->
    maps:from_list(lists:map(fun stringify_key/1, maps:to_list(Partials))).

stringify_key({K, V}) when is_atom(K) -> {atom_to_list(K), V};
stringify_key(V) -> V.
