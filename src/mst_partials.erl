-module(mst_partials).

-export([parse/1]).

parse(Partials) ->
    maps:from_list(lists:map(fun process_entry/1, maps:to_list(Partials))).

process_entry({K, {compiled, V}}) ->
    {atom_to_list(K), V};
process_entry({K, V}) ->
    {atom_to_list(K), mst_parser:parse(V)}.
