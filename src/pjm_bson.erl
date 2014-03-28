-module(pjm_bson).

-export([from_bson/2, to_bson/1, coerce/2]).

-spec from_bson(bson:document(), module() | pjm:model()) -> pjm:model().
from_bson({Object}, Module) when is_atom(Module) ->
    from_bson({Object}, Module:new());
from_bson({Object}, Model) ->
    pjm:set(Object, Model).

-spec to_bson(pjm:model()) -> bson:document().
to_bson(Model) ->
    bson:document(
      pjm:fold(
        fun to_bson_acc/3,
        [],
        Model
       )
     ).

to_bson_acc(_K, undefined, List) ->
    List;
to_bson_acc(K, Value, List) ->
    [{K, term_to_bson(Value)}|List].

term_to_bson({pjm, _, _, _} = Model) ->
    to_bson(Model);
term_to_bson([]) -> [];
term_to_bson({}) -> {};
term_to_bson([{Key, _Value}|_Rest] = List) when is_atom(Key) orelse is_binary(Key) ->
    bson:document(lists:map(fun({K, V}) -> {K, term_to_bson(V)} end, List));
term_to_bson(List) when is_list(List) ->
    lists:map(fun term_to_bson/1, List);
term_to_bson({List}) when is_list(List) ->
    term_to_bson(List);
term_to_bson(Term) -> Term.

%% use {pjm_bson, objectid} as pjm field type.
coerce(objectid, {<<_:96>>} = Id) -> Id;
coerce(objectid, Id) when is_binary(Id) ->
    % From Hex String to Binary
    << << (binary_to_integer(Bits, 16)):4 >> || << Bits:1/binary >> <= Id >>.
