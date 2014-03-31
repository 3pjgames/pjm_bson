-module(pjm_bson).

-export([from_bson/2, to_bson/1, coerce/2]).
-export([bson_to_term/1, term_to_bson/1]).

-spec from_bson(bson:document(), module() | pjm:model()) -> pjm:model().
from_bson(Bson, Module) when is_atom(Module) ->
    from_bson(Bson, Module:new());
from_bson(Bson, Model) ->
    {Attrs} = bson_to_term(Bson),
    pjm:set(Attrs, Model).

-spec bson_to_term(bson:document()) -> term().
bson_to_term(Document) when is_tuple(Document) andalso size(Document) rem 2 =:= 0 ->
    {lists:map(fun({K, V}) -> {K, bson_to_term(V)} end, bson:fields(Document))};
bson_to_term(Tuple) when is_tuple(Tuple)->
    Tuple;
bson_to_term(List) when is_list(List) ->
    lists:map(fun bson_to_term/1, List);
bson_to_term(Term) -> Term.

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

-spec term_to_bson(term()) -> bson:document().
term_to_bson({pjm, _, _} = Model) ->
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
