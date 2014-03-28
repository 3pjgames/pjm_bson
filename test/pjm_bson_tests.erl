-module(pjm_bson_tests).
-compile([{parse_transform, pjm_parse_trans}]).

-include("./test_helpers.hrl").

-pjm_stores_in(users).
-pjm_fields([
             {login, binary, <<"test">>},
             {embed, pjm_bson_tests}
            ]).

from_bson_test() ->
    M = pjm_bson:from_bson(
          {login, <<"ian">>, age, 30},
          ?MODULE
         ),
    ?assertEqual([<<"ian">>, 30], get([login, age], M)).

from_bson_nest_test() ->
    M = pjm_bson:from_bson(
          {login, <<"ian">>, age, 30, embed, {login, "yincan"}},
          ?MODULE
         ),
    ?assertEqual(<<"yincan">>, get(login, get(embed, M))).

to_bson_nest_test() ->
    M = pjm_bson:from_bson(
          {login, <<"ian">>, age, 30, embed, {login, "yincan"}},
          ?MODULE
         ),
    ?assertEqual({login, <<"ian">>, age, 30, embed, {login, "yincan"}}, pjm_bson:to_bson(M)).
