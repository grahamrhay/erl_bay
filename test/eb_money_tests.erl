-module(eb_money_tests).

-include_lib("eunit/include/eunit.hrl").

simple_add_test() ->
    Money1 = {0, 50},
    Money2 = {1, 00},
    ?assertEqual({1, 50}, eb_money:add(Money1, Money2)).

add_overflow_test() ->
    Money1 = {0, 50},
    Money2 = {1, 50},
    ?assertEqual({2, 00}, eb_money:add(Money1, Money2)).
