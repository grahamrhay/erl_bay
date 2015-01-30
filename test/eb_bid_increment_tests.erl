-module(eb_bid_increment_tests).

-include_lib("eunit/include/eunit.hrl").

less_than_one_pound_test() ->
    ?assertEqual({0, 05}, eb_bid_increments:get({0, 99})).

less_than_five_pounds_test() ->
    ?assertEqual({0, 20}, eb_bid_increments:get({4, 99})).

less_than_fifteen_pounds_test() ->
    ?assertEqual({0, 50}, eb_bid_increments:get({14, 99})).

less_than_sixty_pounds_test() ->
    ?assertEqual({1, 00}, eb_bid_increments:get({59, 99})).

less_than_one_hundred_fifty_pounds_test() ->
    ?assertEqual({2, 00}, eb_bid_increments:get({149, 99})).

less_than_three_hundred_test() ->
    ?assertEqual({5, 00}, eb_bid_increments:get({299, 99})).

less_than_six_hundred_test() ->
    ?assertEqual({10, 00}, eb_bid_increments:get({599, 99})).

less_than_fifteen_hundred_test() ->
    ?assertEqual({20, 00}, eb_bid_increments:get({1499, 99})).

less_than_three_thousand_test() ->
    ?assertEqual({50, 00}, eb_bid_increments:get({2999, 99})).

more_than_three_thousand_test() ->
    ?assertEqual({100, 00}, eb_bid_increments:get({3000, 0})).
