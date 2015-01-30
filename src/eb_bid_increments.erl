-module(eb_bid_increments).

-export([get/1]).

-spec get(eb_money:money()) -> eb_money:money().
get(Amount) when Amount < {1, 00} ->
    {0, 05};

get(Amount) when Amount < {5, 00} ->
    {0, 20};

get(Amount) when Amount < {15, 00} ->
    {0, 50};

get(Amount) when Amount < {60, 00} ->
    {1, 00};

get(Amount) when Amount < {150, 00} ->
    {2, 00};

get(Amount) when Amount < {300, 00} ->
    {5, 00};

get(Amount) when Amount < {600, 00} ->
    {10, 00};

get(Amount) when Amount < {1500, 00} ->
    {20, 00};

get(Amount) when Amount < {3000, 00} ->
    {50, 00};

get(_Amount) ->
    {100, 0}.
