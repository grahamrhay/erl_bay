-module(eb_money).

-export([add/2]).

-type money() :: {pos_integer(), pos_integer()}.
-export_type([money/0]).

-spec add(money(), money()) -> money().
add({Pounds1, Pennies1}, {Pounds2, Pennies2}) when (Pennies1 + Pennies2) < 100 ->
    {Pounds1 + Pounds2, Pennies1 + Pennies2};

add({Pounds1, Pennies1}, {Pounds2, Pennies2}) ->
    {Pounds1 + Pounds2 + 1, (Pennies1 + Pennies2) - 100}.
