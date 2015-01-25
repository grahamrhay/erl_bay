-module(eb_auction_tests).

-include_lib("eunit/include/eunit.hrl").

gen_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [fun(Pid) -> [
        fun() -> server_is_alive(Pid) end
    ] end]}.

setup() ->
    process_flag(trap_exit, true),
    {ok, Pid} = eb_auction:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

cleanup(Pid) ->
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
