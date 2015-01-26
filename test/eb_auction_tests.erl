-module(eb_auction_tests).

-include_lib("eunit/include/eunit.hrl").

gen_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun list_of_bids_is_empty_at_start/1,
        fun reject_bid_lower_than_starting_price/1,
        fun first_bid_is_accepted/1,
        fun bid_rejected_if_lower_than_previous_bid/1,
        fun bid_accepted_if_higher_than_current_bid/1
    ]}.

setup() ->
    process_flag(trap_exit, true),
    StartingPrice = {0, 99},
    {ok, Pid} = eb_auction:start_link(StartingPrice),
    Pid.

list_of_bids_is_empty_at_start(Pid) ->
    fun() ->
        ?assertEqual([], gen_server:call(Pid, list_bids))
    end.

reject_bid_lower_than_starting_price(Pid) ->
    fun() ->
        ?assertEqual(bid_too_low, gen_server:call(Pid, {bid, 1, {0, 70}}))
    end.

first_bid_is_accepted(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {1, 99}}),
        [Bid] = gen_server:call(Pid, list_bids),
        ?assertMatch({bid, 1, {1, 99}, _Time}, Bid)
    end.

bid_rejected_if_lower_than_previous_bid(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {1, 99}}),
        ?assertEqual(bid_too_low, gen_server:call(Pid, {bid, 2, {0, 99}}))
    end.

bid_accepted_if_higher_than_current_bid(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {1, 99}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {2, 99}}),
        HighBid = hd(gen_server:call(Pid, list_bids)),
        ?assertMatch({bid, 2, {2, 99}, _Time}, HighBid)
    end.

cleanup(Pid) ->
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
