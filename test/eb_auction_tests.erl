-module(eb_auction_tests).

-include_lib("eunit/include/eunit.hrl").

gen_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun list_of_bids_is_empty_at_start/1,
        fun reject_bid_lower_than_starting_price/1,
        fun first_bid_is_at_starting_price/1,
        fun raising_max_bid_does_not_create_bid/1,
        fun second_bidder_bids_less_than_first_max/1,
        fun second_bidder_bids_less_than_first_max_by_more_than_increment/1,
        fun second_bidder_bids_same_as_first_max/1,
        fun second_bidder_bids_more_than_first_max/1,
        fun second_bidder_bids_more_than_first_max_plus_increment/1
    ]}.

setup() ->
    process_flag(trap_exit, true),
    StartingPrice = {0, 50},
    {ok, Pid} = eb_auction:start_link(StartingPrice),
    Pid.

list_of_bids_is_empty_at_start(Pid) ->
    fun() ->
        ?assertEqual([], gen_server:call(Pid, list_bids))
    end.

reject_bid_lower_than_starting_price(Pid) ->
    fun() ->
        ?assertEqual(bid_too_low, gen_server:call(Pid, {bid, 1, {0, 40}}))
    end.

first_bid_is_at_starting_price(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 01}}),
        [Bid] = gen_server:call(Pid, list_bids),
        ?assertMatch({bid, 1, {0, 50}, _Time, false}, Bid)
    end.

raising_max_bid_does_not_create_bid(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 50}}),
        ?assertMatch(1, length(gen_server:call(Pid, list_bids)))
    end.

second_bidder_bids_less_than_first_max_by_more_than_increment(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {1, 60}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {1, 50}}),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(3, length(Bids)),
        ?assertMatch({bid, 2, {1, 50}, _Time, false}, lists:nth(2, Bids)),
        ?assertMatch({bid, 1, {1, 60}, _Time, false}, lists:nth(3, Bids))
    end.

second_bidder_bids_less_than_first_max(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {5, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {1, 50}}),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(3, length(Bids)),
        ?assertMatch({bid, 2, {1, 50}, _Time, false}, lists:nth(2, Bids)),
        ?assertMatch({bid, 1, {1, 70}, _Time, true}, lists:nth(3, Bids))
    end.

second_bidder_bids_same_as_first_max(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {2, 00}}),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(3, length(Bids)),
        ?assertMatch({bid, 2, {2, 00}, _Time, false}, lists:nth(2, Bids)),
        ?assertMatch({bid, 1, {2, 00}, _Time, false}, lists:nth(3, Bids))
    end.

second_bidder_bids_more_than_first_max(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {2, 10}}),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(3, length(Bids)),
        ?assertMatch({bid, 1, {2, 00}, _Time, false}, lists:nth(2, Bids)),
        ?assertMatch({bid, 2, {2, 10}, _Time, false}, lists:nth(3, Bids))
    end.

second_bidder_bids_more_than_first_max_plus_increment(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {3, 50}}),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(3, length(Bids)),
        ?assertMatch({bid, 1, {2, 00}, _Time, false}, lists:nth(2, Bids)),
        ?assertMatch({bid, 2, {2, 20}, _Time, true}, lists:nth(3, Bids))
    end.

cleanup(Pid) ->
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
