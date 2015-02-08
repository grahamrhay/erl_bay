-module(eb_auction_tests).

-include_lib("eunit/include/eunit.hrl").

no_reserve_test_() ->
    {foreach, fun setup_no_reserve/0, fun cleanup/1, [
        fun list_of_bids_is_empty_at_start/1,
        fun reject_bid_lower_than_starting_price/1,
        fun first_bid_is_at_starting_price/1,
        fun raising_max_bid_does_not_create_bid/1,
        fun cannot_lower_bid_once_made/1,
        fun bid_lower_than_current_is_rejected/1,
        fun second_bidder_bids_less_than_first_max/1,
        fun second_bidder_bids_less_than_first_max_by_more_than_increment/1,
        fun second_bidder_bids_same_as_first_max/1,
        fun second_bidder_bids_more_than_first_max/1,
        fun second_bidder_bids_more_than_first_max_plus_increment/1
    ]}.

reserve_price_test_() ->
    {foreach, fun setup_with_reserve/0, fun cleanup/1, [
        fun reserve_price_not_met_at_start/1,
        fun first_bid_is_less_than_reserve/1,
        fun first_bid_is_more_than_reserve/1,
        fun second_bid_is_still_less_than_reserve/1,
        fun raise_max_bid_over_reserve/1,
        fun second_bid_is_more_than_reserve/1
    ]}.

setup_no_reserve() ->
    process_flag(trap_exit, true),
    StartingPrice = {0, 50},
    EndTime = now_plus_7_days(),
    {ok, Pid} = eb_auction:start_link(StartingPrice, EndTime, undefined),
    Pid.

setup_with_reserve() ->
    process_flag(trap_exit, true),
    StartingPrice = {0, 50},
    EndTime = now_plus_7_days(),
    ReservePrice = {100, 0},
    {ok, Pid} = eb_auction:start_link(StartingPrice, EndTime, ReservePrice),
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

cannot_lower_bid_once_made(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {2, 00}}),
        ?assertMatch(bid_too_low, gen_server:call(Pid, {bid, 1, {1, 50}}))
    end.

bid_lower_than_current_is_rejected(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {1, 00}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {2, 00}}),
        ?assertMatch(bid_too_low, gen_server:call(Pid, {bid, 3, {1, 00}}))
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

now_plus_7_days() ->
    Now = calendar:universal_time(),
    NowInSeconds = calendar:datetime_to_gregorian_seconds(Now),
    calendar:gregorian_seconds_to_datetime(NowInSeconds + (60 * 60 * 24 * 7)).

reserve_price_not_met_at_start(Pid) ->
    fun() ->
        ?assertEqual(false, gen_server:call(Pid, reserve_met))
    end.

first_bid_is_less_than_reserve(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {50, 0}}),
        ?assertEqual(false, gen_server:call(Pid, reserve_met)),
        [Bid] = gen_server:call(Pid, list_bids),
        ?assertMatch({bid, 1, {50, 0}, _Time, false}, Bid)
    end.

first_bid_is_more_than_reserve(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {101, 0}}),
        ?assertEqual(true, gen_server:call(Pid, reserve_met)),
        [Bid] = gen_server:call(Pid, list_bids),
        ?assertMatch({bid, 1, {100, 0}, _Time, false}, Bid)
    end.

second_bid_is_still_less_than_reserve(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {50, 0}}),
        bid_accepted = gen_server:call(Pid, {bid, 1, {75, 0}}),
        ?assertEqual(false, gen_server:call(Pid, reserve_met)),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(2, length(Bids)),
        ?assertMatch({bid, 1, {75, 0}, _Time, false}, lists:nth(2, Bids))
    end.

raise_max_bid_over_reserve(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {50, 0}}),
        bid_accepted = gen_server:call(Pid, {bid, 1, {101, 0}}),
        ?assertEqual(true, gen_server:call(Pid, reserve_met)),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(2, length(Bids)),
        ?assertMatch({bid, 1, {100, 0}, _Time, false}, lists:nth(2, Bids))
    end.

second_bid_is_more_than_reserve(Pid) ->
    fun() ->
        bid_accepted = gen_server:call(Pid, {bid, 1, {50, 0}}),
        bid_accepted = gen_server:call(Pid, {bid, 2, {101, 0}}),
        ?assertEqual(true, gen_server:call(Pid, reserve_met)),
        Bids = gen_server:call(Pid, list_bids),
        ?assertEqual(2, length(Bids)),
        ?assertMatch({bid, 2, {100, 0}, _Time, false}, lists:nth(2, Bids))
    end.
