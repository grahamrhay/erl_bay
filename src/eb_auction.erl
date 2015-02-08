-module(eb_auction).
-behaviour(gen_server).

%% API.
-export([start_link/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type user_id() :: integer().
-record(bid, {bidder :: user_id(), amount :: eb_money:money(), time :: calendar:datetime(), automatic = false :: boolean()}).
-record(state, {starting_price :: eb_money:money(), bids = [] :: [#bid{}], high_bid :: #bid{}, reserve_price :: eb_money:money() | undefined}).

%% API.

-spec start_link(eb_money:money(), calendar:datetime(), eb_money:money() | undefined) -> {ok, pid()}.
start_link(StartingPrice, EndTime, ReservePrice) ->
    gen_server:start_link(?MODULE, [StartingPrice, EndTime, ReservePrice], []).

%% gen_server.

init([StartingPrice, EndTime, ReservePrice]) ->
    lager:info("Bidding begins. Starting price: ~p", [StartingPrice]),
    TimeRemaining = calculate_time_remaining(EndTime),
    lager:info("Time remaining: ~p", [TimeRemaining]),
    _TimerRef = erlang:send_after(TimeRemaining, self(), bidding_ends),
    {ok, #state{starting_price = StartingPrice, reserve_price = ReservePrice}}.

handle_call({bid, _UserId, MaxBid}, _From, State) when MaxBid < State#state.starting_price ->
    {reply, bid_too_low, State};

handle_call({bid, UserId, MaxBid}, _From, State) when State#state.bids =:= [] ->
    BidTime = calendar:universal_time(),
    Bid = get_starting_bid(State, MaxBid, UserId, BidTime),
    HighBid = #bid{bidder = UserId, amount = MaxBid, time = BidTime},
    NewState = State#state{bids = [Bid | State#state.bids], high_bid = HighBid},
    {reply, bid_accepted, NewState};

handle_call({bid, UserId, MaxBid}, _From, State) ->
    HighBid = State#state.high_bid,
    case HighBid#bid.bidder =:= UserId of
        true ->
            check_bid_for_high_bidder(State, UserId, MaxBid);
        false ->
            handle_new_bid(State, MaxBid, UserId)
    end;

handle_call(list_bids, _From, State) ->
    {reply, lists:reverse(State#state.bids), State};

handle_call(reserve_met, _From, State) ->
    {reply, reserve_met(State), State};

handle_call(Request, _From, State) ->
    lager:error("Unexpected call: ~p", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:error("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(bidding_ends, State) when State#state.bids =:= [] ->
    lager:info("Bidding ends", []),
    lager:info("No winner", []),
    {stop, normal, State};

handle_info(bidding_ends, State) ->
    lager:info("Bidding ends", []),
    case reserve_met(State) of
        true ->
            WinningBid = hd(State#state.bids),
            lager:info("Winner: ~p, Bid: ~p", [WinningBid#bid.bidder, WinningBid#bid.amount]);
        false ->
            lager:info("Reserve not met", [])
    end,
    {stop, normal, State};

handle_info(Info, State) ->
    lager:error("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_bid_for_high_bidder(State, UserId, MaxBid) ->
    HighBid = State#state.high_bid,
    case MaxBid > HighBid#bid.amount of
        true ->
            update_high_bid_for_bidder(State, UserId, MaxBid);
        false ->
            {reply, bid_too_low, State}
    end.

update_high_bid_for_bidder(State, UserId, MaxBid) ->
    BidTime = calendar:universal_time(),
    NewState = case State#state.reserve_price =/= undefined of
        true ->
            check_reserve_price_for_high_bidder(State, UserId, MaxBid, BidTime);
        false ->
            State#state{high_bid = #bid{bidder = UserId, amount = MaxBid, time = BidTime}}
    end,
    {reply, bid_accepted, NewState}.

check_reserve_price_for_high_bidder(State, UserId, MaxBid, BidTime) ->
    Bid = case MaxBid < State#state.reserve_price of
        true ->
            #bid{bidder = UserId, amount = MaxBid, time = BidTime};
        false ->
            #bid{bidder = UserId, amount = State#state.reserve_price, time = BidTime}
    end,
    State#state{bids = [Bid | State#state.bids], high_bid = #bid{bidder = UserId, amount = MaxBid, time = BidTime}}.

handle_new_bid(State, MaxBid, UserId) ->
    CurrentBid = hd(State#state.bids),
    case MaxBid =< CurrentBid#bid.amount of
        true ->
            {reply, bid_too_low, State};
        false ->
            handle_valid_new_bid(State, MaxBid, UserId)
    end.

handle_valid_new_bid(State, MaxBid, UserId) ->
    BidTime = calendar:universal_time(),
    NewState = case State#state.reserve_price =/= undefined of
        true ->
            check_reserve_price_for_high_bidder(State, UserId, MaxBid, BidTime);
        false ->
            HighBid = State#state.high_bid,
            case MaxBid =< HighBid#bid.amount of
                true ->
                    bid_lower_than_or_equal_to_high_bid(State, MaxBid, UserId, BidTime, HighBid);
                false ->
                    bid_higher_than_current_high_bid(State, MaxBid, UserId, BidTime, HighBid)
            end
    end,
    {reply, bid_accepted, NewState}.

bid_lower_than_or_equal_to_high_bid(State, MaxBid, UserId, BidTime, HighBid) ->
    LosingBid = #bid{bidder=UserId, amount=MaxBid, time=BidTime},
    update_bid_list(State, LosingBid, HighBid).

bid_higher_than_current_high_bid(State, MaxBid, UserId, BidTime, HighBid) ->
    WinningBid = #bid{bidder=UserId, amount=MaxBid, time=BidTime},
    update_bid_list(State, HighBid, WinningBid).

update_bid_list(State, LosingBid, WinningBid) ->
    NewAmount = eb_money:add(LosingBid#bid.amount, eb_bid_increments:get(LosingBid#bid.amount)),
    NewHighBid = case NewAmount < WinningBid#bid.amount of
        true ->
            #bid{bidder = WinningBid#bid.bidder, amount = NewAmount, time = WinningBid#bid.time, automatic = true};
        false ->
            #bid{bidder = WinningBid#bid.bidder, amount = WinningBid#bid.amount, time = WinningBid#bid.time, automatic = false}
    end,
    State#state{bids = [NewHighBid | [LosingBid | State#state.bids]]}.

calculate_time_remaining(EndTime) ->
    Now = calendar:universal_time(),
    (calendar:datetime_to_gregorian_seconds(EndTime) - calendar:datetime_to_gregorian_seconds(Now)) * 1000. %% milliseconds!

get_starting_bid(State, MaxBid, UserId, BidTime) ->
    case State#state.reserve_price =/= undefined of
        true ->
            get_starting_bid_with_reserve(State, MaxBid, UserId, BidTime);
        false ->
            #bid{bidder = UserId, amount = State#state.starting_price, time = BidTime}
    end.

get_starting_bid_with_reserve(State, MaxBid, UserId, BidTime) ->
    case MaxBid < State#state.reserve_price of
        true ->
            #bid{bidder = UserId, amount = MaxBid, time = BidTime};
        false ->
            #bid{bidder = UserId, amount = State#state.reserve_price, time = BidTime}
    end.

reserve_met(State) when State#state.reserve_price =:= undefined ->
    true;

reserve_met(State) when State#state.bids =:= [] ->
    false;

reserve_met(State) ->
    CurrentBid = hd(State#state.bids),
    CurrentBid#bid.amount >= State#state.reserve_price.
