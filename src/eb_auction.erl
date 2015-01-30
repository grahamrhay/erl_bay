-module(eb_auction).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type user_id() :: integer().
-record(bid, {bidder :: user_id(), amount :: eb_money:money(), time :: calendar:datetime(), automatic :: boolean()}).
-record(state, {starting_price :: eb_money:money(), bids = [] :: [#bid{}], high_bid :: #bid{}}).

%% API.

-spec start_link([eb_money:money()]) -> {ok, pid()}.
start_link(StartingPrice) ->
    gen_server:start_link(?MODULE, [StartingPrice], []).

%% gen_server.

init([StartingPrice]) ->
    {ok, #state{starting_price = StartingPrice}}.

handle_call({bid, _UserId, MaxBid}, _From, State) when MaxBid < State#state.starting_price ->
    {reply, bid_too_low, State};

handle_call({bid, UserId, MaxBid}, _From, State) when State#state.bids =:= [] ->
    BidTime = calendar:universal_time(),
    Bid = #bid{bidder = UserId, amount = State#state.starting_price, time = BidTime, automatic = false},
    NewState = State#state{bids = [Bid | State#state.bids], high_bid = #bid{bidder = UserId, amount = MaxBid, time = BidTime}},
    {reply, bid_accepted, NewState};

handle_call({bid, UserId, MaxBid}, _From, State) ->
    HighBid = State#state.high_bid,
    case HighBid#bid.bidder =:= UserId of
        true ->
            update_bid_for_high_bidder(State, UserId, MaxBid);
        false ->
            handle_new_bid(State, MaxBid, UserId)
    end;

handle_call(list_bids, _From, State) ->
    {reply, lists:reverse(State#state.bids), State};

handle_call(Request, _From, State) ->
    lager:error("Unexpected call: ~p", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:error("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_bid_for_high_bidder(State, UserId, MaxBid) ->
    NewState = State#state{high_bid = #bid{bidder = UserId, amount = MaxBid, time = calendar:universal_time()}},
    {reply, bid_accepted, NewState}.

handle_new_bid(State, MaxBid, UserId) ->
    CurrentBid = hd(State#state.bids),
    case MaxBid =< CurrentBid#bid.amount of
        true ->
            {reply, bid_too_low, State};
        false ->
            HighBid = State#state.high_bid,
            BidTime = calendar:universal_time(),
            case MaxBid =< HighBid#bid.amount of
                true ->
                    Bid = #bid{bidder = UserId, amount = MaxBid, time = BidTime, automatic = false},
                    NewAmount = eb_money:add(MaxBid, eb_bid_increments:get(MaxBid)),
                    NewHighBid = case NewAmount < HighBid#bid.amount of
                        true ->
                            #bid{bidder = HighBid#bid.bidder, amount = NewAmount, time = HighBid#bid.time, automatic = true};
                        false ->
                            #bid{bidder = HighBid#bid.bidder, amount = HighBid#bid.amount, time = HighBid#bid.time, automatic = false}
                    end,
                    NewState = State#state{bids = [NewHighBid | [Bid | State#state.bids]]},
                    {reply, bid_accepted, NewState};
                false ->
                    Bid = #bid{bidder = HighBid#bid.bidder, amount = HighBid#bid.amount, time = HighBid#bid.time, automatic = false},
                    NewAmount = eb_money:add(HighBid#bid.amount, eb_bid_increments:get(HighBid#bid.amount)),
                    NewHighBid = case NewAmount < MaxBid of
                        true ->
                            #bid{bidder = UserId, amount = NewAmount, time = BidTime, automatic = true};
                        false ->
                            #bid{bidder = UserId, amount = MaxBid, time = BidTime, automatic = false}
                    end,
                    NewState = State#state{bids = [NewHighBid | [Bid | State#state.bids]]},
                    {reply, bid_accepted, NewState}
            end
    end.
