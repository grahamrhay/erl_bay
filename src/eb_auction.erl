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

-record(state, {starting_price, bids=[]}).
-record(bid, {bidder, amount, time}).

%% API.

-type money() :: {integer(), integer()}.
-spec start_link([money()]) -> {ok, pid()}.
start_link(StartingPrice) ->
    gen_server:start_link(?MODULE, [StartingPrice], []).

%% gen_server.

init([StartingPrice]) ->
    {ok, #state{starting_price = StartingPrice}}.

handle_call({bid, _UserId, Bid}, _From, State) when Bid < State#state.starting_price ->
    {reply, bid_too_low, State};

handle_call({bid, UserId, BidAmount}, _From, State) when State#state.bids =:= [] ->
    Bid = #bid{bidder = UserId, amount = BidAmount, time = calendar:universal_time()},
    NewState = State#state{bids = [Bid | State#state.bids]},
    {reply, bid_accepted, NewState};

handle_call({bid, UserId, BidAmount}, _From, State) ->
    CurrentHighBid = hd(State#state.bids),
    case BidAmount > CurrentHighBid#bid.amount of
        true ->
            Bid = #bid{bidder = UserId, amount = BidAmount, time = calendar:universal_time()},
            NewState = State#state{bids = [Bid | State#state.bids]},
            {reply, bid_accepted, NewState};
        false ->
            {reply, bid_too_low, State}
    end;

handle_call(list_bids, _From, State) ->
    {reply, State#state.bids, State};

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
