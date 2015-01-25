-module(eb_auction).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

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
