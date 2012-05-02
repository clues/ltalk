%%% -------------------------------------------------------------------
%%% Author  : wave
%%% Description :
%%%
%%% Created : 2012-4-26
%%% -------------------------------------------------------------------
-module(ltalk_name_server).

-behaviour(gen_server).

-export([]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).



init([]) ->
    {ok, #state{}}.

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


