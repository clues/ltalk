%%% -------------------------------------------------------------------
%%% Author  : chao
%%% Description :
%%%
%%% Created : May 1, 2012
%%% -------------------------------------------------------------------
-module(ltalk_user_server).

-behaviour(gen_server).

-export([]).
-include("ltalk_cmd.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab}).

insert(Onliner) ->
	gen_server:call(?MODULE, {insert,Onliner}).


init([]) ->
	Tab = ets:new(?MODULE, [set,protected,{keypos,1}]),
    {ok, #state{tab=Tab}}.

handle_call({get,Key}, From, State) ->
	Reply = ets:lookup(State#state.tab, Key),
    {reply, Reply, State};

handle_call({delte,Key}, From, State) ->
	Reply = ets:all(),
    {reply, Reply, State};

handle_call({delete,Key}, From, State) ->
	Reply = ets:delete(State#state.tab, Key),
    {reply, Reply, State};

handle_call({save_or_update,Onliner}, From, State) ->
	Reply = ets:insert(State#state.tab, Onliner),
    {reply, Reply, State};

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


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

error_format_test() ->
	Line = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},
	HandledLine = #line{data="" ++ ?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line#line.data,
						state=?RESPONSE_CODE_ERROR,cmd=Line#line.cmd},
	?assertEqual(HandledLine,handle(sock,Line)).

-endif.

