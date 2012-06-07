%%% -------------------------------------------------------------------
%%% Author  : chao
%%% Description :
%%%
%%% Created : May 1, 2012
%%% -------------------------------------------------------------------
-module(ltalk_onliner_server).

-behaviour(gen_server).
-include("ltalk_cmd.hrl").

-export([
		start_link/0,
		stop/0,
		get/1,
		get/2,
		save/1,
		delete/1
	]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab}).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%%get all onliner users from memory
get(all) ->
	get(undefined,undefined).


%% get list of onliners,return {ok,[Values]},but if key is
%% socket or name,the return value will be
%% {error,not_found} | {ok,Value}
get(Key,Value) when Key == undefined ->
	Express = #onliner{_='_'},
	gen_server:call(?MODULE, {get,Express});

get(Key,Value) when Key == name ->
	Express = #onliner{name=Value, _='_'},
	case gen_server:call(?MODULE, {get,Express}) of
		{ok,[]} ->
			{error,not_found};
		{ok,L} ->
			{ok,hd(L)}
	end;

get(Key,Value) when Key == socket ->
	Express = #onliner{socket=Value, _='_'},
	case gen_server:call(?MODULE, {get,Express}) of
		{ok,[]} ->
			{error,not_found};
		{ok,L} ->
			{ok,hd(L)}
	end;

get(Key,Value) when Key == state ->
	Express = #onliner{state=Value, _='_'},
	gen_server:call(?MODULE, {get,Express}).

%%save a user to memory
save(Onliner) ->
	gen_server:call(?MODULE, {save_or_update,Onliner}).

%%delete user from memory by user Name
%%if Name is all ,will delete all users
delete(all) ->
	gen_server:call(?MODULE, {delete,all});

%%delete one user from memory 
delete(Name) ->
	gen_server:call(?MODULE, {delete,Name}).


init([]) ->
	Tab = ets:new(?MODULE, [ordered_set,protected,{keypos,2}]),
    {ok, #state{tab=Tab}}.


%% return {ok,[Object]}
handle_call({get,Express}, From, State) ->
	Reply = ets:match_object(State#state.tab, Express),
    {reply, {ok,Reply}, State};

handle_call({delete,all}, From, State) ->
	Reply = ets:delete_all_objects(State#state.tab),
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

handle_cast(stop, State) ->
    {stop, normal, State};

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


get_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,R1},?MODULE:get(socket,1)).

get_all_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,[R1,R2,R3]},?MODULE:get(all)).

update_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	R4 = #onliner{socket=3,name="chao",state=2,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,R3},?MODULE:get(name,"jias2")),
	?MODULE:save(R4),
	?assertEqual({ok,R4},?MODULE:get(name,"chao")).

get_by_name_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	?MODULE:save(R1),
	?assertEqual({ok,R1},?MODULE:get(name,"jias")).

exist_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({error,not_found},?MODULE:get(name,"jias0")),
	?assertEqual({ok,R2},?MODULE:get(name,"jias1")).

delete_all_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?MODULE:delete(all),
	
	?assertEqual({ok,[]},?MODULE:get(all)).

delete_by_name_test() ->
	?MODULE:start_link(),
	?MODULE:delete(all),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?MODULE:delete("jias1"),
	
	?assertEqual({error,not_found},?MODULE:get(socket,2)).

-endif.

