%%% -------------------------------------------------------------------
%%% Author  : chao
%%% Description :
%%%
%%% Created : May 1, 2012
%%% -------------------------------------------------------------------
-module(ltalk_user_server).

-behaviour(gen_server).

-export([get/1,
		 exist/1,
		 save_or_update/1,
		 delete/1]).

-include("ltalk_cmd.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab}).


get(Sock) ->
	gen_server:call(?MODULE, {get,Sock});

get(all) ->
	gen_server:call(?MODULE, {get,all}).

exist(Name) ->
	gen_server:call(?MODULE, {exist,Name}).

save_or_update(Onliner) ->
	gen_server:call(?MODULE, {save_or_update,Onliner}).

delete(all) ->
	gen_server:call(?MODULE, {delete,all});

delete(Sock) ->
	gen_server:call(?MODULE, {delete,Sock}).

init([]) ->
	Tab = ets:new(?MODULE, [set,protected,{keypos,2}]),
    {ok, #state{tab=Tab}}.

handle_call({get,all}, From, State) ->
	Reply = ets:select(State#state.tab,[{{'$1','$2','$3','$4','$5'},[],['$_']}]),
    {reply, Reply, State};

handle_call({get,Key}, From, State) ->
	Reply = ets:lookup(State#state.tab, Key),
    {reply, Reply, State};

handle_call({exist,Name}, From, State) ->
	Reply = case ets:match(State#state.tab,{'_','_',Name,'_','$1'}) of
				[] ->
					false;
				_ ->
					true
			end,
    {reply, Reply, State};

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
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?assertEqual([R1],?MODULE:get(1)).

get_all_test() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?assertEqual([R1,R2,R3],?MODULE:get(all)).

update_test() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	
	R4 = #onliner{socket=3,name="chao",state=2,talkto=[]},
	
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?assertEqual([R3],?MODULE:get(3)),
	save_or_update(R4),
	?assertEqual(3,length(?MODULE:get(all))),
	?assertEqual([R4],?MODULE:get(3)).

exist_test() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?assertEqual(false,?MODULE:exist("jias0")),
	?assertEqual(true,?MODULE:exist("jias1")).

delete_all_test() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?MODULE:delete(all),
	
	?assertEqual([],?MODULE:get(all)).

delete_test() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []),
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	save_or_update(R1),
	save_or_update(R2),
	save_or_update(R3),
	
	?MODULE:delete(2),
	
	?assertEqual(false,?MODULE:exist("jias1")),
	?assertEqual([],?MODULE:get(2)).


-endif.

