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
		get/1,
		 exist/1,
		 save/1,
		 get_by_name/1,
		 delete/1
	]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab}).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

%% get online user  by id:socket
get(Sock) ->
	gen_server:call(?MODULE, {get,Sock});

%%get all online users from memory
get(all) ->
	gen_server:call(?MODULE, {get,all}).

%%tell online user whether exist or not by onlier name
exist(Name) ->
	gen_server:call(?MODULE, {exist,Name}).

%%get onliner by name
get_by_name(Name) ->
	gen_server:call(?MODULE, {get_by_name,Name}).

%%save a user to memory
save(Onliner) ->
	gen_server:call(?MODULE, {save_or_update,Onliner}).

%%delete all user from memory
delete(all) ->
	gen_server:call(?MODULE, {delete,all});

%%delete one user from memory by id:socket
delete(Sock) ->
	gen_server:call(?MODULE, {delete,Sock}).

init([]) ->
	Tab = ets:new(?MODULE, [set,protected,{keypos,2}]),
    {ok, #state{tab=Tab}}.

handle_call({get,all}, From, State) ->
	Reply = ets:select(State#state.tab,[{{'$1','$2','$3','$4','$5'},[],['$_']}]),
    {reply, {ok,Reply}, State};

handle_call({get,Key}, From, State) ->
	case ets:lookup(State#state.tab, Key) of
		[H|T] ->
			{reply, {ok,H}, State};
		_ ->
    		{reply, {error,not_found}, State}
	end;

handle_call({get_by_name,Name}, From, State) ->
	Reply = case ets:match(State#state.tab,{'_','_',Name,'_','$1'}) of
				[] ->
					{error,not_found};
				Onliner ->
					{ok,Onliner}
			end,
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
	?MODULE:start_link(),
	
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,R1},?MODULE:get(1)).

get_all_test() ->
	?MODULE:start_link(),

	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,[R1,R2,R3]},?MODULE:get(all)).

update_test() ->
	?MODULE:start_link(),
	
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	R4 = #onliner{socket=3,name="chao",state=2,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual({ok,R3},?MODULE:get(3)),
	?MODULE:save(R4),
	?assertEqual(3,length(element(2,?MODULE:get(all)))),
	?assertEqual({ok,R4},?MODULE:get(3)).

get_by_name_test() ->
	?MODULE:start_link(),
	
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	?MODULE:save(R1),
	?assertEqual(true,?MODULE:exist("jias")),
	?assertEqual(R1,?MODULE:get_by_name("jias")).

exist_test() ->
	?MODULE:start_link(),
	
	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?assertEqual(false,?MODULE:exist("jias0")),
	?assertEqual(true,?MODULE:exist("jias1")).

delete_all_test() ->
	?MODULE:start_link(),

	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?MODULE:delete(all),
	
	?assertEqual({ok,[]},?MODULE:get(all)).

delete_test() ->
	?MODULE:start_link(),

	R1 = #onliner{socket=1,name="jias",state=0,talkto=[]},
	R2 = #onliner{socket=2,name="jias1",state=0,talkto=[]},
	R3 = #onliner{socket=3,name="jias2",state=1,talkto=[]},
	?MODULE:save(R1),
	?MODULE:save(R2),
	?MODULE:save(R3),
	
	?MODULE:delete(2),
	
	?assertEqual(false,?MODULE:exist("jias1")),
	?assertEqual({error,not_found},?MODULE:get(2)).


-endif.

