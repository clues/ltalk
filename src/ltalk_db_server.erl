%%% -------------------------------------------------------------------
%%% Author  : wave
%%% Description :
%%%
%%% Created : 2012-5-2
%%% -------------------------------------------------------------------
-module(ltalk_db_server).

-behaviour(gen_server).

-define(TAB_USER,user).

-export([start_link/0,
		 stop/1,
		save/2,
		get/2]).

-include("ltalk_cmd.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab_name}).


save(user,User) ->
	gen_server:call(?MODULE, {save_or_update,user,User}).

get(user,Name) ->
	gen_server:call(?MODULE, {get,user,Name}).

start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop(Reason) ->
	gen_server:call(?MODULE, {stop,Reason}).

init([]) ->
	stopped = mnesia:stop(),
	case mnesia:create_schema([node()]) of
		ok ->
			
			mnesia:start(),
			{atomic, ok} = create_tables(),
			{ok, #state{tab_name = ?TAB_USER}};
		{error,{_,{already_exists,_}}} ->
			mnesia:start(),
			yes = mnesia:force_load_table(?TAB_USER),
			{ok, #state{tab_name = ?TAB_USER}}
	end.

handle_call({stop,Reason}, _From, State) ->
	{stop, Reason, ok, State};

handle_call({get,user,Name}, _From, #state{tab_name=TabName}=State) ->
    case get_user(TabName, Name) of
		{atomic,[User|_]} ->		
			{reply, {ok,User}, State};
		_ ->
			{reply, {error,not_found}, State}
	end;
handle_call({save_or_update,user,User}, _From, #state{tab_name=TabName}=State) ->
	case save_or_update_user(TabName,User) of
		{atomic, _}  -> 
			{reply, ok, State};
		{aborted, Reason} ->
			io:format("~p~n", [Reason]),
			{reply, {error,store_failed}, State}
	end;

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

create_tables() ->
	{atomic, ok} = mnesia:create_table(?TAB_USER,
						[{attributes, record_info(fields, user)},
		 				{type,bag},
		 				{disc_only_copies, [node()]}]).

save_or_update_user(TabName,User) ->     
	Result = mnesia:sync_transaction(
		fun() ->
				mnesia:write(TabName,User,write)
		end	
	).

get_user(TabName, Name) ->
	Result = mnesia:sync_transaction(
		fun() ->
				mnesia:read(TabName, Name)
		end	
	).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

save_user_test() ->
	?MODULE:start_link(),
	U1 = #user{name="jias"},
	?MODULE:save(user, U1),
	
	?assertEqual(ok,?MODULE:save(user, U1)).

get_user_test() ->
	?MODULE:start_link(),
	U1 = #user{name="jias"},
	U2 = #user{name="jias1"},
	?MODULE:save(user, U1),
	?MODULE:save(user, U2),
	
	?assertEqual({ok,U2},?MODULE:get(user, "jias1")).

-endif.


