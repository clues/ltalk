%%% -------------------------------------------------------------------
%%% Author  : wave
%%% Description :this db main support  storage user info,
%%% and chats to disk in the present
%%%
%%% Created : 2012-5-2 
%%% -------------------------------------------------------------------
-module(ltalk_db_server).

-behaviour(gen_server).

-include("ltalk_cmd.hrl").

-export([start_link/0,
		 stop/0,
		 save/2,
		 delete/2,
		 empty/1,
		 get/2
		]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% save(TabName,Record)->ok | {error,Reason}
%% will save or update Record internel
save(TabName,Record) ->
	gen_server:call(?MODULE, {save_or_update,TabName,Record}).

%% get list of records from table TabName by Key,
%% return {error,read_failed} | {ok,[Records]}
get(TabName,Key) ->
	gen_server:call(?MODULE, {get,TabName,Key}).

%% delete table TabName by Key
delete(TabName,Key) ->
	gen_server:cast(?MODULE, {remove,TabName,Key}).
 
%% clear all object from table TabName,table will exist still
empty(TabName) ->
	gen_server:cast(?MODULE, {clear_table,TabName}).

start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, {stop,normal}).

save_or_update(TabName,Record) ->     
	Result = mnesia:sync_transaction(
		fun() ->
				mnesia:write(TabName,Record,write)
		end	
	).

get_records(TabName, Key) ->
	Result = mnesia:sync_transaction(
		fun() ->
				mnesia:read(TabName, Key)
		end	
	).

clear_table(TabName) ->
	mnesia:clear_table(TabName).

remove(TabName,Key) ->
	mnesia:sync_transaction(
		fun() ->
				mnesia:delete({TabName,Key})
		end	
	).

create_tables(?TAB_USER) ->
	{atomic, ok} = mnesia:create_table(?TAB_USER,
						[{attributes, record_info(fields, ?TAB_USER)},
		 				{type,bag},
		 				{disc_only_copies, [node()]}]);

create_tables(?TAB_CHAT) ->
	{atomic, ok} = mnesia:create_table(?TAB_CHAT,
						[{attributes, record_info(fields, ?TAB_CHAT)},
		 				{type,bag},
		 				{disc_only_copies, [node()]}]).

init([]) ->
	stopped = mnesia:stop(),
	case mnesia:create_schema([node()]) of
		ok ->
			mnesia:start(),
			{atomic, ok} = create_tables(?TAB_USER),
			{atomic, ok} = create_tables(?TAB_CHAT),
			{ok, #state{}};
		{error,{_,{already_exists,_}}} ->
			mnesia:start(),
			yes = mnesia:force_load_table(?TAB_USER),
			{ok, #state{}}
	end.

handle_call({stop,Reason}, _From, State) ->
	{stop, Reason, ok, State};

handle_call({get,TabName,Key}, _From, State) ->
    case get_records(TabName, Key) of
		{atomic,Records} ->		
			{reply, {ok,Records}, State};
		_ ->
			{reply, {error,read_failed}, State}
	end;
handle_call({save_or_update,TabName,Record}, _From, State) ->
	case save_or_update(TabName,Record) of
		{atomic, _}  -> 
			{reply, ok, State};
		{aborted, Reason} ->
			{reply, {error,Reason}, State}
	end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({clear_table,TabName}, State) ->
	clear_table(TabName),
    {noreply, State};

handle_cast({remove,TabName,Key}, State) ->
	remove(TabName,Key),
    {noreply, State};

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

save_records_test() ->
	?MODULE:start_link(),
	?MODULE:empty(?TAB_USER),
	?MODULE:empty(?TAB_CHAT),
	
	U1 = #user{name="jias"},
	C1 = #chat{owner="jias",talkto="gate",data= <<"take care!">>},

	?MODULE:save(?TAB_USER, U1),
	?MODULE:save(?TAB_CHAT, C1),
	
	?assertEqual(ok,?MODULE:save(?TAB_USER, U1)),
	?assertEqual(ok,?MODULE:save(?TAB_CHAT, C1)).

get_records_test() ->
	?MODULE:start_link(),
	?MODULE:empty(?TAB_USER),
	?MODULE:empty(?TAB_CHAT),
	
	U1 = #user{name="jias"},
	U2 = #user{name="jias1"},
	?MODULE:save(?TAB_USER, U1),
	?MODULE:save(?TAB_USER, U2),
	
	C1 = #chat{owner="jack",talkto="eric",data= <<"take care!">>},
	C2 = #chat{owner="jack",talkto="tom",data= <<"take care!">>},
	?MODULE:save(?TAB_CHAT, C1),
	?MODULE:save(?TAB_CHAT, C2),	
	
	?assertEqual({ok,[U2]},?MODULE:get(?TAB_USER, "jias1")),
	?assertEqual({ok,[C1,C2]},?MODULE:get(?TAB_CHAT, "jack")).

delete_record_test() ->
	?MODULE:start_link(),
	?MODULE:empty(?TAB_USER),
	?MODULE:empty(?TAB_CHAT),
	
	U1 = #user{name="jias"},
	C1 = #chat{owner="jack",talkto="tom",data= <<"take care!">>},

	?MODULE:save(?TAB_USER, U1),
	?MODULE:save(?TAB_CHAT, C1),
	?assertEqual({ok,[U1]},?MODULE:get(?TAB_USER, "jias")),
	?assertEqual({ok,[C1]},?MODULE:get(?TAB_CHAT, "jack")),
	
	?MODULE:delete(?TAB_USER, "jias"),
	?MODULE:delete(?TAB_CHAT, "jack"),
	?assertEqual({ok,[]},?MODULE:get(?TAB_USER, "jias")),
	?assertEqual({ok,[]},?MODULE:get(?TAB_CHAT, "jack")).

-endif.


