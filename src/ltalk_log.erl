%% Author: wave
%% Created: 2012-6-6
%% Description: TODO: Add description to ltalk_log
%% simple print like java log info to console and file,style
-module(ltalk_log).
-include("ltalk_log.hrl").
-behaviour(gen_server).

-export([start_link/0,
		 stop/0,
		 change_level/1,
		 debug/1,
		 debug/2,
		 info/1,
		 info/2,
		 warn/1,
		 warn/2,
		 error/1,
		 error/2,
		 fatal/1,
		 fatal/2
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(Newline,"\r\n").
-record(state, {flog=#filelogger{},clog=#consolelogger{},filename}).

start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% debug(Msg::term());
debug(_Msg)->
	debug("~p",[_Msg]).

%% debug(Format::string(),Msg::term())
debug(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {debug,_Fmat,_Msg}).

%% info(Msg::term());
info(_Msg)->
	info("~p",[_Msg]).

%% info(Format::string(),Msg::term())
info(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {info,_Fmat,_Msg}).	

%% warn(Msg::term());
warn(_Msg)->
	warn("~p",[_Msg]).

%% warn(Format::string(),Msg::term())
warn(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {warn,_Fmat,_Msg}).	

%% error(Msg::term());
error(_Msg)->
	error("~p",[_Msg]).

%% error(Format::string(),Msg::term())
error(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {error,_Fmat,_Msg}).

%% fatal(Msg::term());
fatal(_Msg)->
	fatal("~p",[_Msg]).

%% fatal(Format::string(),Msg::term())
fatal(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {fatal,_Fmat,_Msg}).

%% change_level({Type,Level})
%% Type = console | file
%% Level = debug | info | warn | error| fatal
change_level({Type,Level}) ->
	gen_server:cast(?MODULE, {change_level,Type,Level}).


stop() ->
	gen_server:call(?MODULE, stop).


get_time() ->
	{{Y,M,D},{H,Mi,S}} = calendar:local_time(),
	Time = integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ 
	integer_to_list(D) ++ "  " ++ integer_to_list(H) ++ "-" ++ 
	integer_to_list(Mi) ++ "-" ++ integer_to_list(S),
	list_to_atom(Time).


init([]) ->
	State = #filelogger{},
	AbsPath = filename:join(State#filelogger.dir, State#filelogger.file),
	ok = filelib:ensure_dir(AbsPath),
	{ok,#state{flog=#filelogger{},clog=#consolelogger{},filename=AbsPath}}.


handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({change_level,Type,Level},State) when Type == ?TYPE_FILE ->
	Value = map_value(Level),
	{noreply, State#state{flog=#filelogger{level=Value}}};

handle_cast({change_level,Type,Level},State) when Type == ?TYPE_CONSOLE ->
	Value = map_value(Level),
	{noreply, State#state{clog=#consolelogger{level=Value}}};

handle_cast({Level,Fmat,Msg}, #state{clog=#consolelogger{level=Clevel},
									 flog=#filelogger{level=Flevel},
									 filename=Filename
									} = State) ->
	Value = map_value(Level),
	if 
		Value =< Clevel,Value =< Flevel ->
			print_to(?TYPE_ALL,{Level,Fmat,Msg,Filename});
		Value =< Clevel ->
			print_to(?TYPE_CONSOLE,{Level,Fmat,Msg,Filename});
		Value =< Flevel ->
			print_to(?TYPE_FILE,{Level,Fmat,Msg,Filename});		
		true->
			ingored
	end,
    {noreply, State}.


handle_info(Info, State) ->
    {noreply, State}.


terminate(Reason, State) ->
	ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.

print_to(?TYPE_ALL,{Level,Fmat,Msg,Filename}) ->
	print_to_console(Level,Fmat,Msg),
	print_to_file(Level,Fmat,Msg,Filename);

print_to(?TYPE_CONSOLE,{Level,Fmat,Msg,_Filename}) ->
	print_to_console(Level,Fmat,Msg);

print_to(?TYPE_FILE,{Level,Fmat,Msg,Filename}) ->
	print_to_file(Level,Fmat,Msg,Filename).
	
print_to_console(Level,Fmat,Msg) ->
	_Fmat = "[~p] ~p " ++ Fmat ++ "~n",	
	?PRINT(_Fmat,[Level,get_time()]++Msg).

print_to_file(Level,Fmat,Msg,Filename) ->
	_Fmat = "[~p] ~p " ++ Fmat ++ ?Newline,
	Data = io_lib:format(_Fmat, [Level,get_time()|Msg]),
	ok = file:write_file(Filename, Data, [binary,append]).

map_value(?DEBUG) ->
	?VALUE_DEBUG;
map_value(?INFO) ->
	?VALUE_INFO;
map_value(?WARN) ->
	?VALUE_WARN;
map_value(?ERROR) ->
	?VALUE_ERROR;
map_value(?FATAL) ->
	?VALUE_FATAL.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
	
info_warn_test() ->
	
	?MODULE:start_link(),
	?MODULE:info("hello"),
	?MODULE:change_level({?TYPE_CONSOLE,?WARN}),
	
	%%this info will not print on console
	?MODULE:info("world"),
	?MODULE:warn("warn map value : ~p", [?VALUE_WARN]),
	
	State = bagarg,
	?MODULE:error("erlang run error with reason: ~p", [State]),
	ok.
	
-endif.
