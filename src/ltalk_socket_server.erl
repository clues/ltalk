%%% -------------------------------------------------------------------
%%% Author  : wave
%%% Description :
%%%
%%% Created : 2012-4-26
%%% -------------------------------------------------------------------
-module(ltalk_socket_server).

-behaviour(gen_server).

-export([
		 start_link/0,
		 start_link/1,
		 stop/0
		]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_SOCKET_OPTS,[{active,false},
							 {packet,line}]).

-define(DEFAULT_LISTEN_PORT,4017).
-record(socket_server, {
		port, 
		max=2048,
        listen=null,
        active_sockets=0,
		acceptor_pool=sets:new(),
        acceptor_pool_size=16
		}).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [{port,?DEFAULT_LISTEN_PORT}], []).

start_link(Opts) ->
	gen_server:start_link({local,?MODULE},?MODULE, Opts, []).

stop() ->
	gen_server:cast(?MODULE, stop).

init(Opts) ->
	process_flag(trap_exit, true),
	Port = proplists:get_value(port, Opts),
	listen(#socket_server{port= Port}).

handle_call({get,state}, From, State) ->
	Reply = State,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({accepted,Pid},#socket_server{active_sockets=ActiveSockets,acceptor_pool=Pool}=State) ->
	  State1 = State#socket_server{active_sockets=1 + ActiveSockets},
	  {noreply, recycle_acceptor(Pid, State1)};
		
handle_cast(Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason},
            State=#socket_server{acceptor_pool=Pool}) ->
    case sets:is_element(Pid, Pool) of
        true ->
%%             error_logger:error_report({?MODULE, ?LINE,
%%                                        {acceptor_error, Reason}}),
            timer:sleep(100);
        false ->
            ok
    end,
    {noreply, recycle_acceptor(Pid, State)};

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, #socket_server{listen=Listen}) when Listen =/= null ->
	gen_tcp:close(Listen),
    ok;
terminate(Reason, State)  ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

recycle_acceptor(Pid, #socket_server{
                        acceptor_pool=Pool,
                        listen=Listen,
                        active_sockets=ActiveSockets} = State) ->
    case sets:is_element(Pid, Pool) of
        true ->
			
            Acceptor = ltalk_acceptor:start_link(self(), Listen),
            Pool1 = sets:add_element(Acceptor, sets:del_element(Pid, Pool)),
            State#socket_server{acceptor_pool=Pool1};
        false ->
            State#socket_server{active_sockets=ActiveSockets - 1}
    end.

new_acceptor_pool(#socket_server{acceptor_pool=Pool,
                                 acceptor_pool_size=Size} = State) ->
	F = fun (_, Pool) ->
                Pid = ltalk_acceptor:start_link(self(), State#socket_server.listen),
                sets:add_element(Pid, Pool)
        end,
	
    Pool1 = lists:foldl(F, Pool, lists:seq(1, Size)),
    State#socket_server{acceptor_pool=Pool1}.

listen(State) ->
		case gen_tcp:listen(State#socket_server.port, ?DEFAULT_SOCKET_OPTS) of
			{ok,ListenSocket} ->
				{ok,new_acceptor_pool(State#socket_server{listen=ListenSocket})};
			{error,Reason} ->
				{stop,Reason}
		end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

socket_server_init_test() ->
	ltalk_socket_server:start_link(),
	State = gen_server:call(?MODULE, {get,state}),
	
	timer:sleep(2000),
	?assertEqual(0, State#socket_server.active_sockets),
	?assertNotEqual(null, State#socket_server.listen),
    ?assertEqual(State#socket_server.acceptor_pool_size, sets:size(State#socket_server.acceptor_pool)).


socket_server_accept_one_test() ->
	ltalk_socket_server:start_link(),
    {ok,Socket} = gen_tcp:connect("192.168.208.27", ?DEFAULT_LISTEN_PORT, []),
	
	timer:sleep(2000),
	State = gen_server:call(?MODULE, {get,state}),
	
	?assertNotEqual(null, State#socket_server.listen),
    ?assertEqual(State#socket_server.acceptor_pool_size, sets:size(State#socket_server.acceptor_pool)),
	?assertEqual(1, State#socket_server.active_sockets),
	
	gen_tcp:close(Socket),
	ltalk_socket_server:stop(),
	ok.

-endif.


