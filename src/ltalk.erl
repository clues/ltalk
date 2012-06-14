%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk
%% this module puporse start other servers and monitor
%% them
-module(ltalk).

-include("ltalk_cmd.hrl").

-export([start/1,
		 stop/0]).

%% for script start
start([Port]) when is_atom(Port) ->
        start(list_to_integer(atom_to_list(Port)));

start(Port) when is_integer(Port) ->
	ltalk_log:start_link(),
	ltalk_db_server:start_link(),
	ltalk_onliner_server:start_link(),
	ltalk_socket_server:start_link([{port,Port}]),
	ok.


stop() ->
	ltalk_db_server:stop(),
	ltalk_onliner_server:stop(),
	ltalk_socket_server:stop().



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% client_test() ->
%% 	?MODULE:start(4017),
%% 	
%% 	{ok,S} = gen_tcp:connect("127.0.0.1", 4017, [list,{active,true}]),
%% 	wait_msg(),
%% 	gen_tcp:send(S,packet( "-help~n")),
%% 	wait_msg(),
%% 	
%% 	?MODULE:stop(),
%% 	ok.

packet(Msg) ->
	list_to_binary(Msg).

wait_msg() ->
	receive
		Msg ->
				error_logger:info_msg("~p -- receive: ~p~n", [?MODULE,Msg])
	after 2000 ->
			error_logger:info_msg("~p -- wait msg timeout~n",[?MODULE])
	end.
	
	
-endif.