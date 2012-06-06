%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk_acceptor
-module(ltalk_acceptor).

-export([
		 start_link/2,
		 init/2,
		 loop/1
		 ]).
-include("ltalk_cmd.hrl").

start_link(Server, Listen) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen]).

init(Server,ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok,Socket} ->
			gen_server:cast(Server, {accepted,self()}),
			ltalk_socket:send(Socket, ?INFO_WELCOME),
			Req = ltalk_request:new(Socket),
			loop(Req);
		{error,Reason} ->
			exit(Reason)
	end.



loop(Req) ->
	case ltalk_socket:recv(Req:get(socket), 0) of
		{ok,Packet} ->
			Req:handle(Packet),
			loop(Req);
		{error,Reason} ->
			gen_tcp:close(Req:get(socket)),
			exit(Reason)
	end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.