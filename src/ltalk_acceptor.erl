%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk_acceptor
-module(ltalk_acceptor).

-export([start_link/2]).
-include("ltalk_cmd.hrl").

start_link(Server, Listen) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen]).

init(Server,ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok,Socket} ->
			 error_logger:info_msg("receive one~n"),
			gen_server:cast(Server, {accepted,self()}),
			loop(Socket);
		{error,Reason} ->
			exit(Reason)
	end.



loop(Socket) ->
	case ltalk_socket:recv(Socket, 0) of
		{ok,Packet} ->
			Line = ltalk_decode:getLine(Packet),
			Req = ltalk_request:new(Socket, Line),
			Req:handle(),
			loop(Socket);
		{error,Reason} ->
			gen_tcp:close(Socket),
			exit(Reason)
	end.






%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.