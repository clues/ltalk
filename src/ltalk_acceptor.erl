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
			Req = ltalk_request:new(Socket),
			Req:response(?INFO_WELCOME),
			loop(Req);
		{error,Reason} ->
			exit(Reason)
	end.



%%receive message from socket process,and check package
%%size and end tag(\r\n) at the first step,if package ok
%%will remove end tag from package then handle it
loop(Req) ->
	case ltalk_socket:recv(Req:get(socket), 0) of
		{ok,Packet} when ?MAX_LEN_INFO < length(Packet) orelse 2 > length(Packet) ->
			ltalk_log:warn("~p -- received message size:(~p) not meet required from socket: ~p",
						    [?MODULE,length(Packet),Req:get(socket)]),
			Req:response(?INFO_LEN_NOT_MEET),
			loop(Req);
		{ok,Packet} ->
			Len = length(Packet),
			EndTag = lists:sublist(Packet,Len-1,Len),
			if
				?END_TAG_L == EndTag ->
					Req:handle(lists:sublist(Packet, Len-2));
				true ->
					ltalk_log:warn("~p -- received message:~p have incomplete end tag from socket:~p",
								    [?MODULE,Packet,Req:get(socket)]),
					Req:response(?INFO_INCOMPLETE)
			end,			
			loop(Req);		
		{error,Reason} ->
			ltalk_log:error("~p -- receive have error: ~p,will terminate socket: ~p", 
							[?MODULE,Reason,Req:get(socket)]),
			gen_tcp:close(Req:get(socket)),
			exit(Reason)
	end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.