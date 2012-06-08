%% Author: chao
%% Created: Apr 30, 2012
%% Description: TODO: Add description to ltalk_socket
-module(ltalk_socket).

-compile(export_all).



accept(Listen) ->
	gen_tcp:accept(Listen).

send(Sock,Packet) ->
	gen_tcp:send(Sock, Packet).

recv(Sock,Length) ->
	gen_tcp:recv(Sock, Length).

peername(Socket) ->
	inet:peername(Socket).

%%format socket address like: "192.168.1.1#port8080" | "ip6#port8080" 
peername_str(Socket) ->
	case inet:peername(Socket) of
		{ok,{{A1,A2,A3,A4},Port}} ->
			lists:concat([A1,".",A2,".",A3,".",A4,"#port",Port]);
		{ok,{Addr,Port}} -> %% ip6 not support well
			lists:concat(["ip6#port",Port])
	end.
	