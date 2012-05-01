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

	