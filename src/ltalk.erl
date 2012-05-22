%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk
-module(ltalk).

-export([start/1,
		 stop/0]).



start(Port) ->
	ltalk_socket_server:start([{port,Port}]),
	ok.


stop() ->
	ltalk_socket_server:stop().
