%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk
%% this module puporse start other servers and monitor
%% them
-module(ltalk).

-include("ltalk_cmd.hrl").

-export([start/1,
		 stop/0]).



start(Port) ->
	ltalk_socket_server:start([{port,Port}]),
	ok.


stop() ->
	ltalk_socket_server:stop().



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.