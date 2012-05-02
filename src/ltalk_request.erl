%% Author: wave
%% Created: 2012-4-6
%% Description:ttalk_request,this module the main purpose of packt
%% original information from client
-module(ltalk_request).
-export([handle/2]).
-include("ltalk_cmd.hrl").


handle(Socket,Line) when  Line#line.state == ?ERROR_CODE_CMD_FORMAT ->
	HandledLine = Line#line{state=?RESPONSE_CODE_ERROR,
					data=?INFO_FORMAT_ERROR ++ Line#line.cmd ++ Line#line.data};

handle(Socket,Line) when Line#line.state == ?ERROR_CODE_UNLOGIN ->
	HandledLine = Line#line{state=?RESPONSE_CODE_ERROR,
					data=?INFO_NOTIFY_LOGIN};	 

handle(Socket,Line) when Line#line.state == ?ERROR_CODE_UNREG ->
	HandledLine = Line#line{state=?RESPONSE_CODE_ERROR,
					data=?INFO_NOTIFY_REG};

handle(Socket,Line) when  Line#line.cmd == ?CMD_HELP  ->
	HandledLine = Line#line{state=?RESPONSE_CODE_OK,
					data=?INFO_NOTIFY_HELP};

handle(Socket,Line) when  Line#line.cmd == undefined  ->
	HandledLine = Line#line{state=?RESPONSE_CODE_OK,
					data=?INFO_NOTIFY_HELP}.

send(Socket,Line)  ->
	Bin = list_to_binary(Line#line.data),
	ltalk_socket:send(Socket, Bin).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

error_format_test() ->
	Line = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},
	HandledLine = #line{data="" ++ ?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line#line.data,
						state=?RESPONSE_CODE_ERROR,cmd=Line#line.cmd},
	?assertEqual(HandledLine,handle(sock,Line)).

help_test() ->
	Line = #line{cmd=?CMD_HELP , state=?CODE_OK},
	HandledLine = #line{data=?INFO_NOTIFY_HELP,
						state=?RESPONSE_CODE_OK,cmd=Line#line.cmd},
	?assertEqual(HandledLine,handle(sock,Line)).


error_login_test() ->
	Line = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_UNLOGIN},
	HandledLine = #line{data=?INFO_NOTIFY_LOGIN,
						state=?RESPONSE_CODE_ERROR,cmd=Line#line.cmd},
	?assertEqual(HandledLine,handle(sock,Line)).

error_reg_test() ->
	Line = #line{cmd=?CMD_REG , state=?ERROR_CODE_UNREG},
	HandledLine = #line{data=?INFO_NOTIFY_REG,
						state=?RESPONSE_CODE_ERROR,cmd=Line#line.cmd},
	?assertEqual(HandledLine,handle(sock,Line)).

oo_test() ->
	Mod = new_file:new("jias",1111),
	?assertEqual("jias",Mod:get(name)).

-endif.

