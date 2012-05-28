%% Author: wave
%% Created: 2012-4-6
%% Description:ttalk_request,this module the main purpose of packt
%% original information from client
-module(ltalk_request).
-export([handle/2]).
-include("ltalk_cmd.hrl").


handle(Socket,Line) when  Line#line.state == ?ERROR_CODE_CMD_FORMAT ->
	Response_Data=?INFO_FORMAT_ERROR ++ Line#line.cmd ++ Line#line.data,
	response(Socket,list_to_binary(Response_Data));

handle(Socket,Line) when Line#line.state == ?ERROR_CODE_UNLOGIN ->
	Response_Data=?INFO_NOTIFY_LOGIN,
	response(Socket,list_to_binary(Response_Data));

handle(Socket,Line) when Line#line.state == ?ERROR_CODE_UNREG ->
	Response_Data=?INFO_NOTIFY_REG,
	response(Socket,list_to_binary(Response_Data));

handle(Socket,Line) when  Line#line.cmd == ?CMD_HELP  ->
	Response_Data = ?INFO_NOTIFY_HELP,
	response(Socket,list_to_binary(Response_Data));

%% normal talk message,main handle
handle(Socket,Line) when  Line#line.cmd == undefined  ->
	case ltalk_onliner_server:get(Socket) of
		{error,not_found} ->
				Response_Data=?INFO_NOTIFY_LOGIN,
				response(Socket,list_to_binary(Response_Data));
		Onliner ->
			Receiver = ltalk_onliner_server:get_by_name(Onliner#onliner.talkto),
			response(Receiver,Line#line.data)
	end.

response(Socket,Bin)  ->
	ltalk_socket:send(Socket, Bin).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

response_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->
										  Bin
							end),
	
	Line1 = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line1#line.data),
	
	Line2 = #line{cmd=?CMD_HELP , state=?CODE_OK},
	Bin2 = list_to_binary(?INFO_NOTIFY_HELP),
	
	Line3 = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_UNLOGIN},
	Bin3 = list_to_binary(?INFO_NOTIFY_LOGIN),

	Line4 = #line{cmd=?CMD_REG , state=?ERROR_CODE_UNREG},
	Bin4 = list_to_binary(?INFO_NOTIFY_REG),
	
	?assertEqual(Bin1,handle(sock,Line1)),
	?assertEqual(Bin2,handle(sock,Line2)),
	?assertEqual(Bin3,handle(sock,Line3)),
	?assertEqual(Bin4,handle(sock,Line4)),

	meck:unload(ltalk_socket),
	ok.

-endif.

