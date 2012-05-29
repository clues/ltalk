%% Author: wave
%% Created: 2012-5-29
%% Description: TODO: Add description to ltalk_request_test
-module(ltalk_request_test).
-include("ltalk_cmd.hrl").
-export([]).



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

response_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line1#line.data),
	Req1 = ltalk_request:new(sock,Line1),
	?assertEqual(Bin1,Req1:handle()),
	
	
	Line2 = #line{cmd=?CMD_HELP , state=?CODE_OK},
	Bin2 = list_to_binary(?INFO_NOTIFY_HELP),
	Req2 = ltalk_request:new(sock,Line2),
	?assertEqual(Bin2,Req2:handle()),
	

	meck:unload(ltalk_socket),
	ok.

registe_ok_and_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:start_link(),
	
	
	Line = #line{cmd=?CMD_REG ,data="vivian"},
	Req = ltalk_request:new(sock,Line),
	?assertEqual(<<"registe success,userid: vivian">>,Req:handle()),
	
	Req1 = ltalk_request:new(sock,Line),
	?assertEqual(<<"registe failed,userid: vivian already exist">>,Req1:handle()),	
	
	meck:unload(ltalk_socket),
	ok.

login_ok_and_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:start_link(),
	
	%%this time userid not registed
	Line = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req = ltalk_request:new(sock,Line),
	?assertEqual(<<"login failed,userid: vivian, please registe first!">>,Req:handle()),
	
	%%this time test registe a new userid,and result success
	Line1 = #line{cmd=?CMD_REG ,data="vivian"},
	Req1 = ltalk_request:new(sock1,Line1),
	?assertEqual(<<"registe success,userid: vivian">>,Req1:handle()),
	
	%%this time test login success with above registed userid
	Line2 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req2 = ltalk_request:new(sock,Line2),
	?assertEqual(<<"login success,userid: vivian">>,Req2:handle()),	
	
	%%this time test repeat login use same userid
	Line3 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req3 = ltalk_request:new(sock,Line3),
	?assertEqual(<<"login failed,userid: vivian already login">>,Req3:handle()),	
	
	meck:unload(ltalk_socket),
	ok.	
	

-endif.