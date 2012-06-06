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

help_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd=?CMD_HELP ,data=""},	
	Bin1 = list_to_binary(?INFO_NOTIFY_HELP),
	Req1 = ltalk_request:new(sock),
	?assertEqual(Bin1,Req1:handle(Line1)),

	meck:unload(ltalk_socket),
	ok.

cmd_format_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line1#line.data),
	Req1 = ltalk_request:new(sock),
	?assertEqual(Bin1,Req1:handle(Line1)),
	
	
	Line2 = #line{cmd=?CMD_HELP , state=?CODE_OK},
	Bin2 = list_to_binary(?INFO_NOTIFY_HELP),
	Req2 = ltalk_request:new(sock),
	?assertEqual(Bin2,Req2:handle(Line2)),
	

	meck:unload(ltalk_socket),
	ok.

unknown_cmd_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd="abcdefg" , state=?ERROR_CODE_CMD_UNKNOWN,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_UNKNOWN_CMD ++ "abcdefg" ),
	Req1 = ltalk_request:new(sock),
	?assertEqual(Bin1,Req1:handle(Line1)),
	
	meck:unload(ltalk_socket),
	ok.

registe_ok_and_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_onliner_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	
	
	Line = #line{cmd=?CMD_REG ,data="vivian"},
	Req = ltalk_request:new(sock),
	Req:put(line,Line),
	?assertEqual(<<"registe success,userid: vivian">>,Req:handle(Line)),
	
	Req1 = ltalk_request:new(sock),
	Req1:put(line,Line),
	?assertEqual(<<"registe failed,userid: vivian already exist">>,Req1:handle(Line)),	
	
	meck:unload(ltalk_socket),
	ok.

%% login_ok_and_error_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_onliner_server:start_link(),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	
%% 	%%this time userid not registed
%% 	Line = #line{cmd=?CMD_LOGIN ,data="vivian"},	
%% 	Req = ltalk_request:new(sock),
%% 	?assertEqual(<<"login failed,userid: vivian, please registe first!">>,Req:handle(Line)),
%% 	
%% 	%%this time test registe a new userid,and result success
%% 	Line1 = #line{cmd=?CMD_REG ,data="vivian"},
%% 	Req1 = ltalk_request:new(sock1),
%% 	?assertEqual(<<"registe success,userid: vivian">>,Req1:handle(Line1)),
%% 	
%% 	%%this time test login success with above registed userid
%% 	Line2 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
%% 	Req2 = ltalk_request:new(sock),
%% 	?assertEqual(<<"login success,userid: vivian">>,Req2:handle(Line2)),	
%% 	
%% 	%%this time test repeat login use same userid
%% 	Line3 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
%% 	Req3 = ltalk_request:new(sock),
%% 	?assertEqual(<<"login failed,userid: vivian already login">>,Req3:handle(Line3)),	
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.	
%% 	
%% query_state_error_and_ok_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line1 = #line{cmd=?CMD_QUERY_STATE ,data="vivian"},
%% 	Req1 = ltalk_request:new(sock,Line1),
%% 	?assertEqual(list_to_binary(?INFO_NOTIFY_LOGIN),Req1:handle()),
%% 	
%% 	ltalk_onliner_server:save(#onliner{name="vivian",socket=sock,talkto=all,groups=[],state=0}),
%% 	Req2 = ltalk_request:new(sock,Line1),
%% 	
%% 	?assertMatch("==="++_,binary_to_list(Req2:handle())),	
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.	
%% 
%% query_onliner_error_and_ok_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line1 = #line{cmd=?CMD_QUERY_STATE ,data="vivian"},
%% 	Req1 = ltalk_request:new(sock,Line1),
%% 	?assertEqual(list_to_binary(?INFO_NOTIFY_LOGIN),Req1:handle()),
%% 	
%% 	%%save three user as test data
%% 	ltalk_onliner_server:save(#onliner{name="vivian",socket=sock,talkto=all,groups=[],state=0}),
%% 	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto=all,groups=[],state=1}),
%% 	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto=all,groups=[],state=0}),
%% 	
%% 	Req2 = ltalk_request:new(sock,#line{cmd=?CMD_QUERY_ONLINERS,data="vivian"}),
%% 	
%% 	?assertMatch("==="++ _,binary_to_list(Req2:handle())),	
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.
%% 
%% talking_not_login_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line1 = #line{cmd='undefined' ,data="vivian"},
%% 	Req1 = ltalk_request:new(sock,Line1),
%% 	?assertEqual(list_to_binary(?INFO_NOTIFY_LOGIN),Req1:handle()),
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.
%% 
%% talking_to_one_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%save  users as test data
%% 	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto="tom",groups=[],state=1}),
%% 	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto=all,groups=[],state=0}),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line = #line{cmd='undefined' ,data="hello world"},
%% 	Req = ltalk_request:new(jsock,Line),
%% 	?assertEqual(list_to_binary("hello world"),Req:handle()),
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.
%% 
%% talking_to_noexist_men_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%save  users as test data
%% 	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto="tom",groups=[],state=1}),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line = #line{cmd='undefined' ,data="hello world"},
%% 	Req = ltalk_request:new(jsock,Line),
%% 	?assertEqual(list_to_binary("send error, userid: tom off line or not exist !"),Req:handle()),
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.
%% 
%% talking_to_all_test() ->
%% 	meck:new(ltalk_socket),
%% 	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
%% 	ltalk_db_server:start_link(),
%% 	ltalk_db_server:empty(?TAB_USER),
%% 	ltalk_onliner_server:delete(all),
%% 	ltalk_onliner_server:start_link(),
%% 	
%% 	%%save  users as test data
%% 	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto=?TALK_TO_ALL,groups=[],state=1}),
%% 	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto="abc",groups=[],state=0}),
%% 	
%% 	%%this time user vivian not login at fisrt
%% 	Line = #line{cmd='undefined' ,data="hello world"},
%% 	Req = ltalk_request:new(jsock,Line),
%% 	?assertEqual(1,length(Req:handle())),
%% 	
%% 	meck:unload(ltalk_socket),
%% 	ok.

-endif.