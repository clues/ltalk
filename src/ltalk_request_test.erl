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
	Req1 = ltalk_request:new(sock),
	?assertMatch(?INFO_NOTIFY_HELP++_,binary_to_list(Req1:handle(Line1))),

	meck:unload(ltalk_socket),
	ok.

cmd_format_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd=?CMD_LOGIN , state=?ERROR_CODE_CMD_FORMAT,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_FORMAT_ERROR ++ ?CMD_LOGIN ++ Line1#line.data++?END_TAG_L++?PROMPT_TAG),
	Req1 = ltalk_request:new(sock),
	?assertEqual(Bin1,Req1:handle(Line1)),
	
	
	Line2 = #line{cmd=?CMD_HELP , state=?CODE_OK},
	Bin2 = list_to_binary(?INFO_NOTIFY_HELP++?END_TAG_L++?PROMPT_TAG),
	Req2 = ltalk_request:new(sock),
	?assertEqual(Bin2,Req2:handle(Line2)),
	

	meck:unload(ltalk_socket),
	ok.

unknown_cmd_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	
	Line1 = #line{cmd="abcdefg" , state=?ERROR_CODE_CMD_UNKNOWN,data=" jias jias"},	
	Bin1 = list_to_binary(?INFO_UNKNOWN_CMD ++ "abcdefg" ++?END_TAG_L++?PROMPT_TAG),
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
	<<"registe success,userid: vivian",_/binary>> = Req:handle(Line),
	
	Req1 = ltalk_request:new(sock),
	Req1:put(line,Line),
	<<"registe failed,userid: vivian already exist",_/binary>> =Req1:handle(Line),	
	
	meck:unload(ltalk_socket),
	ok.

login_ok_and_error_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_onliner_server:start_link(),
	ltalk_onliner_server:delete(all),
	ltalk_db_server:empty(?TAB_USER),
	
	%%this time userid not registed
	Line = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req = ltalk_request:new(sock),
	<<"login failed,userid: vivian, please registe first!",_/binary>>=Req:handle(Line),
	
	%%this time test registe a new userid,and result success
	Line1 = #line{cmd=?CMD_REG ,data="vivian"},
	Req1 = ltalk_request:new(sock1),
	<<"registe success,userid: vivian",_/binary>>=Req1:handle(Line1),
	
	%%this time test login success with above registed userid
	Line2 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req2 = ltalk_request:new(sock),
	<<"login success,userid: vivian",_/binary>>= Req2:handle(Line2),	
	
	%%this time test repeat login use same userid
	Line3 = #line{cmd=?CMD_LOGIN ,data="vivian"},	
	Req3 = ltalk_request:new(sock),
	<<"login failed,userid: vivian already login",_/binary>>=Req3:handle(Line3),	
	
	meck:unload(ltalk_socket),
	ok.	
	
query_state_error_and_ok_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%this time user vivian not login at fisrt
	Line1 = #line{cmd=?CMD_QUERY_STATE ,data="vivian"},
	Req1 = ltalk_request:new(sock),
	<<?INFO_NOTIFY_LOGIN,_/binary>> = Req1:handle(Line1),
	
	ltalk_onliner_server:save(#onliner{name="vivian",socket=sock,talkto=all,groups=[],state=0}),
	Req2 = ltalk_request:new(sock),
	
	<<"============state============",_/binary>> = Req2:handle(Line1),	
	
	meck:unload(ltalk_socket),
	ok.	

query_onliner_error_and_ok_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%this time user vivian not login at fisrt
	Line1 = #line{cmd=?CMD_QUERY_STATE ,data="vivian"},
	Req1 = ltalk_request:new(sock),
	<<?INFO_NOTIFY_LOGIN,_/binary>> = Req1:handle(Line1),
	
	%%save three user as test data
	ltalk_onliner_server:save(#onliner{name="vivian",socket=sock,talkto=all,groups=[],state=0}),
	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto=all,groups=[],state=1}),
	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto=all,groups=[],state=0}),
	
	Line2 = #line{cmd=?CMD_QUERY_ONLINERS,data="vivian"},
	Req2 = ltalk_request:new(sock),
	
	<<"============onliners============",_/binary>> = Req2:handle(Line2),	
	
	meck:unload(ltalk_socket),
	ok.

talking_not_login_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%this time user vivian not login at fisrt
	Line1 = #line{cmd='undefined' ,data="vivian"},
	Req1 = ltalk_request:new(sock),
	<<?INFO_NOTIFY_LOGIN,_/binary>> = Req1:handle(Line1),
	
	meck:unload(ltalk_socket),
	ok.

quit_test() ->
	
	%%take self process as socket server
	process_flag(trap_exit,true),
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%this time user vivian not login at fisrt
	Line1 = #line{cmd=?CMD_QUIT ,data=""},
	spawn_link(fun() ->
					   Req1 = ltalk_request:new(sock),
					   Req1:handle(Line1)
			   end),
	receive
		{'EXIT',Pid,normal} ->
			ok
	after 1000 ->
			exit(timeout)
	end,
	meck:unload(ltalk_socket),
	ok.	

talking_to_one_not_online_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)->Bin end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%save  users as test data
	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto="tom",groups=[],state=1}),
	
	
	Line = #line{cmd='undefined' ,data="hello world"},
	Req = ltalk_request:new(jsock),
	<<"send failed, user: tom not online!",_/binary>> = Req:handle(Line),
	
	meck:unload(ltalk_socket),
	ok.

talking_to_one_socket_excpetion_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(S,Bin)-> case S of
												   tsock ->
													   {error,nosocket};
												   jsock ->
													   Bin
											   end
								 end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%save  users as test data
	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto="tom",groups=[],state=1}),
	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto=all,groups=[],state=0}),
	
	%%this time user vivian not login at fisrt
	Line = #line{cmd='undefined' ,data="hello world"},
	Req = ltalk_request:new(jsock),
	Req:put(id,"jack"),
	
	<<"send failed,socket excpetion: ",_/binary>> = Req:handle(Line),
	
	meck:unload(ltalk_socket),
	ok.

talking_to_one_ok_test() ->
	meck:new(ltalk_socket),
	meck:expect(ltalk_socket,send,fun(_S,Bin)-> case _S of
													tsock ->
														ok;
													jsock ->
														Bin
												end
				end),
	ltalk_db_server:start_link(),
	ltalk_db_server:empty(?TAB_USER),
	ltalk_onliner_server:delete(all),
	ltalk_onliner_server:start_link(),
	
	%%save  users as test data
	ltalk_onliner_server:save(#onliner{name="jack",socket=jsock,talkto="tom",groups=[],state=1}),
	ltalk_onliner_server:save(#onliner{name="tom",socket=tsock,talkto=all,groups=[],state=0}),
	
	%%this time user vivian not login at fisrt
	Line = #line{cmd='undefined' ,data="hello world"},
	Req = ltalk_request:new(jsock),
	Req:put(line,Line),
	Req:put(id,"jack"),
	
	<<?PROMPT_TAG>> = Req:handle(Line),
	
	meck:unload(ltalk_socket),
	ok.

-endif.