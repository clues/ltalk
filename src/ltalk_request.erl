%% Author: wave
%% Created: 2012-4-6
%% Description:ttalk_request,this module the main purpose of packt
%% original information from client
-module(ltalk_request,[Socket,Line]).
-include("ltalk_cmd.hrl").
-export([
		 handle/0,
		 get/1
		]).


get(socket) ->
	Socket;

get(line) ->
	Line.

handle() when  Line#line.state == ?ERROR_CODE_CMD_FORMAT ->
	Response_Data=?INFO_FORMAT_ERROR ++ Line#line.cmd ++ Line#line.data,
	send(list_to_binary(Response_Data));

handle() when Line#line.state == ?ERROR_CODE_UNLOGIN ->
	Response_Data=?INFO_NOTIFY_LOGIN,
	send(list_to_binary(Response_Data));

handle() when Line#line.state == ?ERROR_CODE_UNREG ->
	Response_Data=?INFO_NOTIFY_REG,
	send(list_to_binary(Response_Data));

handle() when  Line#line.cmd == ?CMD_HELP  ->
	Response_Data = ?INFO_NOTIFY_HELP,
	send(list_to_binary(Response_Data));

handle() when  Line#line.cmd == ?CMD_QUERY_STATE  ->
	send(do(query_state,is(logged)));

handle() when  Line#line.cmd == ?CMD_REG  ->
	send(do(register,is(logged)));

handle() when  Line#line.cmd == ?CMD_LOGIN  ->
	send(do(login,is(logged)));

%% normal talk message,main handle
handle() when  Line#line.cmd == undefined  ->
	case ltalk_onliner_server:get(socket,Socket) of
		{error,not_found} ->
				Response_Data=?INFO_NOTIFY_LOGIN,
				send(list_to_binary(Response_Data));
		{ok,Onliner} ->
			{ok,Receiver} = ltalk_onliner_server:get(name,Onliner#onliner.talkto),
			send(Line#line.data)
	end.

do(register,Logged) when Logged ->
	lists:concat(["registe failed,userid: ",Line#line.data," already exist"]);

do(login,Logged) when Logged ->			
	lists:concat(["login failed,userid: ",Line#line.data," already login"]);

do(query_state,Logged) when Logged==false ->
	?INFO_NOTIFY_LOGIN;

do(query_state,Logged) when Logged ->
	?INFO_NOTIFY_LOGIN;

do(Type,Logged) when Logged==false ->			
	case is(registered) of
		true when 'login' == Type ->
			ltalk_onliner_server:save(#onliner{name=Line#line.data,socket=Socket}),
			lists:concat(["login success,userid: ",Line#line.data]);
		true ->
			lists:concat(["registe failed,userid: ",Line#line.data," already exist"]);
		false when 'login' == Type ->
			lists:concat(["login failed,userid: ",Line#line.data,", ",?INFO_NOTIFY_REG]);
		false ->
			ltalk_db_server:save(?TAB_USER, #user{name=Line#line.data}),
			lists:concat(["registe success,userid: ",Line#line.data])
	end.

is(logged) ->
	Name = Line#line.data,
	case ltalk_onliner_server:get(name, Name) of
		{error,not_found} ->
			false;
		{ok,R} ->
			true
	end;

is(registered) ->
	Name = Line#line.data,
	case ltalk_db_server:get(?TAB_USER, Name) of
		{error,read_failed} ->
			false;
		{ok,[]} ->
			false;
		{ok,[_]} ->
			true
	end.

send(Data) when is_list(Data) ->
	send(list_to_binary(Data));
send(Bin)  ->
	ltalk_socket:send(Socket, Bin).



