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
	Response_Data = ?INFO_NOTIFY_HELP,
	send(list_to_binary(Response_Data));

handle() when  Line#line.cmd == ?CMD_REG  ->
	R = case do_reg() of
		{error,Reason} ->
			lists:concat(["registe failed,userid: ",Line#line.data," ",Reason]);
		ok ->
			lists:concat(["registe success,userid: ",Line#line.data])
	end,
	send(list_to_binary(R));

handle() when  Line#line.cmd == ?CMD_LOGIN  ->
	R = case do_login() of
		{error,Reason} ->
			lists:concat(["login failed,userid: ",Line#line.data," ",Reason]);
		ok ->
			lists:concat(["login success,userid: ",Line#line.data])
	end,
	send(list_to_binary(R));

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

do_reg() ->
	Name = Line#line.data,
	case ltalk_onliner_server:get(name, Name) of
		{error,not_found} ->
			case ltalk_db_server:get(?TAB_USER, Name) of
				{error,read_failed} = R->
					R;
				{ok,[]} ->
					ltalk_db_server:save(?TAB_USER, #user{name=Name}),
					ok;
				{ok,[_]} ->
					{error,already_exist}				
			end;
		{ok,R} ->
			{error,already_exist}
	end.

do_login() ->
	Name = Line#line.data,
	case ltalk_onliner_server:get(name, Name) of
		{error,not_found} ->
			case ltalk_db_server:get(?TAB_USER, Name) of
				{error,read_failed} = R->
					R;
				{ok,[]} ->
					{error,not_found};
				{ok,[_]} ->
					ltalk_onliner_server:save(#onliner{name=Name,socket=Socket}),
					ok				
			end;
		{ok,R} ->
			{error,already_login}
	end.
  
send(Bin)  ->
	ltalk_socket:send(Socket, Bin).



