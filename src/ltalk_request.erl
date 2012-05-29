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
	response(Response_Data);

handle() when  Line#line.state == ?ERROR_CODE_CMD_UNKNOWN ->
	Response_Data=?INFO_UNKNOWN_CMD ++ Line#line.cmd,
	response(Response_Data);

handle() when  Line#line.cmd == ?CMD_HELP  ->
	Response_Data = ?INFO_NOTIFY_HELP,
	response(list_to_binary(Response_Data));

handle() when  Line#line.cmd == ?CMD_QUERY_STATE  ->
	response(do(query_state,is(logged)));

handle() when  Line#line.cmd == ?CMD_QUERY_ONLINERS  ->
	response(do(query_onliners,is(logged)));

handle() when  Line#line.cmd == ?CMD_REG  ->
	response(do(register,is(logged)));

handle() when  Line#line.cmd == ?CMD_LOGIN  ->
	response(do(login,is(logged)));

%% normal talk message,main handle
handle() when  Line#line.cmd == undefined  ->
	do(talk,is(logged)).

do(talk,Logged) when Logged == false ->
	response(?INFO_NOTIFY_LOGIN);

do(talk,Logged) when Logged ->
	{ok,#onliner{talkto=To}=My} = ltalk_onliner_server:get(socket, Socket),
	case To  of
		?TALK_TO_ALL -> %%not check whether user off or noexist in current.
			{ok,L} = ltalk_onliner_server:get(all),
			L1 = lists:delete(My, L),
			F = fun(E,L) ->
						send(E#onliner.socket,Line#line.data),
						[E|L]
				end,
			lists:foldl(F,[], L1);
		?TALK_TO_GROUP ->
			keep;
		One ->
			case ltalk_onliner_server:get(name, One)	of
				{error,not_found} ->
					response(lists:concat(["send error, userid: ",One," off line or not exist !"]));
				{ok,Receiver} ->
					send(Receiver#onliner.socket,Line#line.data)
			end
	end;

do(register,Logged) when Logged ->
	lists:concat(["registe failed,userid: ",Line#line.data," already exist"]);

do(login,Logged) when Logged ->			
	lists:concat(["login failed,userid: ",Line#line.data," already login"]);

do(query_onliners,Logged) when Logged==false ->
	?INFO_NOTIFY_LOGIN;

do(query_state,Logged) when Logged==false ->
	?INFO_NOTIFY_LOGIN;

do(query_onliners,Logged) when Logged ->
	{ok,L} = ltalk_onliner_server:get(all),
	F = fun(E,L) ->
			L1 = lists:concat(["\r\n id: ",E#onliner.name," state: ",E#onliner.state]),
			[L1|L]
		end,
	L2 = lists:foldl(F, [], L),
	lists:concat(["============onliners============",
				  L2,
				  "\r\n============onliners============"]);

do(query_state,Logged) when Logged ->
	{ok,Onliner} = ltalk_onliner_server:get(socket, Socket),
	lists:concat(["============state============",
				  "\r\n     id: ",Onliner#onliner.name,
				  "\r\n talkto: ",Onliner#onliner.talkto,
				  "\r\n groups: ",Onliner#onliner.groups,
				  "\r\n  state: ",Onliner#onliner.state,
				  "\r\n============state============"]);

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
	case ltalk_onliner_server:get(socket, Socket) of
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

send(S,Data) when is_list(Data) ->
	send(S,list_to_binary(Data));

send(S,Bin) ->
	ltalk_socket:send(Socket, Bin).


response(Data) when is_list(Data) ->
	response(list_to_binary(Data));
response(Bin)  ->
	ltalk_socket:send(Socket, Bin).



