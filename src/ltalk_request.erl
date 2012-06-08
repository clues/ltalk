%% Author: wave
%% Created: 2012-4-6
%% Description:ttalk_request,this module the main purpose of packt
%% original information from client
-module(ltalk_request,[Socket]).
-include("ltalk_cmd.hrl").
-export([
		 handle/1,
		 get/1,
		 response/1
		]).

-ifdef(TEST).
-compile(export_all).

put(line,Line) ->
	erlang:put(line, Line);

put(id,Name) ->
	erlang:put(id, Name).

-endif.

get(socket) ->
	Socket;
get(line) ->
	erlang:get(line);
get(id) ->
	erlang:get(id).

handle(Packet) when is_list(Packet),length(Packet) ==0 ->
	response(<<>>);

handle(Packet) when is_list(Packet) ->
	Line = ltalk_decode:getLine(Packet),
	erlang:put(line,Line),
	handle(Line);			

handle(Line) when  Line#line.state == ?ERROR_CODE_CMD_FORMAT ->
	Response_Data=?INFO_FORMAT_ERROR ++ Line#line.cmd ++ Line#line.data,
	response(Response_Data);

handle(Line) when  Line#line.state == ?ERROR_CODE_CMD_UNKNOWN ->
	Response_Data=?INFO_UNKNOWN_CMD ++ Line#line.cmd,
	response(Response_Data);

handle(Line) when  Line#line.cmd == ?CMD_HELP  ->
	Response_Data = ?INFO_NOTIFY_HELP,
	response(Response_Data);

handle(Line) when  Line#line.cmd == ?CMD_QUERY_STATE  ->
	response(do(query_state,is(logged)));

handle(Line) when  Line#line.cmd == ?CMD_QUERY_ONLINERS  ->
	response(do(query_onliners,is(logged)));

handle(Line) when  Line#line.cmd == ?CMD_REG  ->
	response(do(register,is(logged)));

handle(Line) when  Line#line.cmd == ?CMD_LOGIN  ->
	response(do(login,is(logged)));

handle(Line) when  Line#line.cmd == ?CMD_QUIT  ->
	response(do(quit,is(logged)));

%% normal talk message,main handle
handle(Line) when  Line#line.cmd == 'undefined'  ->
	do(talk,is(logged)).

do(quit,Logged) when Logged == false ->
	exit(normal);

do(quit,Logged) when Logged ->
	{ok,#onliner{name=Name}} = ltalk_onliner_server:get(socket, Socket),
	ltalk_onliner_server:delete(Name),
	exit(normal);

do(talk,Logged) when Logged == false ->
	response(?INFO_NOTIFY_LOGIN);

do(talk,Logged) when Logged ->
	{ok,#onliner{talkto=To}=My} = ltalk_onliner_server:get(socket, Socket),
	Line = erlang:get(line),
	case To  of
		?TALK_TO_ALL -> %%not check whether user off or noexist in current.
			{ok,L} = ltalk_onliner_server:get(all),
			L1 = lists:delete(My, L),
			F = fun(E,L) ->
						send(E#onliner.socket,Line#line.data),
						[E|L]
				end,
			lists:foldl(F,[], L1),
			response("");
		?TALK_TO_GROUP ->
			response("waitting...");
		One ->
			case ltalk_onliner_server:get(name, One)	of
				{error,not_found} ->
					response(lists:concat(["send failed, user: ",One," not online!"]));
				{ok,Receiver} ->
					case send(Receiver#onliner.socket,Line#line.data) of
						ok ->
							response("");
						{error,Reason} ->
							response(lists:concat(["send failed,socket excpetion: ",Reason]))
					end
			end
	end;

do(register,Logged) when Logged ->
	Line = erlang:get(line),
	lists:concat(["registe failed,userid: ",Line#line.data," already exist"]);

do(login,Logged) when Logged ->
	Line = erlang:get(line),
	lists:concat(["login failed,userid: ",Line#line.data," already login"]);

do(query_onliners,Logged) when Logged==false ->
	?INFO_NOTIFY_LOGIN;

do(query_state,Logged) when Logged==false ->
	?INFO_NOTIFY_LOGIN;

do(query_onliners,Logged) when Logged ->
	{ok,L} = ltalk_onliner_server:get(all),
	F = fun(E,L) ->
			L1 = lists:concat(["   id: ",E#onliner.name," state: ",E#onliner.state,?END_TAG_L]),
			[L1|L]
		end,
	L2 = lists:foldl(F, [], L),
	lists:concat(["============onliners============",?END_TAG_L,
				  L2,
				  "============onliners============",?END_TAG_L]);

do(query_state,Logged) when Logged ->
	{ok,Onliner} = ltalk_onliner_server:get(socket, Socket),
	lists:concat(["============state============",?END_TAG_L,
				  "   id:     ",Onliner#onliner.name,?END_TAG_L,
				  "   talkto: ",Onliner#onliner.talkto,?END_TAG_L,
				  "   groups: ",Onliner#onliner.groups,?END_TAG_L,
				  "   state:  ",Onliner#onliner.state,?END_TAG_L,
				  "============state============",?END_TAG_L]);

do(Type,Logged) when Logged==false ->			
	Line = erlang:get(line),
	case is(registered) of
		true when 'login' == Type ->
			ltalk_onliner_server:save(#onliner{name=Line#line.data,socket=Socket}),
			erlang:put(id, Line#line.data),
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
	Line = erlang:get(line),
	Name = Line#line.data,
	case ltalk_db_server:get(?TAB_USER, Name) of
		{error,read_failed} ->
			false;
		{ok,[]} ->
			false;
		{ok,[_]} ->
			true
	end.


%%b->s->b mode return ok|{error,Reason}
send(Remote,Data) when is_list(Data) ->
	Title = lists:concat(["\r\n",erlang:get(id)," ",ltalk_util:local_time_string(),?END_TAG_L]),
	send(Remote,list_to_binary(Title++Data ++ ?END_TAG_L ++ ?PROMPT_TAG));

send(Remote,Bin) ->
	ltalk_socket:send(Remote, Bin).

%%b->s mode return ok|{error,Reason}
response("") ->
	response(list_to_binary(?PROMPT_TAG));

response(Data) when is_list(Data) ->
	response(list_to_binary(Data++?END_TAG_L++?PROMPT_TAG));

response(Bin)  ->
	ltalk_socket:send(Socket, Bin).



