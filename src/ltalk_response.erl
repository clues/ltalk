%% Author: wave
%% Created: 2012-4-11
%% Description: TODO: Add description to ttalk_response
-module(ltalk_response).
-include("ltalk_cmd.hrl").
-export([]).


-define(INFO_NOTIFY_REG,"please registe first").
-define(INFO_NOTIFY_LOGIN,"please login first").
-define(INFO_FORMAT_ERROR,"wrong command format -").
-define(INFO_NOTIFY_HELP,"====wlecome======"
					   ++"you can use follow command "
	   				   ++"for interacte with other "
	                   ++"-help  #get all command "
	                   ++"-reg id  #registe you infomation "
	                   ++"-login id #login in system ").

-spec endcode(Info::string()) -> binary().
endcode(Info) ->
	list_to_binary(Info).


endcode_line(Line) when Line#line.state == ?ERROR_CODE_CMD_FORMAT ->
	Info = ?INFO_FORMAT_ERROR ++ Line#line.cmd ++ Line#line.data,
	endcode(Info);
endcode_line(Line) when Line#line.state == ?CODE_HELP ->
	endcode(?INFO_NOTIFY_HELP);
endcode_line(Line) when Line#line.state == ?ERROR_CODE_UNLOGIN ->
	endcode(?INFO_NOTIFY_LOGIN);
endcode_line(Line) when Line#line.state == ?ERROR_CODE_UNREG ->
	endcode(?INFO_NOTIFY_REG);
endcode_line(Line) ->
	endcode(Line#line.data).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
	
	endcode_line_help_test() ->
		Line = #line{cmd=?CMD_HELP,state=?CODE_HELP},
		Bin = list_to_binary(?INFO_NOTIFY_HELP),
		?assertEqual(Bin,endcode_line(Line)).

-endif.

