%% Author: chao
%% Created: Apr 29, 2012
%% Description: TODO: Add description to ltalk_line
-module(ltalk_decode).

-export([getLine/1]).
-include("ltalk_cmd.hrl").


removeHeadSpace([]) ->
	[];
removeHeadSpace([H|T]) when H =:= $\s ->
	removeHeadSpace(T);
removeHeadSpace(L) ->
	L.

parseCmd([],Cmd,Data) ->
	Line = #line{cmd=lists:reverse(Cmd),data=Data},
	fliter_cmd(Line,get_cmd(Line#line.cmd));

parseCmd([H|T],Cmd,Data) when H =/= $\s ->
	parseCmd(T,[H|Cmd],Data);

parseCmd(RawLine,Cmd,Data) ->
	parseCmd([],Cmd,RawLine).

-spec getLine(RawLine::string()) ->term().
getLine(RawLine) ->
	parseLine(removeHeadSpace(RawLine),[],[]).


parseLine([],Cmd,Data) ->
	#line{};
parseLine([H|T],Cmd,Data) when H =:= [] ->
	#line{};
parseLine([H|T],Cmd,Data) when H =:= $- ->
	parseCmd(T,[],[]);
parseLine(TirmedLine,Cmd,Data) ->
	#line{data=TirmedLine}.


get_cmd(undefined) ->
	undefined;
get_cmd(?CMD_HELP) ->
	?CMD_HELP;
get_cmd(?CMD_REG) ->
	?CMD_REG;
get_cmd(?CMD_LOGIN) ->
	?CMD_LOGIN;
get_cmd(?CMD_HISTORY) ->
	?CMD_HISTORY;
get_cmd(?CMD_SWITCH) ->
	?CMD_SWITCH;
get_cmd(?CMD_QUERY_STATE) ->
	?CMD_QUERY_STATE;
get_cmd(?CMD_QUERY_ONLINERS) ->
	?CMD_QUERY_ONLINERS;
get_cmd(?CMD_ADD) ->
	?CMD_ADD;
get_cmd(?CMD_DEL) ->
	?CMD_DEL;
get_cmd(_Cmd) ->
	unknowncmd.

fliter_cmd(Line,Cmd) when Cmd == undefined ->
	Line;
fliter_cmd(Line,Cmd) when Cmd == unknowncmd ->
	#line{cmd='unknowncmd',data=("-"++Line#line.cmd++Line#line.data)};
fliter_cmd(Line,Cmd) ->
	NewLine = #line{cmd=Cmd,data=Line#line.data},
	check_cmd_format(NewLine).


check_cmd_format(#line{cmd=Cmd} = Line)
  		when Cmd == ?CMD_HELP orelse
			 Cmd == ?CMD_QUERY_STATE orelse
			 Cmd == ?CMD_QUERY_ONLINERS orelse
			 Cmd == ?CMD_QUIT ->
	format(Line,0);

check_cmd_format(#line{cmd=Cmd} = Line) 
  		when Cmd == ?CMD_REG orelse
			 Cmd == ?CMD_LOGIN orelse
			 Cmd == ?CMD_SWITCH orelse
			 Cmd == ?CMD_DEL ->
	format(Line,1);

check_cmd_format(#line{cmd=Cmd} = Line) 
  		when Cmd == ?CMD_HISTORY orelse
			 Cmd == ?CMD_ADD ->
	format(Line,2);

check_cmd_format(Line) ->
	Line.

%%Line data section must match ArgsNum
format(Line,ArgsNum) ->
	Tokens = string:tokens(Line#line.data," "),
	if
		length(Tokens) =/= ArgsNum ->
			#line{cmd=Line#line.cmd,data=Line#line.data,state=?ERROR_CODE_CMD_FORMAT};
		true ->
			#line{cmd=Line#line.cmd,data=Tokens}
	end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
removeHeadSpace_null_test() ->
	?assertEqual([],removeHeadSpace("")).

removeHeadSpace_test() ->
	?assertEqual("-help",removeHeadSpace("  -help")),
	?assertEqual("-login abc",removeHeadSpace("  -login abc")).

parseLine_null_test() ->
	?assertEqual(#line{},getLine("")).
parseLine_without_cmd_test() ->
	?assertEqual(#line{data="hello,world"},getLine("  hello,world")).
parseLine_with_cmd_test() ->
	?assertEqual(#line{cmd="login",data=["jias"]},getLine(" -login   jias")).

fliter_cmd_with_undefined_cmd_test() ->
	?assertEqual(#line{cmd='unknowncmd',data="-loginlogin   jias"},getLine(" -loginlogin   jias")).
fliter_cmd_with_cmd_flag_test() ->
	?assertEqual(#line{cmd='unknowncmd',data="- "},getLine(" - ")).

format_cmd_0_ok_test() ->
	?assertEqual(#line{cmd="help",data=[]},getLine(" -help ")).
format_cmd_1_ok_test() ->
	?assertEqual(#line{cmd="login",data=["jias"]},getLine("-login jias")).
format_cmd_2_ok_test() ->
	?assertEqual(#line{cmd="history",data=["2","3"]},getLine("-history 2 3")).
format_cmd_state_error_test() ->
	?assertEqual(#line{cmd="login",data=" jias chao",state=?ERROR_CODE_CMD_FORMAT},getLine("-login jias chao")).

-endif.
