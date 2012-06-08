%% Author: wave
%% Created: 2012-6-8
%% Description: TODO: Add description to ltalk_util
-module(ltalk_util).

-export([
		 local_time_string/0
		]).

%% format local time as: 1987-11-11 12:25:21
local_time_string() ->
	 {{Year,Month,Day},{Hour,Min,Second}}=calendar:local_time(),
	 lists:concat([Year,"-",Month,"-",Day," ",Hour,":",Min,":",Second]).

time_to_string({Y0,H0,MS}) ->
	{{Y,M,D},{H,Mi,S}} = calendar:now_to_local_time({Y0,H0,MS}),
	MS0 = microseconds2columes(integer_to_list(MS div 1000)),
	[Y2,M2,D2,H2,Mi2,S2] = [string2columns(integer_to_list(X)) || X <- [Y,M,D,H,Mi,S]],
	lists:flatten(io_lib:format("~s-~s-~s ~s:~s:~s.~s", [Y2,M2,D2,H2,Mi2,S2,MS0]));
time_to_string(undefined) -> "";
time_to_string(X) -> X.

string2columns(X) ->
	case string:len(X) of
		1 -> "0" ++ X;
		_ -> X
	end.
	
microseconds2columes(X) ->
	case string:len(X) of
		1 ->
			"00" ++ X;
		2 ->
			"0" ++ X;
		_ -> 
			X
	end.
	


tstamp() ->
    {Mega, Sec, Micro} = now(),
    ((Mega * 1000000) + Sec)*1000 + Micro div 1000.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
