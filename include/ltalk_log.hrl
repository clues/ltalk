

-define(FATAL,'fatal').
-define(ERROR,'error').
-define(WARN,'warn').
-define(INFO,'info').
-define(DEBUG,'debug').

-define(VALUE_FATAL,0).
-define(VALUE_ERROR,1).
-define(VALUE_WARN,2).
-define(VALUE_INFO,3).
-define(VALUE_DEBUG,4).

-define(TYPE_ALL,all).
-define(TYPE_CONSOLE,console).
-define(TYPE_FILE,file).

-define(DEFAULT_FORMATTER,null).
-define(FORMATTER,null).

-record(filelogger,{level=?WARN,dir=".",file="smplog.txt",size=2048,rotation=5,format}).
-record(consolelogger,{level=?DEBUG,format}).


-ifdef(TEST).
-define(PRINT(Fmt,Msg),error_logger:info_msg(Fmt, Msg)).
-else.
-define(PRINT(Fmt,Msg),io:format(Fmt, Msg)).
-endif.