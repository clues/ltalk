


-define(CMD_HELP,"help").%%-help
-define(CMD_REG,"reg").%%-reg
-define(CMD_LOGIN,"login").%%-login
-define(CMD_HISTORY,"history").%%-history
-define(CMD_QUIT,"quit").%%-quit

-define(CODE_OK,200).

-define(ERROR_CODE_CMD_FORMAT,400).
-define(ERROR_CODE_UNREG,401).
-define(ERROR_CODE_UNLOGIN,402).
-define(ERROR_CODE_NET_CONNECT,410).

-define(RESPONSE_CODE_OK,200).
-define(RESPONSE_CODE_ERROR,400).
-define(RESPONSE_CODE_FORWARD,300).

-record(line,{cmd=undefined,data=[],state=0}).
-record(res,{code=200,data = <<>>}).

-record(onliner,{socket,name,state,talkto}).
-record(user,{name,create_time,state,last_access_time}).
-record(chat,{name,time,data}).



-define(INFO_NOTIFY_REG,"please registe first").
-define(INFO_NOTIFY_LOGIN,"please login first").
-define(INFO_FORMAT_ERROR,"wrong command format -").
-define(INFO_NOTIFY_HELP,"====wlecome======"
					   ++"you can use follow command "
	   				   ++"for interacte with other "
	                   ++"-help  #get all command "
	                   ++"-reg id  #registe you infomation "
	                   ++"-login id #login in system ").