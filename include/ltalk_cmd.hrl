


-define(CMD_HELP,"help").%%-help
-define(CMD_REG,"reg").%%-reg
-define(CMD_LOGIN,"login").%%-login
-define(CMD_SWITCH,"switch").%%-switch
-define(CMD_HISTORY,"history").%%-history
-define(CMD_QUERY_ONLINERS,"ql").%%-ql
-define(CMD_QUERY_STATE,"qs").%%-qs
-define(CMD_ADD,"add").%%-add
-define(CMD_DEL,"del").%%-del
-define(CMD_QUIT,"quit").%%-quit


-define(CODE_OK,200).

-define(ERROR_CODE_CMD_FORMAT,400).
-define(ERROR_CODE_CMD_UNKNOWN,404).
-define(ERROR_CODE_UNREG,401).
-define(ERROR_CODE_UNLOGIN,402).
-define(ERROR_CODE_NET_CONNECT,410).

-define(RESPONSE_CODE_OK,200).
-define(RESPONSE_CODE_ERROR,400).
-define(RESPONSE_CODE_FORWARD,300).

-define(TAB_USER,user).
-define(TAB_CHAT,chat).

-define(TALK_TO_ALL,0).
-define(TALK_TO_ONE,1).
-define(TALK_TO_GROUP,2).

-record(line,{cmd=undefined,data=[],state=0}).
-record(res,{code=200,data = <<>>}).

-record(onliner,{name,socket,talkto=?TALK_TO_ALL,groups=[],state}).
-record(user,{name,groups=[],create_time,state,last_access_time}).
-record(chat,{owner,talkto,time,data}).


-define(INFO_NOTIFY_REG,"please registe first!").
-define(INFO_NOTIFY_LOGIN,"please login first!").
-define(INFO_FORMAT_ERROR,"wrong command format -").
-define(INFO_UNKNOWN_CMD,"unknown command  -").
-define(INFO_WELCOME,"====welcome======\r\n"++
										"               ltalk                  \r\n"++
										"====welcome======").
-define(INFO_NOTIFY_HELP,"====welcome======"
					   ++"you can use follow command "
	   				   ++"for interacte with other "
	                   ++"-help  #get all command "
	                   ++"-reg id  #registe you infomation "
	                   ++"-login id #login in system ").