


-define(CMD_HELP,"help").%%-help           show all command details 
-define(CMD_REG,"reg").%%-reg              register someone,useid as unique
-define(CMD_LOGIN,"login").%%-login        forbid login repeat at the same time
-define(CMD_SWITCH,"switch").%%-switch     switch to a group,someone or all user
-define(CMD_HISTORY,"history").%%-history  query self chat records
-define(CMD_QUERY_ONLINERS,"ql").%%-ql     query onliners list in current
-define(CMD_QUERY_STATE,"qs").%%-qs        query self state in current
-define(CMD_ADD,"add").%%-add              add someone to group
-define(CMD_DEL,"del").%%-del              delete someone from group
-define(CMD_QUIT,"quit").%%-quit           quit ltalk


-define(CODE_OK,200).

-define(ERROR_CODE_CMD_FORMAT,400).
-define(ERROR_CODE_CMD_UNKNOWN,404).
-define(ERROR_CODE_UNREG,401).
-define(ERROR_CODE_UNLOGIN,402).
-define(ERROR_CODE_NET_CONNECT,410).
-define(ERROR_CODE_INFO_TOO_LARGE,411). %%received message length too large
-define(ERROR_CODE_INFO_INCOMPLETE,411). %%received message incompelete,lose end tag

-define(RESPONSE_CODE_OK,200).
-define(RESPONSE_CODE_ERROR,400).
-define(RESPONSE_CODE_FORWARD,300).

-define(TAB_USER,user).
-define(TAB_CHAT,chat).

-define(PROMPT_TAG,"ltalk>").
-define(END_TAG_L,"\r\n"). %%info end tag list "\r\n" 
-define(END_TAG_B,<<13,10>>). %%info end tag binary (\r\n)  
-define(MAX_LEN_INFO,256).  %%max length of info receive form client

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
-define(INFO_LEN_NOT_MEET,"message size not meet requirement,server refuse!").
-define(INFO_INCOMPLETE,"incomplete message,server refuse!").
-define(INFO_FORMAT_ERROR,"wrong command format -").
-define(INFO_UNKNOWN_CMD,"unknown command: -").
-define(INFO_WELCOME,"====welcome======\r\n" ++
		             "      ltalk      \r\n" ++
                     "====welcome======\r\n").
-define(INFO_NOTIFY_HELP,"=================command detail================\r\n" ++
			             "   -help       #get all command\r\n" ++
			             "   -reg id     #registe you infomation\r\n"++
			             "   -login id   #login in system\r\n" ++
			             "   -switch who #switch chat to one,group or all\r\n"++
			             "   -qs         #query your current state\r\n"++
			             "   -ql         #query onliners list at current\r\n"++
			             "   -add id     #add some one to group\r\n"++
			             "   -del id     #delete some one from group\r\n"++
			             "   -history    #query your history chat records\r\n").