


-define(CMD_HELP,"help").%%-help
-define(CMD_REG,"reg").%%-reg
-define(CMD_LOGIN,"login").%%-login
-define(CMD_HISTORY,"history").%%-history
-define(CMD_QUIT,"quit").%%-quit

-define(CODE_OK,200).
-define(CODE_HELP,201).

-define(ERROR_CODE_CMD_FORMAT,400).
-define(ERROR_CODE_UNREG,401).
-define(ERROR_CODE_UNLOGIN,402).
-define(ERROR_CODE_NET_CONNECT,410).

-record(line,{cmd=undefined,data=[],state=0}).
-record(res,{code=200,data = <<>>}).