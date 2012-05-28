PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile
	
compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc

etest:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit
	
clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

get-deps:
	@$(REBAR) get-deps	

app:
	@$(REBAR) create template=application dest=$(DEST) appid=$(PROJECT)
	