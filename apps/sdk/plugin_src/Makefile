REBAR := ../../../bnow/rebar

.PHONY: all deps

all: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps
	
clean:
	$(REBAR) clean
