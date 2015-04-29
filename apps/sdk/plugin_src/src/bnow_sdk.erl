-module(bnow_sdk).
-include("../../../bnow/include/defined.hrl").

-export([init/1, handle_event/3, terminate/2, code_change/3,
        handle_data/1, app_cmd/2]).
-behaviour(bnow_plugin).
-record(state, {}).

init(_Opts)->
    {ok, #state{}}.

handle_event(_Data, _Action, _State)->
    {noreply, _State}.
    
terminate(_Reason, State) ->
    bifrost:stop(bnow_dev_ftp),
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

handle_data(Data)->
    bnow_server:handle_data(bnow_record:from_list(Data)),
    ok.

app_cmd(App, Cmd)->
    A = list_to_atom(binary_to_list(App)),
    R = case list_to_atom(binary_to_list(Cmd)) of
        start-> bnow_app:install(A);
        stop-> bnow_app:uninstall(A);
        update-> bnow_app:update(A)
    end,
    case R of
        {atomic ,ok} -> ok;
        {ok, _ } -> ok;
        _-> error
    end.
