-module(bnow_boot).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, ensure_started/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ensure_started(sasl),
    ensure_started(os_mon),
    ensure_started(mnesia),
    ensure_started(ex_fcgi),
    ensure_started(inets),
    ensure_started(ssl),
    ssl:start(),
    ensure_started(cowboy),
    ensure_started(magic),
    
    ok = mnesia:wait_for_tables(bnow_setup:all_tables(), 30000),
    ensure_started(iplib),
    ok = bnow_modifier:init(),
    code:rehash(),
    io:format("bnow started\n"),
    bnow_sup:start_link().

stop(_State) ->
    bnow_rrd:gc(-1),
    mnesia:stop(),
    iplib:stop(),
    bnow_httpd:stop(),
    ok.

ensure_started(App)->
    case proplists:get_value(App, application:which_applications()) of
        undefined->
            application:start(App);
        _ -> ok
    end.
