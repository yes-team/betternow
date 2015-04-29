-module(bnow).
-export([start/0, stop/0]).
-include("defined.hrl").
-include("vsn.hrl").
-compile(export_all).

% bnow:add_filter(abc, f1, http, [{method,"GET"}]).
% bnow:add_action(abc, a1, update, {update_args, "goods-${goods_id}", "25*goods_id"}).
% bnow:active(abc, f1, a1).

start()->
    application:start(?MODULE).

stop()->
%    bnow_server:close(),
%    bnow_rrd:sync(),
    application:stop(?MODULE).

app_list()-> [].
app_load(_Name)-> ok.
app_unload(_Name)-> ok.
app_dir()->
    {ok, Appdir} = application:get_env(bnow, appdir),
    Appdir.

version()-> ?bnow_vsn.
licence()-> {free_user}.

debug(Title, Data)->
    message(<<"debug">>, Title, Data).

info(Title, Data)->
    message(<<"info">>, Title, Data).

message(Channel, Title, D0) when is_list(D0)->
    Trace = try throw(yes) catch yes -> erlang:get_stacktrace() end,
    {_,_,_,Opts} = lists:nth(2, Trace),
    {_, D1} = lists:foldl(fun(Item, {N,D})->
            case Item of
            {K, V} -> {N, [{K, V} | D]};
            V -> {N+1, [{N, V} | D]}
            end
    end, {0, Opts}, D0),
    D2 = [{<<"@title">>, bnow_var:bin( io_lib:format("~p ~s", [self(), Title]))} | D1],
    bnow_flow:handle_data(Channel, D2).

getconf_php(App, Key)->
    case mnesia:dirty_read(bnow_appconf, {App, Key}) of
        [R|_] -> R#bnow_appconf.phpval;
        _ -> false
    end.

getconf(App, Key)->
    case mnesia:dirty_read(bnow_appconf, {App, Key}) of
        [R|_] -> R#bnow_appconf.value;
        _ -> false
    end.

setconf(App, Key, Value, PhpVal)->
    {atomic, ok} = mnesia:transaction(fun()->
             mnesia:write(#bnow_appconf{
                     id = {App, Key},
                     app = App,
                     key = Key,
                     value = Value,
                     phpval = PhpVal
                 })
        end),
    ok.

listconf(App)->
    {atomic, L} = mnesia:transaction(fun()->
            mnesia:index_read(bnow_appconf, App, #bnow_appconf.app)
        end),
    L2 = [{X#bnow_appconf.key, X#bnow_appconf.value} || X<-L],
    {ok, L2}.







