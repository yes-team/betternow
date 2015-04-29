
-module(bnow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([cast_listener/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARG(Id, Module, Args, Type),
        {Id, {Module, start_link, [Args]}, permanent, 50000, Type, [Module]}).

-include("defined.hrl").
-compile([export_all]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("starting ~p......\n", [?MODULE]),
    ets:new(mq_connection, [named_table, protected]),
    ets:new(bnow_listener, [named_table, protected]),
    S = [
        ?CHILD(bnow_global, worker),
        ?CHILD(bnow_timer, worker),
        ?CHILD(bnow_cache, worker),

        ?CHILD(bnow_httpd, worker),
        ?CHILD(bnow_app, worker),
        ?CHILD(bnow_gate, worker),
        ?CHILD(bnow_flow, worker),
        ?CHILD(bnow_notify, worker),
        ?CHILD(bnow_plugin_sup, supervisor),
        ?CHILD(bnow_server, worker)
    ] ++ start_mq() ++ [?CHILD(bnow_worker_sup, supervisor), ?CHILD(bnow_bundle, worker)],
    io:format("[~p,~p]:children:~p\n", [?MODULE, ?LINE, S]),
    {ok, { {one_for_all, 5, 10}, S} }.

start_mq() ->
    MQs = bnow_var:get_app_env(bnow, request_mqs, []),
    lists:foldl(fun(Request, Acc0) ->
                        Options = bnow_var:get_app_env(bnow, Request, []),
                        {host, Host} = lists:keyfind(host, 1, Options),
                        {queue_name, QueueList} = lists:keyfind(queue_name, 1, Options),
                        Children = lists:foldl(fun({Q, PrefetchCount}, ChildAcc0) ->
                                                       NewOptions = lists:keyreplace(queue_name, 1, Options, {queue_name, [{Q, PrefetchCount}]}),
                                                       Id = bnow_listener:id(Host, Q),
                                                       ets:insert(bnow_listener, {Id}),
                                                       [?CHILD_ARG(Id, bnow_listener, [Host, Q, Options], worker) | ChildAcc0]
                                               end, [], QueueList),
                        erlang:display(Children),
                        Children ++ Acc0
                end, [], MQs).    

cast_listener(Msg) ->
    ets:foldl(fun({Id}, _) ->
                      gen_server:cast(Id, Msg)
              end, [], bnow_listener).
    
