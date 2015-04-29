-module(bnow_plugin_sup).
-include("defined.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([unload/1, load/1]).
-compile(export_all).

%% Helper macro for declaring children of supervisor
-define(CHILD(M, Opts), {M, {bnow_plugin, start_link, [M, Opts]}, permanent, 5000, worker, [bnow_plugin, M]}).

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
    {atomic,M} = mnesia:transaction(fun()->
        lists:foreach(fun(App)->
                bnow_app:load_beams(App)
            end, mnesia:all_keys(bnow_plugin)),
        mnesia:foldr(fun(M, L)->
            [?CHILD(M#bnow_plugin.module, M#bnow_plugin.options) | L]
            end, [], bnow_plugin)
        end),
    io:format("ok\n"),
    {ok, { {one_for_one, 5, 10}, M} }.

load(M)->
    supervisor:start_child(?MODULE, ?CHILD(M#bnow_plugin.module, M#bnow_plugin.options)).

unload(M)->
    supervisor:terminate_child(?MODULE, M#bnow_plugin.module),
    supervisor:delete_child(?MODULE, M#bnow_plugin.module).
