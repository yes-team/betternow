-module(bnow_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, N), {{I, N}, {I, start_link, [N]}, permanent, 5000, Type, [I]}).

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
	Count = erlang:system_info(schedulers),
	L = [?CHILD(bnow_worker, worker, N) || N <- lists:seq(1, Count)],
    io:format("ok\n"),
    {ok, { {one_for_one, 5, 10}, L} }.
