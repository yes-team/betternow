-module(bnow_server).
-behaviour(gen_server).
-include("defined.hrl").
-define(SERVER, ?MODULE).
-record(state, {count=0, count_last=0, mcount=0, mcount_last=0, line=0}).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, sync/0, sync/1, counter/0, count/0, timer_second/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    bnow_global:store(running, true)
    %,bnow_bundle:init()
    ,bnow_distinct:db()
    ,{ok, #state{}}.


handle_call(count, _From, S) ->
    {reply, {S#state.count_last, S#state.mcount}, S};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(counter, S) ->
    {noreply, S#state{count=S#state.count+1, mcount=S#state.mcount+1}};

handle_cast({timer_second, Now}, S) ->
    S2 = case Now - S#state.line of
        N when N > 60 -> S#state{mcount_last=S#state.mcount, mcount=0};
        _ -> S
    end,
    {noreply, S#state{count_last=S#state.count, count=0}};

handle_cast(_Cast, State) ->
    io:format("bnow_server(cast): ~p\n", [_Cast]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("bnow_server(info): ~p\n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format( "bnow server terminated\n\n", [] )
    ,ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sync()->
    ets:foldl(fun(A, _)->
        erlang:display(A)
    end, [], bnow_worker),
    gen_server:abcast(?MODULE, {sync, node()}).
sync(N)->
    gen_server:cast({?MODULE, N}, {sync, node()}).

counter()->
    gen_server:cast(?MODULE, counter).

count()->
    gen_server:call(?MODULE, count).

timer_second(Now)->
    gen_server:cast(?MODULE, {timer_second, Now}).
