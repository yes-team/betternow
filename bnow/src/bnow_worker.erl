-module(bnow_worker).
-behaviour(gen_server).
-include("defined.hrl").
-define(SERVER, ?MODULE).
-record(state, {count=1000}).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([handle_data/2, handle_data_bin/1, sync/1]).
-compile(export_all).

-export([whereis_name/1, register_name/2, unregister_name/1, send/2]).

whereis_name(Name)->
    case ets:lookup(?MODULE, Name) of
        [{_, Pid}|_] -> Pid;
        _ -> undefined
    end.

register_name(Name, Pid)->
    ets:insert(?MODULE, {Name, Pid}),
    yes.

unregister_name(Name)->
    ets:delete(?MODULE, Name).

send(Name, Data)->
    ok.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(N) ->
    io:format("staring ~p.....", [?MODULE]),
    gen_server:start({via, ?MODULE, N}, ?MODULE, [N], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([N]) ->
    bnow_listener:active_node(self()),
    io:format("~p ok\n", [?MODULE]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    io:format("~p(cast): ~p\n", [?MODULE, _Cast]),
    {noreply, State}.

handle_info({sync, From}, State) ->
    bnow_listener:active_node(self(), From),
    {noreply, State};

handle_info({data, #msg_state{data = Data} = MsgState}, S) ->
%%    fprof:trace(start),
    try
        handle_data_bin(Data)
    catch Error:Why -> 
            io:format("error\n"),
            io:format("[~p, ~p]:handle data exception, error:~p, Why:~p, ~p", [?MODULE, ?LINE, Error, Why, erlang:get_stacktrace()]),
            error_logger:error_msg("[~p, ~p]:handle data exception, error:~p, Why:~p, ~p", [?MODULE, ?LINE, Error, Why, erlang:get_stacktrace()]),
            ok
    end,
    bnow_server:counter(),
    bnow_queue_utils:ack_queue(MsgState),
%%    fprof:profile(),
%%    fprof:analyse({dest,"file.analysis"}),
    {noreply, S};

handle_info(_Info, State) ->
    io:format("~p(info): ~p\n", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[~p, ~p]: terminate for reason:~p\n", [?MODULE, ?LINE, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_data(Pid, MsgState)->
	Pid ! {data, MsgState}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_data_bin(Bin)->
    %% error_logger:info_msg("[~p:~p] here:~p~n",[?MODULE, ?LINE, "handle_data_bin"]),
    D = bnow_record:from_bin(Bin),
    bnow_bundle:update( rrd , { <<?SYSTEM_COUNTER>>, bnow_timer:now(), 1, 30, ?T_SUM } ),
    bnow_filter_runtime:handle_filter(bnow_var:bin(bnow_record:class(D)), D),
    bnow_record:close(D),
    ok.

%--------------------------------------------------------------

exec_filter(#bnow_filter{class=Class} = F, D)->
    case bnow_var:bin(bnow_record:class(D)) of
        Class ->
            filter_item(F#bnow_filter.data, {true, D});
        _ -> 
            nomatch
    end.

filter_item(_, false)-> ok;
filter_item([], _)-> ok;

filter_item([A | T], {true, D}) when is_record(A, bnow_filter)->
%%    List = bnow_record:clone(D),
    D_new = bnow_var:atom(A#bnow_filter.id),
    bnow_record:clone(D, D_new),
%%    lists:foldl(fun({K, V}, Acc0) ->
%%                        bnow_record:set_value(K, V, D_new)
%%                end, D, List),
    exec_filter(A, D_new),
%%        bnow_record:close(D_new),
%%    exec_filter(A, bnow_record:clone(D)),
    filter_item(T, {true, D});

filter_item([A | T], {true, D}) when is_record(A, bnow_action)->
    %% error_logger:info_msg("[~p:~p] bnow_action, A:~p~n",[?MODULE, ?LINE, A]),
    apply_action(A, D),
    filter_item(T, {true, D});

filter_item([A | T], {true, D}) when is_record(A, bnow_assert)->
    Continue2 = bnow_app:match_assert(A, D),
    filter_item(T, Continue2);

filter_item([A | T], {true, D0}) when is_record(A, bnow_set)->
    D1 = bnow_app:apply_set(A, D0),
    filter_item(T, {true, D1}).

%--------------------------------------------------------------

apply_action(#bnow_action{action=flow} = A, D)->
    Name = magic:exec(A#bnow_action.compiled_args, D),
    bnow_flow:handle_data(Name, bnow_record:to_list(D)),
    ok;

apply_action(#bnow_action{action=topn, compiled_args=T} = A, D)->
    T2 = T#topn_cmd{
        name = magic:exec(T#topn_cmd.name, D),
        item = magic:exec(T#topn_cmd.item, D),
        value = magic:exec(T#topn_cmd.value, D)
    },
    bnow_bundle:update(topn, T2),
%%    bnow_topn:update(T2),
    ok;

apply_action(#bnow_action{action=update} = A, D)->
    {N, V} = A#bnow_action.compiled_args,
    Name = magic:exec(N, D),
    Value = magic:exec(V, D),
    Time = bnow_record:time(D),
    bnow_bundle:update( rrd, {Name, Time, Value, 30, (A#bnow_action.args)#update_args.function } ), 
    ok;

apply_action(#bnow_action{action=plugin} = A, D)->
                                                      
    bnow_plugin:handle_event(A, D);
apply_action(_, _)-> 
    ok.

sync(Pid)->
	Pid ! {sync, node()}.
