-module(bnow_bundle).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([update/2]).
%-export([delete_dets_dir/1]).
-compile(export_all).

-record(state, { rrd , topn, channel, channel_num=0, queue_name, exchange, now_t, old_t=[], next_t, time, cursor , timer_after = 30000}).
-define(EtsT, bnow_bundle_ets).
-include("defined.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("starting ~p......\n", [?MODULE]),
    process_flag(trap_exit, true),
    
    Options = bnow_var:get_app_env(bnow, rrd_queue, []),
    {queue_name, QueueName} = lists:keyfind(queue_name, 1, Options),
    {exchange, RrdExchange} = lists:keyfind(exchange, 1, Options),
    {channel_num, Channel_num} = lists:keyfind(channel_num, 1, Options),

    TupleList = [bnow_queue_utils:build_mq_connection(Options) || X <- lists:seq(1, Channel_num)],
    
    Channels = [{Conn, bnow_queue_utils:queue_connect(Conn, QueueName)} || {ok, Conn} <- TupleList],
    
    Time = bnow_timer:now(),
    %{ok, DetsName} = create_dets_table(Time),

    NNowT = ets:new(?EtsT, [public, set]),
    NextT = ets:new(?EtsT, [public, set]),

    mnesia:dirty_write(#bnow_cursor{key=Time}),

    send_after(1000 ),

    self() ! recover,
%    timer:send_interval(60000, ?MODULE, delete_dets_dir),

    io:format("~p:ok\n", [?MODULE]),
    {ok, #state{channel=Channels, 
                queue_name=QueueName,
                exchange=RrdExchange, 
                old_t=[], 
                now_t=NNowT, 
                %dets_t=DetsName,
                next_t=NextT, 
                time=Time,
                cursor=1,
                channel_num=Channel_num
               }}.

send_after(Time) ->
    timer:send_after(Time, ?MODULE, time_send_to_rrd).

set_timer(Time) -> 
    gen_server:call( ?MODULE , {set_time_after , Time} ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_channel, _From, #state{channel=Channels, queue_name=QueueName, exchange=ExChange, cursor=Cursor} = S) ->
    CH = lists:nth(Cursor, Channels),
    NewC = if
               Cursor >= S#state.channel_num -> 1;
               true -> Cursor +1
           end,
    {reply, {ok, {CH, QueueName, ExChange}}, S#state{cursor=NewC}};
handle_call(state, _From, State) ->

    {reply, {ok, State}, State};
handle_call(get_now_table, _From, #state{now_t=NowT} = State) ->
    {reply, {ok, NowT}, State};

handle_call({set_time_after , Time} , _From , State) ->
    { reply , ok , State#state{timer_after = Time} };
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(delete_dets_dir, #state{time=Time} = S) ->
    Dir_name  = (bnow_var:int(Time) div 100) - 2 ,
    spawn_link(?MODULE, delete_dets_dir, [Dir_name]),
    {noreply, S};
    
    
handle_info(time_send_to_rrd, #state{now_t=NowT, old_t=[],channel=[CH|_], queue_name=QN, exchange=EX, time=Time, next_t=NextT} = S) ->
    NewTime = bnow_timer:now(),
    case Time of
        undefined ->
            %{ok, DetsName} = create_dets_table(NewTime),
            NNowT = ets:new(?EtsT, [public, set]),

            mnesia:dirty_write(#bnow_cursor{key=NewTime}),
            timer:send_after( S#state.timer_after, ?MODULE, time_send_to_rrd ),
            {noreply, S#state{old_t=[], now_t=NNowT, time=NewTime}};
        NewTime ->
            timer:send_after( S#state.timer_after, ?MODULE, time_send_to_rrd ),
            {noreply, S#state{old_t=[]}};
        _ ->
            %{ok, DetsName} = create_dets_table(NewTime),
            NNextT = ets:new(?EtsT, [public, set]),
            mnesia:dirty_write(#bnow_cursor{key=NewTime}),
            timer:send_after( S#state.timer_after, ?MODULE, time_send_to_rrd ),
            {noreply, S#state{old_t=[{NowT, NewTime}], now_t=NextT, next_t=NNextT, time=NewTime}}
    end;

handle_info(time_send_to_rrd, #state{now_t=NowT, old_t=[{OldT, OldTime}|T],channel=CHs, queue_name=QN, exchange=EX, time=Time, next_t=NextT} = S) ->
    {_, CH} = lists:nth(S#state.cursor, CHs),    
    mnesia:transaction(fun() ->
                               Num = ets:foldr(fun(Data, Acc0) ->
                                                       NewC = if
                                                                  Acc0 >= S#state.channel_num -> 1;
                                                                  true -> Acc0 +1
                                                              end,
                                                       bnow_cache:insert_file( delete , Data ),
                                                       send_to_rabbit(Data, CH, QN, EX),
                                                       NewC
                                               end, 1, OldT),
%%                               io:format("send to rrd num:~p\n", [Num]),
                               ets:delete(OldT),
                               mnesia:delete({bnow_cursor, OldTime})
                       end),
    NewTime = bnow_timer:now(),
    OldCursor = S#state.cursor,
    NewC = if
               OldCursor >= S#state.channel_num -> 1;
               true -> OldCursor +1
           end,
    case NewTime of
        Time ->
            timer:send_after( S#state.timer_after, ?MODULE, time_send_to_rrd ),
            {noreply, S#state{old_t=T}};
        _ ->
            %{ok, DetsName} = create_dets_table(NewTime),
            NNextT = ets:new(?EtsT, [ public, set]),
            mnesia:dirty_write(#bnow_cursor{key=NewTime}),
            timer:send_after( S#state.timer_after, ?MODULE, time_send_to_rrd ),
            {noreply, S#state{old_t=T ++ [{NowT, NewTime}], now_t=NextT, next_t=NNextT, time=NewTime}}
    end;

handle_info(recover, #state{channel=CH, queue_name=QN, exchange=EX, now_t=NowT } = S) ->
    mnesia:transaction(
      fun() ->
              mnesia:foldl(fun(#bnow_cursor{key=Name}, _) ->
            %                       recover(Name, CH, QN, EX),
                                   ok
                           end, [], bnow_cursor)
      end),
    mnesia:clear_table(bnow_cursor),    
    case NowT of
        undefined -> ok;
        _  ->
            mnesia:dirty_write(#bnow_cursor{key=NowT})
    end,
        bnow_sup:cast_listener(bnow_bundle_ok),
    {noreply, S};
handle_info(test, #state{now_t=NowT, old_t=OldT,channel=CH, queue_name=QN, exchange=EX } = S) ->
    Num = ets:foldr(fun({Bin}, Acc0) ->
                      bnow_queue_utils:send_message_to_queue(CH, QN, EX, <<"abcdefghijklmnopqrstuvwxyz">>),
                      Acc0 +1
              end, 0, test),
    io:format("test send to rrd:~p\n", [Num]),
    {noreply, S};
    
handle_info(_Info, State) ->
    io:format("[~p, ~p]: info:~p\n", [?MODULE, ?LINE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("[~p, ~p]: terminate for reason:~p\n", [?MODULE, ?LINE, _Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

key(K) ->
    Name = bnow_var:get_app_env(bnow, application),
    <<"/",Name/binary , K/binary>>.


update( rrd , {Name, Time, Value, Step, T } ) -> 
    {ok, NowT} = gen_server:call(?MODULE, get_now_table),
    Name1 = key( Name ),
    NewValue = case ets:lookup(NowT, {Name1, Time}) of
        [] ->
                       {Value, 1};
        [{{Name1, Time}, {OldValue,Count}, Step, T }] ->
            case T of
                0 -> % avg
                    {Value + OldValue, Count+1};
                1 -> % sum
                    {Value + OldValue, Count+1};
                2 -> % max
                    {max(Value, OldValue), Count+1};
                3 -> % min
                    {min(Value, OldValue), Count+1}
                    
            end
    end,
%%    dets:insert(DetsT, {{Name1, Time}, NewValue, Step, 0 }),
    bnow_cache:insert_file(insert, {{Name1, Time}, NewValue, Step, 0 }),
    ets:insert(NowT, {{Name1, Time}, NewValue, Step, T });
    
update( rrd , {Name, Time, Value, Step, T } ) -> 
    Name1 = key( Name ),
    Bin = << 1, 0, (bnow_var:int(Time)):32/big , (size(Name1)):16/big, Name1/binary , (bnow_var:int(Value)):64/big , T:8 , Step:16/big, 1:32/big>>,
%%    {ok, #state{channel=CH, queue_name=QN, exchange=EX}} = gen_server:call(?MODULE, state),
    {ok, {CH, QN, EX}} = gen_server:call(?MODULE, get_channel),
    bnow_queue_utils:send_message_to_queue(CH, QN, EX, Bin),
%%    spawn(bnow_queue_utils,  send_message_to_queue, [CH, QN, EX, Bin]),
    ok;

update(topn, #topn_cmd{name=N, value=Value} = TC) ->
    Name = bnow_var:bin(N),
    Name1 = key(Name),
    {ok, NowT} = gen_server:call(?MODULE, get_now_table),
    NewValue = case ets:lookup(NowT, Name1) of
        [] ->
                       {Value,1};
        [{Name1, {OldValue,Count}, _}] ->
                       {OldValue + Value, Count+1}
               end,
%%    dets:insert(DetsT, {Name1, NewValue,  TC}),
    bnow_cache:insert_file(insert, {Name1, NewValue,  TC}),
    ets:insert(NowT, {Name1, NewValue, TC });
    
update(topn, #topn_cmd{name=N, item=I, value=V1, interval_hours=H1, size=Size1, limit=Limit1}) ->
%%    io:format("~p,~p,~p,~p,~p,~p\n" ,[N, I, V1, H1, Size1, Limit1]),
    Name = bnow_var:bin(N),
    Name1 = key(Name),
    NameLen = size(Name1),
    Item = bnow_var:bin(I),
    ItemLen = size(Item),
    V = bnow_var:bin(V1),
    VLen = size(V),
    H = bnow_var:bin(H1),
    HLen = size(H),
    Size = bnow_var:bin(Size1),
    SizeLen = size(Size),
    Limit = bnow_var:bin(Limit1),
    LimitLen = size(Limit),
    
    Bin = <<2, 0, NameLen:16/big, Name1/binary, ItemLen:16/big, Item/binary, VLen:16/big, V/binary, HLen:16/big, H/binary, SizeLen:16/big, Size/binary, LimitLen:16/big, Limit/binary >>,
%%    {ok, #state{channel=CH, queue_name=QN, exchange=EX}} = gen_server:call(?MODULE, state),
    {ok, {CH, QN, EX}} = gen_server:call(?MODULE, get_channel),
    bnow_queue_utils:send_message_to_queue(CH, QN, EX, Bin),
%%    spawn(bnow_queue_utils, send_message_to_queue, [CH, QN, EX, Bin]),
    ok.
    
name_id(Name, EtsName) ->
    list_to_atom(lists:flatten(io_lib:format("~s_~s"), [bnow_var:list(Name), bnow_var:list(EtsName)])).

send(Bin) ->
    io:format("here\n"),
    {ok, #state{channel=CH, queue_name=QN, exchange=EX}} = gen_server:call(?MODULE, state),
    io:format("here1\n"),
%%    spawn(bnow_queue_utils,  send_message_to_queue, [CH, QN, EX, Bin]).
    bnow_queue_utils:send_message_to_queue([CH, QN, EX, Bin]).

%create_dets_table(Name) ->
%    MinDir = bnow_var:get_app_env(bnow, detsdir, "") ++ "/" ++  bnow_var:list(bnow_var:int(Name) div 100),
%    case file:make_dir(MinDir) of
%        ok -> ok;
%        {error, eexist} -> ok;
%        {error, Error} ->
%            Msg = lists:flatten(io_lib:format("[~p, ~p]mkdir failed, dir:~p\n", [?MODULE, ?LINE, MinDir])),
%            io:format("[~p, ~p]mkdir failed, dir:~s\n", [?MODULE, ?LINE, MinDir]),
%            throw(Msg)
%    end,
%    Path = MinDir ++ "/" ++  bnow_var:list(Name),
%
%    dets:open_file(Path, [{ram_file, false}]).

%delete_dets_dir(DetsName) ->
%    MinDir = bnow_var:get_app_env(bnow, detsdir, "") ++ "/" ++  bnow_var:list(DetsName),
%    os:cmd("rm -rf " ++ MinDir).
    

%recover(Name, CH, QN, EX) ->
%    MinDir = bnow_var:get_app_env(bnow, detsdir, "") ++ "/" ++  bnow_var:list(bnow_var:int(Name) div 100),
%    Path = MinDir ++ "/" ++ bnow_var:list(Name),
%    dets:foldl(fun(Data, Acc0) ->
%                          send_to_rabbit(Data, CH, QN, EX)
%                    end, 0, Path).


send_to_rabbit({{Name, Time}, {Value,Count}, Step, T }, CH, QN, EX) ->
    Bin = << 1, 0, (bnow_var:int(Time)):32/big , (size(Name)):16/big, Name/binary , (bnow_var:int(Value)):64/big,  T:8 , Step:16/big, (bnow_var:int(Count)):32/big>>,
    bnow_queue_utils:send_message_to_queue(CH, QN, EX, Bin);
send_to_rabbit({Name, {Value, _}, #topn_cmd{item=I, interval_hours=H1, size=Size1, limit=Limit1}}, CH, QN, EX) ->
    NameLen = size(Name),
    Item = bnow_var:bin(I),
    ItemLen = size(Item),
    V = bnow_var:bin(Value),
    VLen = size(V),
    H = bnow_var:bin(H1),
    HLen = size(H),
    Size = bnow_var:bin(Size1),
    SizeLen = size(Size),
    Limit = bnow_var:bin(Limit1),
    LimitLen = size(Limit),
    Bin = <<2, 0, NameLen:16/big, Name/binary, ItemLen:16/big, Item/binary, VLen:16/big, V/binary, HLen:16/big, H/binary, SizeLen:16/big, Size/binary, LimitLen:16/big, Limit/binary >>,
    bnow_queue_utils:send_message_to_queue(CH, QN, EX, Bin).

    
