-module(bnow_listener).
-include("defined.hrl").
-include_lib("amqp_client/include/amqp_client.hrl"). 
%-include_lib("rabbit_common/include/rabbit_framing.hrl"). 
-behavior(gen_server).
-define(SERVER, ?MODULE).
%%-record(state, { sock, nodes=[], cursor=1, master, node_num}).
-record(state, {mq_server, queue_name, connection, listener_name, channel, cursor=1, nodes=[], node_num, sock, prefetch, mq_connected=false, bundle_status=down}).


%% API
-export([start_link/1]).
-export([active_node/1 , active_node/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([id/2]).

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
start_link([Host, Queue, Options]) ->
    Id = id(Host, Queue),
    gen_server:start_link({local, Id}, ?MODULE, [Host, Queue, Options], []).

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
init([Host, Queue, Options]) ->
    {Host, Conn, [{Queue, Channel, PrefetchCount}]} = bnow_queue_utils:listen_queue(Options),
    A = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    erlang:display({?MODULE, ?LINE, A}),
    {ok, #state{queue_name=Queue,
               prefetch=PrefetchCount,
               channel=Channel,
               connection=Conn}}.

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
handle_call(close_channel, _From, State) ->
	Ch = State#state.channel,
	amqp_channel:close(Ch),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    io:format("[~p,~p]: handle other call:~p\n", [_Request]),
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
%% 当bundle工作后且nodes不为空时(即有worker启动) 才开始接收数据
handle_cast({active_node, NewNode}, #state{channel=Channel, prefetch=PrefetchCount, queue_name=Queue, nodes=[], bundle_status=ok} = State) ->
    State2 = add_node(NewNode, State),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = PrefetchCount}),
    #'basic.consume_ok'{consumer_tag = _Tag} =
        amqp_channel:subscribe(Channel, #'basic.consume'{queue = list_to_binary(Queue)}, self()),
    {noreply, State2};

handle_cast({active_node, NewNode}, #state{channel=Channel, prefetch=PrefetchCount, queue_name=Queue} = State) ->
    State2 = add_node(NewNode, State),
    {noreply, State2};

handle_cast(bnow_bundle_ok, #state{nodes=[]} = State) ->

    {noreply, State#state{bundle_status=ok}};

handle_cast(bnow_bundle_ok, #state{channel=Channel, prefetch=PrefetchCount, queue_name=Queue} = State) ->
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = PrefetchCount}),
    #'basic.consume_ok'{consumer_tag = _Tag} =
        amqp_channel:subscribe(Channel, #'basic.consume'{queue = list_to_binary(Queue)}, self()),
    {noreply, State#state{bundle_status=ok}};
    
handle_cast(_Msg, State) ->
    io:format("[~p,~p]: handle other call:~p\n", [_Msg]),
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
handle_info({#'basic.deliver'{delivery_tag = Queue_message_tag} = A, #amqp_msg{payload = BinData1} =B}, State) ->
    {N, V} = lists:nth(State#state.cursor, State#state.nodes),
    case (B#amqp_msg.props)#'P_basic'.content_type of
        <<"application/json">> -> BinData =  

    try
msgpack:pack( { noatom( jsx:decode( BinData1 ) ) } )  %  , io:format(  "====--~p\n\n" , [jsx:decode(BinData1)]  ) ;
    catch Error:Why -> 
            io:format("error\n"),
            io:format("[~p, ~p, ~p]:handle data exception, error:~p, Why:~p, ~p", [?MODULE, ?LINE, BinData1, Error, Why, erlang:get_stacktrace()]),
            error_logger:error_msg("[~p, ~p,~p]:handle data exception, error:~p, Why:~p, ~p", [?MODULE, ?LINE,BinData1, Error, Why, erlang:get_stacktrace()]),
            ok
    end;



        _-> BinData = BinData1 %, io:format( "~p \n\n" , [B#amqp_msg.props] )
    end,
if 
	is_binary( BinData ) -> ok;
true -> io:format("---- ~p\n\n" , [BinData1])
end,
%io:format( "--- ~p\n\n" , [ BinData ] ),
    MsgState = #msg_state{data = BinData,
                          queue_msg_tag = Queue_message_tag,
                          mq_req_channel = State#state.channel,
                          listener_name = State#state.listener_name},
%%    spawn(bnow_bundle, send, [BinData]),
%%    bnow_queue_utils:ack_queue(MsgState),
    bnow_worker:handle_data(N, MsgState),
    case V of
        %%   10000 -> bnow_server:sync(N);
        0 -> bnow_worker:sync(N), del_node(N, State);
        _ -> State#state{nodes=orddict:store(N, V-1, State#state.nodes)}
    end,    
    OldCursor = State#state.cursor,
    NewC = if
               OldCursor >= State#state.node_num -> 1;
               true -> OldCursor +1
           end,
%%    timer:sleep(10000),
    {noreply, State#state{cursor = NewC}};
%%    {noreply, State};


handle_info({active_node, NewNode}, #state{channel=Channel, prefetch=PrefetchCount} = State) ->
    io:format("[~p, ~p]: active node: ~p\n" ,[NewNode]),
    State2 = add_node(NewNode, State),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = PrefetchCount}),
    {noreply, State2};
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
id(Host, Queue) ->
    list_to_atom(lists:flatten(io_lib:format("bnow_listener_~s:~s", [Host, Queue]))).


active_node(N) ->
    ets:foldl(fun({X}, Acc0) ->
                      gen_server:cast(X, {active_node, N})
              end, [], bnow_listener).
%%    rpc:abcast(?MODULE, {active_node, N}).

active_node(N, Node) ->
    rpc:abcast([Node], ?MODULE, {active_node, N}).

add_node(Node, S)->
    Nodes2 = orddict:store(Node, 1000, S#state.nodes),
    S#state{nodes=Nodes2, node_num=length(Nodes2) }.

del_node(Node, S)->
    Nodes2 = orddict:erase(Node, S#state.nodes),
    S#state{nodes=Nodes2, node_num=length(Nodes2) }.

noatom(L) ->noatom( L , [] ).
noatom( [{K,V}|L] , R ) when is_atom(V) -> noatom( L , [{K,list_to_binary(atom_to_list(V))} |R] );
noatom( [V|L] , R ) ->  noatom(L , [V|R]);
noatom( [] , R ) -> R.

