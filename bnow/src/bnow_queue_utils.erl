%%%-------------------------------------------------------------------
%%% @copyright 2013 ShopEx Network Technology Co, .Ltd
%%% @author liyouyou <liyouyou@shopex.cn>
%%% @doc
%%% bnow_queue_utils.erl
%%% @end
%%% Created : 2013/08/01 02:36:05 liyouyou
%%% #Time-stamp: <syrett 2014-05-09 17:30:47>
%%%-------------------------------------------------------------------

-module(bnow_queue_utils).

-include_lib("amqp_client/include/amqp_client.hrl"). 
-include("defined.hrl").

-compile(export_all).


listen_queue(Options) ->
    {ok, Connection} = build_mq_connection(Options),
    {queue_name, QueueList} = lists:keyfind(queue_name, 1, Options),
    case QueueList of
        [] ->
            ok = amqp_connection:close(Connection),
            throw("no queues");
        _ ->
        {host, Host} = lists:keyfind(host, 1, Options),
        QueueChannel = lists:foldl(fun({QueueName,PrefetchCount}, Acc0) ->
                                           Channel = queue_connect(Connection, QueueName),
                                           amqp_channel:call(Channel, #'basic.qos'{prefetch_count = PrefetchCount}),
                                           [{QueueName, Channel, PrefetchCount}|Acc0]
                                   end, [], QueueList),
        {Host, Connection, QueueChannel}
    end.
                        
queue_connect(Conn, QueueName) ->
    {Ret, Channel} = amqp_connection:open_channel(Conn),
    case Ret of
        error ->
            ?dio(Ret),
            ok = amqp_connection:close(Conn),
            throw("open channel failed");
        ok ->
            ?dio(ok),
            Result = amqp_channel:call(Channel,
                                       #'queue.declare'{queue = list_to_binary(QueueName),
                                                        durable = true}),
            case Result of
                #'queue.declare_ok'{queue = _Q} -> ok;
                _ ->
                    ok = amqp_channel:close(Channel),
                    ok = amqp_connection:close(Conn),
                    throw("queue.declare failed")
            end,
            Channel
    end.
    
build_mq_connection(Options) ->
    io:format("[~p,~p], connect queue, options:~p\n", [?MODULE, ?LINE, Options]),
    NetWork = lists:foldl(fun({host, H}, Acc0) ->
                                  Acc0#amqp_params_network{host=H};
                             ({virtual_host, VH}, Acc0) ->
                                  Acc0#amqp_params_network{virtual_host=VH};
                             ({username, U}, Acc0) ->
                                  Acc0#amqp_params_network{username=U};
                             ({password, P}, Acc0) ->
                                  Acc0#amqp_params_network{password=P};
                             ({port, Port}, Acc0) ->
                                  Acc0#amqp_params_network{port=Port};
                             ({heartbeat, HT}, Acc0) ->
                                  Acc0#amqp_params_network{heartbeat=HT};
                             (_, Acc0) ->
                                  Acc0
                          end, #amqp_params_network{}, Options),
    io:format("[~p,~p], connect queue network:~p\n", [?MODULE, ?LINE, NetWork]),
    case amqp_connection:start(NetWork) of
        {ok, Connection} ->
            {ok, Connection};
        _Error ->
            io:format("[~p,~p]: connect error:~p\n", [?MODULE, ?LINE, _Error]),
            throw("connect error")
    end.
    
    
close_mq_connection(Connection, Channel) ->
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.

send_message_to_queue(Channel, Queue_name, Exchange, Bin) ->
    ok = amqp_channel:cast(Channel,
                           #'basic.publish'{
%%                             exchange = bnow_var:bin(Exchange),
                             routing_key = list_to_binary(Queue_name)},
                           #amqp_msg{payload = Bin}),
    ok.

ack_queue(MsgState) when is_record(MsgState, msg_state) ->
    amqp_channel:call(MsgState#msg_state.mq_req_channel, #'basic.ack'{delivery_tag = MsgState#msg_state.queue_msg_tag}).

test() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "192.168.51.66", virtual_host= <<"/">>, username= <<"guest">>, password = <<"guest">>}).
