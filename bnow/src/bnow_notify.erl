-module(bnow_notify).

-include_lib("stdlib/include/qlc.hrl").
%-include_lib("../../../bnow/deps/cowboy/include/http.hrl").
-include("defined.hrl").
-behaviour(gen_server).
-compile(export_all).
%-define(dline(X), erlang:display([?MODULE, ?LINE, X])).

-export([filter/1, del_rule/2, add_rule/6, add_rule/7, list_rule/0, list_rule/1, list_rule/2, list_rule/3]).
-export([add_token/2, add_token/3, list_token/0, list_token/2, list_token/3 ,del_token/1]).
-export([md5/1]).


-define(retry, 3).
-define(Interval, 60).

-record(state, {}).
-record(filter_avg, {range, threshold}).
-record(filter_num, {range, threshold, num}).
-record(filter_rate, {range, threshold}).

-define(notify, ?MODULE ).
-define(notify_token , bnow_notify_token ).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    io:format("starting ~p......\n", [?MODULE]),
    Pid = spawn_link(?MODULE, init_timer_loop, []),
    register(bnow_notify_timer, Pid),
    init_timer(),
    inets:start(),
    io:format("ok\n"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


list_rule() ->
    list_rule(1).

list_rule(Offset) ->
    list_rule(Offset, 10).

list_rule(Offset, Limit) ->
    list_rule(Offset, Limit, <<>>).

list_rule(Offset, Limit, Match) ->
    F = fun() ->
        QH = case Match of
            <<>> ->
                qlc:sort(qlc:q([X||X<-mnesia:table(?notify)]));
            [] -> 
                qlc:sort(qlc:q([X||X<-mnesia:table(?notify)]));
            _ -> 
                qlc:sort(qlc:q([X||X<-mnesia:table(?notify), list_rule_match(X, Match)]))
        end,
        Qc = qlc:cursor(QH),
        case Offset of
            1 -> skip;
            _ -> qlc:next_answers(Qc,Offset-1)
        end,
        qlc:next_answers(Qc,Limit)
    end,
    {atomic,L} = mnesia:transaction(F),
    {ok,L}.


list_rule_match(X, Match) ->
    lists:foldl(fun({<<"token_id">>, Token}, AccIn) when X#?notify.token=:=Token ->
                     AccIn;
                ({<<"key">>, Key}, AccIn) when X#?notify.key=:=Key ->
                    AccIn;
                ({<<"callback_id">>, Callback_Id}, AccIn) when X#?notify.callback_id=:=Callback_Id ->
                    AccIn;
                (_, _) ->
                    false
                end, true, Match).

add_token(Id, Callback) ->
    Token = md5(Id ++ io_lib:format("~p", [Callback])),
    case add_token(Id, Token, Callback) of
        {ok, true} -> {ok, Token};
        A -> A
    end.

add_token(Id, Token, Callback) ->
    case ets:lookup(?notify_token, Id) of
        [] -> ok = mnesia:dirty_write(#?notify_token{id=Id, token=Token, callback=Callback}),
             {ok, true};
        _ -> {error, 'token_id is exist'}
    end.

del_token(Id) ->
    ok = mnesia:dirty_delete(?notify_token, Id),
    {ok, true}.

list_token() ->
    list_token(1, 10).

list_token(Offset, Limit) ->
    list_token(Offset, Limit, []).

list_token(Offset, Limit, Match) ->
     F = fun() ->
        QH = case Match of
            <<>> -> qlc:sort(qlc:q([X||X<-mnesia:table(?notify_token)]));
            [] -> qlc:sort(qlc:q([X||X<-mnesia:table(?notify_token)]));
            _  -> qlc:sort(qlc:q([X||X<-mnesia:table(?notify_token), binary:match(X#?notify_token.id, Match)=/=nomatch]))
        end,
        Qc = qlc:cursor(QH),
        case Offset of
            1 -> skip;
            _ -> qlc:next_answers(Qc,Offset-1)
        end,
        qlc:next_answers(Qc,Limit)
    end,
    {atomic,L} = mnesia:transaction(F),
    {ok,L}.


del_rule(Key, Token) ->
    ok = mnesia:dirty_delete(?notify, <<Key/binary, ":", Token/binary>>),
    {ok, true}.
        
add_rule(Key, Token, Time, Filter, Callback_Id, Action) ->
    add_rule(Key, Token, Time, Filter, Callback_Id, Action, true).

add_rule(Key, Token, Time, Filter, Callback_Id, Action, Readonly) ->
    Id = <<Key/binary, ":", Token/binary>>,
    case ets:lookup(?notify, Id) of
        [{?notify, Id, _, _, true}] -> 
            {error, 'readonly'};
        _ ->
            F = case proplists:get_value(<<"type">>, Filter) of
                <<"avg">> -> 
                    #filter_avg{
                        range=bnow_var:int(proplists:get_value(<<"range">>, Filter)), 
                        threshold=bnow_var:int(proplists:get_value(<<"threshold">>, Filter))};
                <<"num">> ->
                    #filter_num{
                        range=bnow_var:int(proplists:get_value(<<"range">>, Filter)), 
                        threshold=bnow_var:int(proplists:get_value(<<"threshold">>, Filter)), 
                        num=bnow_var:int(proplists:get_value(<<"num">>, Filter))};
                <<"rate">> ->
                    #filter_rate{
                        range=bnow_var:int(proplists:get_value(<<"range">>, Filter)), 
                        threshold=bnow_var:int(proplists:get_value(<<"threshold">>, Filter))};
                T ->
                    error_logger:error_msg("type error~, ~p ~n", [T]),
                    throw('type error')
            end,
            ok = mnesia:dirty_write(#?notify{id=Id, key=Key, callback_id=Callback_Id, time=bnow_var:int(Time), filter=F, action=Action, readonly=Readonly, token=Token}),
            send_init_timer(bnow_var:int(Time)),
            {ok, true}
    end.


filter( Time ) ->
    ets:foldl(fun(#?notify{key=Key, callback_id=Callback_Id, time=T, filter=Filter, action=Action, token=Token_id}, _AccIn) when T=:=Time ->
            %?dline(binary_to_list(Action)),
            %% { avg, {range,5}, {val,50} }
            %% { num, {range,5}, {val,50}, {num,3} }
            [Callback, Threshold, Real_Val] = case Filter of
                %% 单位时间范围(Range)内 总平均值超出 阈值(Threshold)
                #filter_avg{ range=Range, threshold=Threshold1 } -> 
                    %?dline(filter_avg),
                    filter_avg(Key, Range, Threshold1);
                %% 单位时间范围(Range)超出 阈值(Threshold) N 次
                #filter_num{ range=Range, threshold=Threshold1, num=Num } -> 
                    %?dline(filter_num),
                    filter_num(Key, Range, Threshold1, Num);
                #filter_rate{ range=Range, threshold=Threshold1 } -> 
                    %?dline(filter_num),
                    filter_rate(Key, Range, Threshold1);
                _ -> 
                    false
            end,
            %?dline([Callback, Threshold, Real_Val]),
            case Callback of 
                true -> try
                            callback(Callback_Id, Token_id, Action, Threshold, Real_Val)
                        catch Error:Reason ->
                            error_logger:error_msg("[~p, ~p] Error:~p,  Reason:~p ~n", [?LINE, ?MODULE, Error, Reason])
                        end;
                _ -> skip 
        end; (_,_) ->
            skip
        end
    , [], ?notify),
    destructor().

%%%----------------------------------------------------------------------
%%% Internal Functions
%%%----------------------------------------------------------------------
filter_avg(Key, Range, Threshold) ->
    {ok, {_Type, L}} = fetch_rrd(Key, Range),
    Val = lists:foldl(fun({_, {V, _}}, AccIn) ->
            AccIn + V
    end, 0, L),
    %?dline([binary_to_list(Key), Val, Range, Threshold]),
    Real_Val = erlang:round(Val/Range),
    [Real_Val > Threshold, Threshold, Real_Val].

filter_num(Key, Range, Threshold, Num) ->
    {ok, {_Type, L}} = fetch_rrd(Key, Range),
    Val = lists:foldl(fun({_, {V, _}}, AccIn) ->
            case V > Threshold of
                true -> AccIn +1;
                false -> AccIn
            end
    end, 0, L),
    %?dline([binary_to_list(Key), Val, Range, Threshold, Num]),
    [Val > Num, Num, Val].

filter_rate(Key, Range, Threshold) ->
    {ok, {_Type, L}} = fetch_rrd(Key, Range),
    {RRate , {_RMin , _RMax} , _ } = lists:foldl(fun 
        ({_, {0, _}}, OriginAcc) -> OriginAcc;
        ({_, {V, _}}, {0 , {0, 0} , 0 }) ->
            {0 , {V,V} ,V};
        ({_, {V, _}}, {Rate , {CurMin , CurMax } , Min }) ->
            if
                V > CurMax -> 
                    { V/Min , { Min , V } , Min }
                ;V < CurMin ->
                    { Rate , {CurMin , CurMax} , V }
                ;true -> {Rate , { CurMin , CurMax } , Min }
            end
        
    end, { 0 , {0,0}, 0 }, L),
    %?dline([binary_to_list(Key), Val, Range, Threshold]),
    [RRate * 100 > Threshold, Threshold, RRate].


callback(Callback_Id, Token_id, Action, Threshold, Real_Val) ->
    [Token,Callback] = case ets:lookup(?notify_token, Token_id) of
        [] -> <<>>;
        [#?notify_token{token=Token1,callback=Callback1}] -> [Token1, Callback1]
    end,
    %?dline([Callback_Id, Callback, Action, Threshold, Real_Val, token, binary_to_list(Token)]),
    %?dline( binary_to_list(proplists:get_value(<<"type">>, Callback)) ),
    case proplists:get_value(<<"type">>, Callback) of
        <<"http">> ->
            callback_http(Callback_Id, binary_to_list(proplists:get_value(<<"url">>, Callback)), Token, Action, Threshold, Real_Val);
        <<"tcp">> ->
            case proplists:get_value(<<"host">>, Callback, []) of
                [] ->
                    case application:get_env(ecae_host) of
                        undefined -> 
                            error_logger:error_msg("[~p, ~p] tcp callback, host is empty & get_env(ecae_host) failed! ~n", [?LINE, ?MODULE]);
                        {ok, Host} ->
                            {ok, Port} = application:get_env(ecae_port),
                            callback_tcp(Callback_Id, Host, Port, Token, Action, Threshold, Real_Val)
                    end;
                H ->
                    Host = binary_to_list(H),
                    Port = bnow_var:int(proplists:get_value(<<"port">>, Callback)),
                    callback_tcp(Callback_Id, Host, Port, Token, Action, Threshold, Real_Val)
            end;
        <<"apply">> ->
        %?dline(apply),
            Module = list_to_atom(binary_to_list(proplists:get_value(<<"module">>, Callback, ?MODULE))), 
            Func = list_to_atom(binary_to_list(proplists:get_value(<<"func">>, Callback, callback_no_apply))),
            %?dline([Module, Func]),
            apply(Module, Func, [[{token,Token}, {callback_id,Callback_Id}, {action,Action}, {threshold,Threshold}, {real_val,Real_Val}]]);
        _ ->
            ok
    end.


callback_http(C, Url, T, A, N, Rule_val1)  ->
    Callback_Id = binary_to_list(C), 
    Token = binary_to_list(T), 
    Action = binary_to_list(A), 
    Now_val = integer_to_list(N),
    Rule_val = integer_to_list(Rule_val1),
    Time = integer_to_list(bnow_timer:now()),
    Ac = md5( md5(Action++Callback_Id++Now_val++Rule_val++Time) ++ Token ),
    Arg = "ac="++ Ac ++
        "&action=" ++ Action ++ 
        "&callback_id="++ Callback_Id ++ 
        "&now_val=" ++ Now_val ++
        "&rule_val=" ++ Rule_val ++
        "&time=" ++ Time,
    callback_http(Url, Arg, 1).

callback_http(Url, Arg, N) when N > ?retry->
    error_logger:error_msg("[~p, ~p] callback Failed! Url:~p, ~n", [?LINE, ?MODULE, Url]);
callback_http(Url, Arg, N) ->
    %?dline([Token, Callback_Id, Action, Threshold, Real_Val]),
    %?dline(Arg),
    case httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Arg}, [{timeout,3000}], []) of
        {ok, Result} ->
            %?dline(Result),
            {ok, Result};
        {error, Reason} ->
            error_logger:error_msg("retry(callback):~p,[~p, ~p] Url:~p, ~n", [N, ?LINE, ?MODULE, Url]),
            callback_http(Url, Arg, N+1)
    end.

callback_tcp(Callback_Id, H, P, Token, Action, Threshold, Real_Val) ->
    callback_tcp(Callback_Id, H, P, Token, Action, Threshold, Real_Val, 1).

callback_tcp(Callback_Id, H, P, Token, _Action, _Threshold, _Real_Val, N) when N > ?retry ->
    %?dline([callback_tcp_failed, H, P]),
    error_logger:error_msg("[~p, ~p] callback Failed! callback_id:~p, Host:~p, Port:~p, Token:~p ~n", [?LINE, ?MODULE, Callback_Id, H, P, Token]),
    {error, "callback failed!"};
callback_tcp(Callback_Id, H, P, Token, Action, Threshold, Real_Val, N) ->
    case get_tcp_socket(H, P, N) of
        {error, Reason} ->
            error_logger:error_msg("[~p, ~p] get_tcp_socket failed! ~p ~n", [?LINE, ?MODULE, Reason]),
            callback_tcp(Callback_Id, H, P, Token, Action, Threshold, Real_Val, N+1);
        S ->
            Len = size(Callback_Id),
            %?dline([Token, <<Len:8>> , Callback_Id, Action, ";", Threshold, ";", Real_Val, ";\r\n"]),
            Token_len = size(Token),
            case gen_tcp:send(S, <<Token_len:8, Token/binary, Len:8 , Callback_Id/binary, Action/binary, ";", Threshold, ";", Real_Val, ";\r\n">>) of
                ok ->
                    case gen_tcp:recv(S, 1024, 500) of
                        {ok, <<"success">>} -> 
                            ok;
                        _H ->
                            callback_tcp(Callback_Id, H, P, Token, Action, Threshold, Real_Val, N+1)
                    end;
                {error, Reason} ->
                    error_logger:error_msg("[~p, ~p] tcp send failed! ~p ~n", [?LINE, ?MODULE, Reason]),
                    callback_tcp(Callback_Id, H, P, Token, Action,Threshold, Real_Val, N+1)
            end
    end.

get_tcp_socket(H, P, N) ->
    L = string:tokens(H, ","),
    Host = case N > length(L) of
        true -> lists:nth(1, L);
        false -> lists:nth(N, L)
    end,
    Key = Host ++":"++ integer_to_list(P),
    case erlang:get(Key) of
        undefined ->
            case gen_tcp:connect(Host, P, [], 500) of
                {ok, Socket} ->
                    erlang:put(Key, Socket),
                    socket_list(Socket),
                    Socket;
                E -> E
            end;
        S -> S
    end.


fetch_rrd(Id, Range) ->
    End =  bnow_timer:now(),
    Start = End - Range * ?Interval,
    fetch_rrd(Id, Start, End, ?Interval).
fetch_rrd(Id, Start, End, Interval) ->
    R = bnow_rrd:fetch(Id, bnow_var:int(Start), bnow_var:int(End), bnow_var:int(Interval)),
    {ok, R}.

init_timer_loop() ->
    receive
        {init_timer, Time} ->
            %?dline(Time),
            init_timer(Time);
        _H ->  
        %?dline(_H),
        skip
    end,
    init_timer_loop().

send_init_timer(Time) ->
%whereis(enotify_timer),
    bnow_notify_timer ! {init_timer, Time}.

init_timer() ->
    ets:foldl(fun(#?notify{time=Time}, _AccIn) ->
            send_init_timer(Time)
        end, [], ?notify).
   
init_timer(Time) ->
    Key = "time:"++integer_to_list(Time),
    %?dline([init_timer, Key]),
    case erlang:get(Key) of
        undefined ->
            Rf = timer:apply_interval(Time * ?Interval * 1000, ?MODULE, filter, [Time]),
            erlang:put(Key, Rf);
        _ -> 
            skip
    end.

socket_list() ->
    %?dline([socket_list, erlang:get(socket_list)]),
    case erlang:get(socket_list) of
        undefined -> [];
        L -> L
    end.

socket_list(S) ->
    Socket_list = case erlang:get(socket_list) of
        undefined -> [];
        List -> List
    end,
    erlang:put(socket_list, [S | Socket_list]).

destructor() ->
    [gen_tcp:close(S) || S <-socket_list()].


callback_no_apply(L) ->
    error_logger:error_msg("callback(apply) failed, apply(module, func) empty!~n").

md5(Str)->md5(Str,16).

md5(Str,C) when is_list(Str), C == 16 ->
    md5_hex(Str);
md5(Str,C) when is_list(Str) ->
    md5(iolist_to_binary(Str),C);
md5(BinStr,C) ->
    <<N:128>> = erlang:md5(BinStr),
    [Md5] = io_lib:format( lists:flatten([$~,$.]++io_lib:format("~pb",[C])), [N]),
    Md5.

md5_hex(Str) ->
    Md5_list = binary_to_list(erlang:md5(Str)),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(List) ->
    lists:map(fun(X) -> int_to_hex(X) end, List).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).


valid( R) ->
    { QsVals , R1 } = cowboy_http_req:qs_vals(R)
    ,try
        Ac = <<(element(1,cowboy_http_req:qs_val( <<"ac">> , R1 )))/binary , "">>
        ,{ok,#bnow_cert{ secret = Secret}} = bnow_cert:fetch( element(1 , cowboy_http_req:qs_val( <<"api_secret_key">> , R1 )) )
        ,ApiTime = list_to_binary(binary_to_list(element(1,cowboy_http_req:qs_val( <<"api_time">> , R1 )) ))
        ,Now = bnow_timer:now()
        ,if 
            %ApiTime < Now - 180 ; ApiTime > Now+60 -> {false , <<"timeout">> , R1}
            true -> 
                QsVals1 = lists:keysort( 1 , proplists:delete( <<"ac">> , QsVals ) )
                ,SignSeed = bnow_cert:md5( <<(lists:foldl( fun({X,Y} , Acc)  -> <<Acc/binary , X/binary,Y/binary>>  end , <<>> , QsVals1 ))/binary ,Secret/binary >> )
                ,if
                    SignSeed =:= Ac -> {true , QsVals , R1}
                    ;true -> {false, [<<"Sign Error:">>, Ac], R1}
                end
        end
    catch _:_->
        {false , <<"valid error">> , R1 }
    end.
