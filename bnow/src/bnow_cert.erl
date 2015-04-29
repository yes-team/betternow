-module(bnow_cert).

-include("defined.hrl").
-export([new/1,delete/1,list/0,block/1,alive/1,update/2,fetch/1,valid/2, md5/1]).

new(Params) -> 
    Trans = fun() -> 
        {ok, Key} = bnow_var:guid(10, fun(K)-> mnesia:dirty_read(bnow_cert, K)=:=[] end),
        {ok, Secret} = bnow_var:guid(12)
        ,Acl = proplists:get_value( <<"acl">> , Params )
        ,mnesia:write( #bnow_cert{ 
            key = Key
            ,secret = Secret
            ,status = case proplists:get_value( <<"status">> , Params ) of
                <<"active">> -> <<"active">>
                ;_ -> <<"block">>
            end
            ,acl = if 
                is_list(Acl) -> Acl
                ;true -> []
            end
        } )
        ,Key
    end
    ,case mnesia:transaction(Trans) of
        {atomic,R } -> {ok, R}
        ;_ -> {ok, false}
    end.

delete(Key) ->
    {ok, mnesia:dirty_delete(bnow_cert, Key)}.

block(Key) ->
    Trans = fun() ->
        case mnesia:read( bnow_cert , Key ) of
            [] -> false
            ;[Cert] -> mnesia:write(Cert#bnow_cert{status = block})
        end
    end
    ,case mnesia:transaction(Trans) of
        {atomic,R } -> {ok, R}
        ;_ -> {ok, false}
    end.

alive(Key) ->
    Trans = fun() ->
        case mnesia:read( bnow_cert , Key ) of
            [] -> false
            ;[Cert] -> mnesia:write(Cert#bnow_cert{status = block})
        end
    end
    ,case mnesia:transaction(Trans) of
        {atomic,R } -> {ok, R}
        ;_ -> {ok, false}
    end.
    

list()->
    {atomic, L1} = mnesia:transaction(fun()->
            mnesia:foldl(fun(U, L)-> [U|L] end, [], bnow_cert)
        end),
    {ok, L1}.

update(Key , Params) ->
    Acl = proplists:get_value( <<"acl">> , Params )
    ,Trans = fun() ->
        case mnesia:read( bnow_cert , Key ) of
            [] -> false
            ;[Cert] -> mnesia:write(Cert#bnow_cert{
                status = proplists:get_value( <<"status">> , Params ) 
                ,acl = if 
                    is_list(Acl) -> Acl
                    ;true -> []
                end
            })
        end
    end
    ,case mnesia:transaction(Trans) of
        {atomic,R } -> {ok, R}
        ;_ -> {ok, false}
    end.

fetch(Key) ->
    case mnesia:dirty_read( bnow_cert , Key ) of
        [] -> {error, not_exists}
        ;[Cert] -> {ok, Cert}
    end.

md5(IoList)->
    <<N:128>> = erlang:md5(IoList),
    bnow_var:bin(lists:flatten(io_lib:format("~32.16.0b", [N]))).

do_token( R0, Keys ) -> 
    %% token = md5( "bnow" . bkey . secret . time )
    { QsVals , R1 } = cowboy_http_req:qs_vals(R0)
    ,{Bkey, _} = cowboy_http_req:qs_val( <<"appkey">>, R1 )
    ,{Time , _} = cowboy_http_req:qs_val( <<"time">>, R1 )
    ,case lists:all( fun( T ) when T =:= undefined -> false ; (_) -> true end , [ Bkey , Time ] ) of
        true -> 
            case mnesia:dirty_read( bnow_cert , Bkey ) of
                [] -> 
                    { false , <<"Key not exsit">> , R1 }
                ;[Cert] ->
                    Time1 = bnow_var:int(Time)
                    ,CertTimeout = case application:get_env( bnow, api_cert_timeout ) of
                        {ok,Ct} -> Ct
                        ;_ -> 86400
                    end
                    ,Now = bnow_timer:now()
                    ,if
                        Time1 < Now - CertTimeout -> 
                        %io:format("timeout: ~p, ~p, ~p\n", [Now, Time1, CertTimeout]),
                            { false , <<"Timeout">> , R1}
                        ;true -> 
                            try
                                {true, lists:foldr(fun({K, V}, L)->
                                    case split_item(K) of
                                        { _ , undefined } -> throw({sign_error, K});
                                        { Kreal , Token } ->
                                            case bnow_var:bin(md5(iolist_to_binary(
                                                [ Kreal, Bkey , Time , Cert#bnow_cert.secret ]))) of
                                                Token when K=:=V -> [{Kreal, Kreal} | L];
                                                Token -> [{Kreal, V} | L];
                                                _Token ->
%io:format(":: ~s\n", [[ Kreal, Bkey , Time , Cert#bnow_cert.secret ]]),
%io:format("~p\n~p\n", [ _Token, Token]),
                                                    throw({sign_error, Kreal})
                                            end
                                    end
                                end
                                ,[], Keys), R1}
                        catch _:{sign_error, ErrK} ->
                            {false, [<<"Sign Error:">>, ErrK], R1}
                        end
                    end
                end
        ;_ -> { false, <<"Invalid arguments">> , R1}
    end
    .

valid( R, [] ) -> {true, [] , R};
valid( R, Keys ) ->
    F = fun({X,Y}) -> 
        {wipe_sign(X),wipe_sign(Y)}
    end
    ,case application:get_env( bnow, api_need_verify ) of
        { ok,true } -> 
            { Sid , R0 } = cowboy_http_req:cookie(<<"sid">>,R)
            ,case mnesia:dirty_read( bnow_session, Sid ) of
                [Sess | _] when Sess#bnow_session.user =/= '' -> 
                    { true , [ F(K) || K <- Keys] , R0};
                _ ->
                    do_token(  R0, Keys )
            end
        ;_ -> {true,[F(K) || K <- Keys] ,R}
    end.

wipe_sign(X)->
    case binary:matches( X , <<".">> ) of
        [] -> X
        ;L -> 
            { Pos , _ } = lists:last(L)
            ,Sign = binary:part( X , Pos+1, byte_size(X)-Pos-1 )
            ,case get_sign(Sign) of
                false -> X
                ;{ok, _X1} -> 
                    bianry:part( X , 0 , Pos )
            end
    end.

split_item( X ) ->
    case binary:matches( X , <<".">> ) of
        [] -> {X,undefined}
        ;L -> 
            { Pos , _ } = lists:last(L)
            ,Sign = binary:part( X , Pos+1, byte_size(X)-Pos-1 )
            ,case get_sign(Sign) of
                false -> {X,undefined}
                ;{ok, Sign1} -> 
                    {binary:part( X , 0 , Pos ) ,Sign1}
            end
    end.

get_sign(X) ->
    case byte_size(X) of
        32 -> {ok , X}
        ;34 -> 
            case { binary:first(X) , binary:last(X) } of
                { $[ , $] } -> binary:part( X , 1 , 32 )
                ;_ -> false
            end
        ;_ -> false
    end.
