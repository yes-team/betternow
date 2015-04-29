-module(bnow_modifier).
-export([modifier/3, init/0]).
-include("deps/iplib/include/defined.hrl").

-define( T(T), ( T - 48 ) ).
init()-> ok.

%modifier(capitalize, V, _) -> "world";
%modifier(date_format, V, _) -> "date_format";
modifier(replace, V, [A | [ B | _]]) ->
    binary:replace(V, bnow_var:bin(A), bnow_var:bin(B));
modifier(regex_replace, V, [A | [ B | _]]) ->
    re:replace(V, A, B, [global]);
%modifier(truncate, V, _) -> "regex_replace";
modifier(length, V, _) ->
    erlang:byte_size(V);

modifier(xpath, V, _) -> V;
modifier(jsonpath, PathK, _) -> 
    %io:format( "path ~p  ~p\n\n"  , [Path , JsonK]),
    [ Path , JsonK ] = binary:split( PathK , <<":">> )
    ,case get( {json , JsonK , Path } ) of
        undefined -> <<"atom:undefined">>
        ;R -> R
    end;
modifier(jsondecode , V, _ ) -> 
    P = mochijson2:decode(V)
    ,K = erlang:md5(V)
    ,json_parser(K,P,<<>>)
    ,put( {json , K } ,P )
    ,K;

modifier(date2time , <<Y1,Y2,Y3,Y4,"-",M1,M2,"-",D1,D2," ",H1,H2,":",N1,N2,":",S1,S2>> , _ ) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds({
        { 
            ?T(Y1)*1000 + ?T(Y2)*100 + ?T(Y3)*10 + ?T(Y4) 
            ,?T(M1)*10 + ?T(M2) 
            ,?T(D1) *10 + ?T(D2)
        }
        ,{ 
            ?T(H1)*10 + ?T(H2)
            ,?T(N1)*10 + ?T(N2)
            ,?T(S1)*10 + ?T(S2)
        }
    }),
    %%  1970 time 62167219200
    Seconds1 - 62167219200 - 8 * 3600;

%modifier(count_words, V, _) -> "regex_replace";

modifier(default, V, [Default|_]) ->
    case string:strip(bnow_var:list(V)) of
        [] -> Default;
        _ -> V
    end;
%modifier(from_charset, V, [Charset |_])-> V;

modifier( distince_data , V , _ ) ->
    case bnow_distinct:get( V) of
        {ok, _, Data} -> Data;
        _ -> <<"undefined">>
    end;

modifier(iplib, V, Opt)->
    Ret = case get({ip, V}) of
        undefined ->
            IpData = iplib:search(V),
            put({ip, V}, IpData),
            IpData;
        IpData -> IpData
    end,
    case Ret of
        {ok, IP} ->
            case Opt of
                ["country"|_] -> IP#iplib_ip_info.country;
                ["location"|_] -> IP#iplib_ip_info.location;
                ["network"|_] -> IP#iplib_ip_info.network;
                ["netname"|_] -> IP#iplib_ip_info.netname;
                _ -> IP#iplib_ip_info.province
            end;
        _ -> <<"unknow">>
    end;
    
modifier(lower, V, _) -> string:to_lower(bnow_var:list(V));
modifier(upper, V, _) -> string:to_upper(bnow_var:list(V));
modifier(strip, V, _) -> string:strip(bnow_var:list(V));
modifier(_, Value, _) -> Value.

n2b(V) when is_integer( V ) -> list_to_binary( integer_to_list( V ) )
;n2b(V) when is_float( V ) -> list_to_binary( float_to_list(V) )
;n2b(V) when is_binary( V ) -> V.

json_parser( K,P, Path ) ->
    case P of
        { struct,L } -> 
            lists:foldl( 
                fun(X, I) -> 
                    json_parser(K,X , <<Path/binary>>)
                    , I+1  
                end 
            , 1 ,  L )
        ;L when is_list(L)-> 
            lists:foldl( 
                fun(X, I) -> 
                    json_parser(K,X , <<Path/binary ,"/", (list_to_binary( integer_to_list( I ) ))/binary>>)
                    , I+1  
                end 
            , 1 ,  L )
        ;{ K1 , V } when is_number( V ) or is_binary(V) -> 
            put( { json,K,<<Path/binary,"/",(n2b(K1))/binary>> } ,n2b(V) )
        ;{ K1 , V } -> 
            json_parser(K,V , <<Path/binary  ,"/", (bnow_var:bin(K1))/binary>>)
        ;V -> 
            put( { json,K,Path } ,bnow_var:bin(V) )
    end.

