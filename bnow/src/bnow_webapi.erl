-module(bnow_webapi).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-include_lib("cowboy/include/http.hrl").
-include("defined.hrl").

-export([init/3, handle/2, terminate/2]).

-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3,
    fetch_data/5
]).

-record(state, {api_host}).

init({tcp, http}, Req, _Opts) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {<<"websocket">>, _} ->
            {upgrade, protocol, cowboy_http_websocket};
        {_, R2} -> {ok, R2, #state{api_host = bnow_var:get_app_env(bnow, api_host)}}
    end.

handle(#http_req{path=[<<"api">>, <<"mgmt">>]}=R, S) ->
    {P0, R1} = cowboy_http_req:qs_vals(R),
    Keys0 = parse_query_items(P0, []),
    Rr = case bnow_cert:valid( R1, Keys0 ) of
        {true , _Keys , R2} ->
            case proplists:lookup(<<"m">>, P0) of
                {_, <<"update_app">>} ->
                    case proplists:get_value( <<"app">> , P0 ) of
                        App when is_binary( App ) -> 
                            {P1 , R3} = cowboy_http_req:body_qs(R2),
                            try
                                case proplists:get_value( <<"content">> , P1 )  of
                                    Content when is_binary( Content ) -> 
                                        {_A, _B} = xmerl_scan:string( binary_to_list( Content ),[{space,normalize}, {encoding, "latin1"}]),
                                        {ok, Appdir} = application:get_env(bnow, appdir),
                                        AppPath = filename:join(Appdir, binary_to_list(App)),
                                        file:make_dir(AppPath),
                                        XmlPath = filename:join(AppPath, "app.xml"),
                                        file:write_file( XmlPath , [Content] )
                                    ;_ -> ok
                                end,
                                bnow_app:update( list_to_atom( binary_to_list( App ) ) ),
                                {ok, R4} = cowboy_http_req:reply(200, [], [<<"succ">>], R3),
                                R4
                            catch _Err:_Why->
                                {ok, R5} = cowboy_http_req:reply(200, [], [<<"fail">>], R3)
                                ,R5
                            end
                        ;_ -> 
                            {ok, R3} = cowboy_http_req:reply(200, [], [<<"fail">>], R2)
                            ,R3
                    end;
                {_, <<"show_app_xml">>} ->
                    {_ , Appname} = proplists:lookup( <<"app">> , P0 ),
                    {ok, Appdir} = application:get_env(bnow, appdir),
                    AppPath = filename:join(Appdir, binary_to_list(Appname)),
                    XmlPath = filename:join(AppPath, "app.xml"),
                    case file:read_file( XmlPath ) of
                        {ok , Content } -> 
                            {ok, R3} = cowboy_http_req:reply(200, [], [Content], R2)
                            ,R3
                        ;_ -> 
                            {ok, R3} = cowboy_http_req:reply(200, [], [<<"fail">>], R2)
                            ,R3
                    end;
                {_, <<"start_app">>} ->
                    {_ , Appname} = proplists:lookup( <<"app">> , P0 ),
                    bnow_app:install( list_to_atom( binary_to_list( Appname ) ) ),
                    {ok, R3} = cowboy_http_req:reply(200, [], "succ", R2),
                    R3;
                {_, <<"stop_app">>} ->
                    {_ , Appname} = proplists:lookup( <<"app">> , P0 ),
                    bnow_app:uninstall( list_to_atom( binary_to_list( Appname ) ) ),
                    {ok, R3} = cowboy_http_req:reply(200, [], "succ", R2),
                    R3;
                _ ->
                    {ok, R3} = cowboy_http_req:reply(404, [], "Not exists api", R2),
                    R3
            end
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};
    

handle(#http_req{path=[<<"api">>, <<"data">>]}=R, S) ->
   % Keys = parse_query_items(P0, []),
   % {Type, Params} = case proplists:lookup(<<"type">>, P0) of
   %     {_, <<"collage">>} ->
   %         {collage, proplists:delete(<<"step">>, P0)};
   %     _ -> {list, P0}
   % end,
   % {Start, End} = time_range(Params, 3600),
   % {ok, L} = fetch_data(rrd, Keys, Start, End, Params),
   % {ok, R3} = render_data(rrd, Type, L, Keys, R1, {Start, End})
    
    %{<<"location">> , <<>>}
    %Keys0 = parse_query_items(P0, []),
    Rect = << (S#state.api_host)/binary ,  (R#http_req.raw_path)/binary ,"?", (R#http_req.raw_qs)/binary >>,
    {ok, R3} = cowboy_http_req:reply(302, [{<<"location">> , Rect}], [], R),
    {ok, R3, S};

handle(#http_req{path=[<<"api">>, <<"topn">>]}=R, S) ->
    {Params, R1} = cowboy_http_req:qs_vals(R),
    Keys0 = parse_query_items(Params, []),
    Rr = case bnow_cert:valid( R1, Keys0 ) of
        {true , Keys , R2} ->
            {Start, End} = time_range(Params, 24*3600),
            {ok, L} = fetch_data(topn,Keys, Start, End,Params),
            {ok, R3} = render_data(topn, L, Keys, R2, {Start, End}),
            R3
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};

handle(#http_req{path=[<<"api">>, <<"notify">>, <<"add_token">>]} = R , S) ->
    {P0, R1} = cowboy_http_req:qs_vals(R),
    %Keys0 = parse_query_items(P0, []),
    Rr = case bnow_notify:valid( R1) of
        {true , Keys , R2} ->
            Token_id = proplists:get_value(<<"token_id">>, P0),
            F = fun({<<"callback_", A/binary>>, V}, AccIn) ->
                    [{A, V} | AccIn];
                (_, AccIn) ->
                    AccIn
            end,
            Callback = lists:foldr(F, [], P0),
            bnow_notify:add_token(Token_id, Callback),
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], "ok", R1)
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};
handle(#http_req{path=[<<"api">>, <<"notify">>, <<"add_rule">>]} = R , S) ->
    {P0, R1} = cowboy_http_req:qs_vals(R),
    %Keys0 = parse_query_items(P0, []),
    Rr = case bnow_notify:valid( R1) of
        {true , Keys , R2} ->
            Key = proplists:get_value(<<"key">>, P0),
            Token = proplists:get_value(<<"token_id">>, P0),
            Interval = proplists:get_value(<<"interval">>, P0),
            F = fun({<<"filter_", A/binary>>, V}, AccIn) ->
                    [{A, V} | AccIn];
                (A, AccIn) ->
                    AccIn
            end,
            Filter = lists:foldr(F, [], P0),
            Callback_id = proplists:get_value(<<"callback_id">>, P0),
            Action = proplists:get_value(<<"action">>, P0),
            bnow_notify:add_rule(Key, Token, Interval, Filter, Callback_id, Action),
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], "ok", R1)
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};
handle(#http_req{path=[<<"api">>, <<"notify">>, <<"get_rule">>]} = R , S) ->
    {P0, R1} = cowboy_http_req:qs_vals(R),
    %Keys0 = parse_query_items(P0, []),
    Rr = case bnow_notify:valid( R1) of
        {true , Keys , R2} ->
            F = [{<<"token_id">>, proplists:get_value(<<"token_id">>, P0)}],
            F1 = case proplists:lookup(<<"key">>, R2) of
                {_, K} -> [{<<"key">>, K} | F];
                _ -> F
            end,
            {ok,Lst} = bnow_notify:list_rule(1, 20, F1),
            F = fun(X) ->
                [Recordt|Lsss] = tuple_to_list(X),
                [ "{ " ,bnow_var:bin(Recordt), [ [ ",",bnow_var:bin( Alsss )] || Alsss <- Lsss] , "}"]
            end,
            Lst1 = [ F(L) || L <- Lst ],
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], ["[",Lst1 , "]" ], R1)
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};
handle(#http_req{path=[<<"api">>, <<"notify">>, <<"del_rule">>]} = R , S) ->
    {P0, R1} = cowboy_http_req:qs_vals(R),
    %Keys0 = parse_query_items(P0, []),
    Rr = case bnow_notify:valid( R1) of
        {true , Keys , R2} ->
            bnow_notify:del_rule( proplists:get_value(<<"key">>, P0) , proplists:get_value(<<"token">>, P0) ),
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], "ok", R1)
        ;{false , Why , R2} ->
            {ok, R3} = cowboy_http_req:reply(200, [], [Why], R2),
            R3
        ;_ ->
            {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R1),
            R2
    end
    ,{ok, Rr, S};



handle(R, S) ->
    {ok, R2} = cowboy_http_req:reply(404, [], "Not exists api", R),
    {ok, R2, S}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    io:format("request ~p\n", [self()]),
    bnow_flow:link(),
    {ok, Req2, undefined, hibernate}.

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({data, Bin}, Req, State) when is_binary(Bin)->
    {reply, {text, Bin},  Req, State, hibernate };

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

binary_to_integer(Bin, Default)->
    try erlang:list_to_integer(binary_to_list(Bin)) catch _:_-> Default end.

csv_data(L, Items)->
    DateLabel = <<"Date">>,
    FirstLine = [DateLabel | [ N || {_, N}<-Items]],
    L2 = [ [integer_to_list(Time) | [ integer_to_list(D) || D<-Date]] || {Time, Date} <- L],
    join([join(Line,<<",">>) || Line <- [FirstLine | L2]], <<"\r\n">>).

js_data(Items)->
    js_data_item(Items, [<<"]">>]).

js_data_item([{Label, {_, Data}} | T], L)->
    DataList = case lists:foldl(fun({Time, {V, _}}, L0) ->
            [<<",\n">> | [ "     [", bnow_var:list(Time), "000,",
            bnow_var:list(V),
            "]" | L0]]
            end, [], Data) of
                [] -> [];
                [_| DataList0] -> DataList0
        end,
    L2 = [
        <<"\n {\"name\":\"">>, Label, <<"\",\n  \"data\": [\n">>,
        DataList,
        <<"]}\n">>
    | L],

    case T of
        [] -> [<<"[">> | L2];
        _ -> js_data_item(T, [<<",">> | L2])
    end;
js_data_item([], L)-> <<"[]">>.

render_data(rrd, collage, L,  _Items, R, {Begin0, End})->
    Content = ["[", join(lists:reverse(lists:map(fun({K, {Type, Vl}})->
                Begin = case lists:last(Vl) of
                        {T1, _} -> T1;
                        _ -> Begin0
                    end,
                {V, _} = lists:foldl(fun({_, {Vi,Vc}}, {Ri,Rc})->
                        bnow_rrd:merge_v(Vi, Vc, Ri, Rc, Type)
                    end,{0,0}, Vl),
                [<<"\n {\"name\":\"">>, K, <<"\", \"value\":">>
                    , bnow_var:bin(V), <<", \"func\":\"">>,
                    bnow_var:bin(Type) , <<"\", \"start\":">>,
                    bnow_var:bin(Begin),
                    <<", \"end\":">>,
                    bnow_var:bin(End),
                    <<"}">> ]
        end, L)), ","), "]"],
    output_json(R, Content);

render_data(rrd, list, L,  _Items, R, _)->
    Content = js_data(L),
    output_json(R, Content).

render_data(topn, L,  _Items, R, _)->
    Content = js_data(topn,L),
    output_json(R, Content).
js_data(topn,Items)->
    js_data_item(topn,Items, [<<"]">>]).

js_data_item(topn,[{Label, { Data , Start, End}} | T], L)->
    DataList = case lists:foldl(fun({Name, Value}, L0) ->
            [<<",\n">> | [ "     {\"name\":\"", bnow_var:list(Name), "\",",
            "\"value\":",bnow_var:list(Value),
            "}" | L0]]
            end, [], Data) of
                [] -> [];
                [_| DataList0] -> DataList0
        end,
    L2 = [
        <<"\n  {\"name\":\"">>, Label, <<"\",\n   \"time\":{\n    \"start\":">>
        ,bnow_var:list(Start),<<",\n    \"end\":">>
        ,bnow_var:list(End),<<"\n   },\n">>, <<"   \"data\": [\n">>,
        DataList,
        <<"]}\n">>
    | L],

    case T of
        [] -> [<<"[">> | L2];
        _ -> js_data_item(topn,T, [<<",">> | L2])
    end;
js_data_item(topn,[], L)-> <<"[]">>.

output_json(R, Content)->
    case cowboy_http_req:qs_val(<<"var">>, R) of
        {undefined, R1} ->
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], Content, R1);
        {Var, R1}->
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/javascript; charset=utf-8">>}], ["var ",Var,"=",Content,";"], R1)
    end.

parse_query_items([], L) -> lists:reverse(L);
parse_query_items([{<<"d[]">>, V} | T], L)->
    parse_query_items(T, [{V, V} | L]);
parse_query_items([{<<"d[", N/binary>>, V} | T], L)->
    case binary:last(N) of
        $] -> parse_query_items(T, [{V, binary:part(N, 0, byte_size(N)-1)} | L]);
        _ -> parse_query_items(T, L)
    end;
parse_query_items([{<<"d">>, V} | T], L)->
    parse_query_items(T, [{V, V} | L]);
parse_query_items([{_,_} | T], L)-> parse_query_items(T, L).

join([], _)->[];
join([H | []], _)-> H;
join([H | T], Separator)-> join(T, Separator, [H]).
join([], _, L)-> lists:reverse(L);
join([H | T], Separator, L)->join(T, Separator, [ H | [Separator | L ]]).

fetch_topn_data( Item , Size , Start , End ) ->
    S1 = Start div 3600
    ,E1 = End div 3600
    ,S = S1 * 3600
    ,E = E1 * 3600
    ,Istart = (Start - S) + 1
    ,Ilen = max(E1 - S1,1)
    ,PItemPrepared = ets:new( item_prepared ,[set , private] )
    ,PItemOrder = ets:new( item_order , [ordered_set,private] )
    ,merge_topn( bnow_topn:archives( Item , { Istart , Ilen } ) , PItemPrepared ,PItemOrder , Start , End )
    ,Ts = fetch_order_topn(  PItemOrder , Size , undefined , [] )
    ,ets:delete( PItemPrepared )
    ,ets:delete( PItemOrder )
    ,{ Ts , S , E + 3599 }.

%%-record(bnow_topn, {id, name, time, interval_hours, total=0, data=[], limit=240}).

merge_topn( [] , P , O,_S,_E ) ->
    ets:foldl( fun({K,V},_) ->
        ets:insert( O, {{V,K},K} )
    end , [] , P )
;merge_topn( [T|Ts],P,O,S,E ) ->
%% io:format(">> ~p, ~p\n", [T#bnow_topn.time , S]),
    if
        T#bnow_topn.time >= S , T#bnow_topn.time =< E ->
            lists:foreach( fun({K,V}) ->
                V1 = case ets:lookup( P , K ) of
                    [] -> V
                    ;[{_,Vo}] -> Vo + V
                end
                ,ets:insert( P , { K,V1 } )
            end ,  T#bnow_topn.data )
        ;true -> ok
    end
    ,merge_topn( Ts , P , O, S,E ).

fetch_order_topn( _O, 0,_K,R ) -> R
;fetch_order_topn( _O , _ , '$end_of_table' , R ) -> R
;fetch_order_topn( O , L , undefined , R ) ->
    case ets:last(O) of
        '$end_of_table' -> R;
        K1 ->
            {V, K} = K1,
            fetch_order_topn( O, L - 1 , K1 , [{K,V}|R] )
    end
;fetch_order_topn( O , L,Kp ,R) ->
    case ets:prev(O, Kp) of
        '$end_of_table' -> R;
        K1 ->
            {V, K} = K1,
            fetch_order_topn( O, L-1 , K1 , [{K,V}|R] )
    end.

time_range(Params, Length)->
    Now = bnow_timer:now(),
    End = case proplists:lookup(<<"end">>, Params) of
        none -> Now;
        {_, Endbin} -> binary_to_integer(Endbin, Now)
    end,

    Start = case proplists:lookup(<<"start">>, Params) of
        none -> End - Length;
        {_, StartBin} -> binary_to_integer(StartBin, End - 3600)
    end,
    {Start, End}.

fetch_data(topn, Items, Start, End, Params ) ->
    Size = case proplists:lookup(<<"size">>, Params) of
        none -> 10;
        {_, Sizebin} -> bnow_var:int(Sizebin, 10)
    end,
    {ok, lists:foldl(fun({Item, Label}, L)->
            [{Label, fetch_topn_data( Item,Size,Start,End )} | L]
        end, [], Items)};

fetch_data(rrd, Items, Start, End, Params)->
    Step = case proplists:lookup(<<"step">>, Params) of
        none -> 60;
        {_, StepBin} -> bnow_var:int(StepBin, 60)
    end,
    {ok, lists:foldl(fun({Item, Label}, L)->
            [{Label, bnow_rrd:fetch(Item, Start, End, Step)} | L]
        end, [], Items)}.

sign(SecretBin, Method, Path, Headers, GetParams, PostParams, md5) ->
    OrgSign = [
               bnow_var:list(SecretBin),
               bnow_var:list(Method),
               encode_path(Path),
               encode_header(Headers),
               encode_params(GetParams),
               encode_params(PostParams),
               bnow_var:list(SecretBin)
              ],
    SignString = list_to_binary(string:join(OrgSign, "&")),
    Mac = bnow_var:md5(SignString),
    {ok, list_to_binary(Mac)}.

encode_path(Path) ->
    uri_unicode:encode_uri_component(unicode:characters_to_list(Path, utf8)).

encode_header(Headers) ->
    Params = lists:filter(fun(L) -> case L of [] -> false; _ -> true end end,
           [case bnow_var:bin(Key) of
                <<"Authorization">> -> {bnow_var:bin(Key), bnow_var:bin(Value)};
                <<"X-Api-", _T/binary>> -> {bnow_var:bin(Key), bnow_var:bin(Value)};
                _ -> []
           end || {Key, Value} <- Headers]),
    encode_params(Params).

encode_params(Params) ->
    Orgstr = string:join(lists:sort([case Param of
        {Key, true} -> [unicode:characters_to_list(Key, utf8), "="];
        {Key, Value} -> [unicode:characters_to_list(Key, utf8), "=", unicode:characters_to_list(Value, utf8)]
    end || Param <- Params]), "&"),
    uri_unicode:encode_uri_component(lists:flatten(Orgstr)).
