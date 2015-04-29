-module(bnow_webflow).  
-behaviour(cowboy_http_handler).  
-behaviour(cowboy_http_websocket_handler).  
-include_lib("cowboy/include/http.hrl").

-export([init/3, handle/2, terminate/2]).  
-record(state, {channels}).

-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).

init({tcp, http}, Req, _Opts) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {<<"websocket">>, _} ->
            {upgrade, protocol, cowboy_http_websocket};
        {_, R2} -> {ok, R2, #state{}}
    end.

bad_request(R, S)->
    {ok, R2} = cowboy_http_req:reply(404, [{<<"Content-Type">>, <<"text/html">>}], 
        "<h1>flow 404 Not found</h1><hr />", R),
    {ok, R2, S}.

handle(R, S) ->
    case cowboy_http_req:qs_val(<<"callback">>, R) of
        {undefined, R1} -> bad_request(R1, S);
        {Callback, R1} ->
            case cowboy_http_req:qs_val(<<"channel">>, R1) of
                {undefined, R2} -> bad_request(R2, S);
                {Ch, R2}->
                    case binary:split(Ch, <<",">>, [global]) of
                        [] ->  bad_request(R2, S);
                        L ->
                            bnow_flow:link(),
                            lists:foreach(fun(Channel)->
                                bnow_flow:join(Channel)
                                end, L),
                            {ok, R3} = cowboy_http_req:chunked_reply(200,
                                [{<<"Content-Type">>, <<"text/html">>}], R2),
                            inet:setopts(R1#http_req.socket, [{active, once}]),
                            Body =
                            <<"<html><header><title>live</title></header><body><h1>live api</h1>",0:1024,"</body>
                            <script>callback=", Callback/binary, ";callback({'type':'ping'});</script>">>,
                            cowboy_http_req:chunk(Body, R3),
                            flow_loop(R3),
                            {ok, R3, S}
                    end
            end
    end.

flow_loop(R)->
    receive
        {data, Data} ->
            cowboy_http_req:chunk([<<"<script>try{ callback(">>, Data,
                <<"); }catch(err){ if(console) if(console.info) console.info(err) }</script>">>], R),
            flow_loop(R);
        {tcp_closed, Sock} when Sock=:= R#http_req.socket ->
            ok;
        {tcp, Sock, _} when Sock=:= R#http_req.socket ->
            inet:setopts(Sock, [{active, once}]),
            flow_loop(R);
        _ -> 
            flow_loop(R)
    after timer:seconds(20) ->
            cowboy_http_req:chunk(<<"<script>callback({'type':'ping'});</script>">>, R),
        flow_loop(R)
    end.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    bnow_flow:link(),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Json}, Req, State) -> 
    case mochijson2:decode(Json) of
        {struct, Msg} ->
            case proplists:lookup(<<"type">>, Msg) of
                {_, <<"join">>}->
                    case proplists:lookup(<<"data">>, Msg) of
                        {_, Channel} -> 
                            bnow_flow:join(Channel),
                            {reply, {text, <<"{\"type\":\"joined\", \"channel\":\"", Channel/binary ,"\"}">>}, Req, State, hibernate};
                        _ ->
                            {reply, {text, <<"{\"type\":\"error\", \"data\": \"channel?\"}}">>}, Req, State, hibernate}
                    end;
                {_, <<"leave">>}->
                    case proplists:lookup(<<"data">>, Msg) of
                        {_, Channel} -> 
                            bnow_flow:leave(Channel),
                            {reply, {text, <<"{\"type\":\"left\", \"channel\":\"", Channel/binary ,"\"}">>}, Req, State, hibernate};
                        _ ->
                            {reply, {text, <<"{\"type\":\"error\", \"data\": \"channel?\"}">>}, Req, State, hibernate}
                    end;
                {_, <<"tmp_trigger_start">>}->
                    case proplists:lookup(<<"data">>, Msg) of
                        {_, Channel} -> 
                            bnow_flow:start_filter(Channel),
                            {reply, {text, <<"{\"type\":\"trigger_started\"}">>}, Req, State, hibernate};
                        _ ->
                            {reply, {text, <<"{\"type\":\"error\", \"data\":
                                    \"trigger_started_filed\"}}">>}, Req, State, hibernate}
                    end;
                {_, <<"tmp_trigger_stop">>}->
                    bnow_flow:stop_filter(),
                    {reply, {text, <<"{\"type\":\"trigger_stopped\"}">>}, Req, State, hibernate};
                _ ->
                    {reply,
                        {text, << "{\"type\":\"error\", \"data\":\"bad action type\"}" >>},  
                        Req, State, hibernate  
                    }
            end;
        _ ->
            {reply,
                {text, << "{\"type\":\"error\", \"data\":\"bad action type\"}" >>},  
                Req, State, hibernate
            }
    end;

websocket_handle(_Any, Req, State) ->  
    {ok, Req, State}.  

websocket_info({data, Bin}, Req, State)->
    {reply, {text, Bin},  Req, State, hibernate };
    
websocket_info(_Info, Req, State) ->  
    {ok, Req, State, hibernate}.  
  
websocket_terminate(_Reason, _Req, _State) ->  
    ok.
