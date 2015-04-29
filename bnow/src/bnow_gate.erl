-module(bnow_gate).
-export([start_link/0, init/0, api_loop/1]).
-export([serialize/1]).

start_link() ->
    Pid = spawn_link(?MODULE,init,[]),
    register(?MODULE,Pid),
    {ok,Pid}.

init() ->
    io:format("starting ~p......\n", [?MODULE]),
    {ok, Port} = application:get_env(bnow, gate_port),
    {ok,ListenSock}=gen_tcp:listen(Port, [binary,{packet,4},
                                    {reuseaddr,true},
                                    {active,true}]),
    io:format("ok\n"),
    listen_loop(ListenSock).

listen_loop(ListenSock) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    Pid = spawn(?MODULE,api_loop,[Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    listen_loop(ListenSock).

api_loop(Socket) ->
    receive
        {tcp, Socket, <<Len:16,M:Len/binary,A/binary>>}->
            Method = list_to_atom(binary_to_list(M)),
            {App , Args} = case binary_to_term(A) of
                ["" | V] -> {undefined, V};
                [AppName | V] -> {AppName, V}
            end,
            try
                reply(Socket, bnow_api:handle(Method, App, Args))
            catch Err:Why->
                [ {EM, EF, _, Opts}| _ ] = erlang:get_stacktrace(),
                L = [ {Err, Why}, {model,EM} | [{method,EF} | Opts]],
                reply(Socket, {kv, L})
            end;
        {tcp_closed, Socket}-> ok
    end.

reply(Socket,Reply)->
    Send = serialize(Reply),
    ok = gen_tcp:send(Socket, Send),
    api_loop(Socket).

serialize({kv, Val}) when is_list(Val)->
    L2 = lists:foldl(fun({K,V}, L)->
                [ [serialize(K), serialize(V)] | L]
        end, [], Val),
    ["a:", integer_to_list(length(Val)), ":{", lists:reverse(L2), "}"];

serialize(Val)->
    try
	    if
    		is_float(Val)->
                Float = io_lib:format("~.3f",[Val]),
                [<<"d:">>, Float, <<";">>];

    		is_integer(Val)->
                I = integer_to_list(Val),
                [<<"i:">>, I, <<";">>];

    		is_list(Val)->
                {L2, _} = lists:foldl(fun(V, {L,N})->
                            {[ [serialize(N), serialize(V)] | L], N+1}
                    end, {[],0}, Val),
                ["a:", integer_to_list(length(Val)), ":{", lists:reverse(L2), "}"];

    		is_atom(Val)->
    			case Val of
    				true->
    					<<"b:1;">>;
    				false->
    					<<"b:0;">>;
    				_->
                        Str = atom_to_list(Val),
                        Size = integer_to_list(length(Str)),
                        [<<"s:">>, Size, <<":\"">>, Str, <<"\";">>]
    			end;

    		is_binary(Val)->
    			[<<"s:">>, integer_to_list(erlang:byte_size(Val)), <<":\"", Val/binary, "\";">>];

    		is_tuple(Val)->
                serialize(tuple_to_list(Val));

    		true ->
    			"b:0;"
    	end
    catch _:_Err->
        <<"b:0;">>
    end.
