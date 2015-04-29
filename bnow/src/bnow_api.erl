-module(bnow_api).
-export([handle/3]).
-include("defined.hrl").

handle(rrd_fetch, _, [Name, StartTime, EndTime, Resolution])->
    {ok, bnow_rrd:fetch(Name, StartTime, EndTime, Resolution)};

handle(call, _, [M | [ F | A]])-> 
    apply(bnow_var:atom(M), bnow_var:atom(F), A);

handle(session_read, _, [Id])-> 
    case mnesia:dirty_read(bnow_session, Id) of
        [S|_] -> 
            case S#bnow_session.user of
                '' -> {ok, S#bnow_session{user=[]}};
                undefined -> {ok, S#bnow_session{user=[]}};
                Uname when is_atom(Uname) ->
                    case bnow_user:fetch(Uname) of
                        {ok, U} -> {ok, S#bnow_session{user = U}};
                        _ -> {ok, S#bnow_session{user=[]}}
                    end;
                _ ->
                    {ok, S#bnow_session{user=[]}}
            end;
        _-> {ok, #bnow_session{}}
    end;
handle(session_write, _, [Id, User, Data, TTL])->
    mnesia:dirty_write(#bnow_session{sess_id=Id, user=bnow_var:atom(User),
            data=Data, update_time=bnow_timer:now(), ttl=TTL}),
    {ok, true};

handle(exec, _, [ExprBin]) when is_binary(ExprBin)->
    Expr = binary_to_list(ExprBin),
	try eval(Expr) of Val -> {ok, Val} catch _E:X -> {error, X} end;
handle(version, _, _)-> {ok, bnow:version() };
handle(licence, _, [K])-> bnow_cert:fetch( K );

handle(get_secert, _, _)-> {ok, bnow:licence() };

handle(topn, _, [Name, Start, Len])-> 
    {ok, bnow_topn:archives(Name, {bnow_var:int(Start), bnow_var:int(Len)})};
handle(topn, _, [Name, Len])-> 
    {ok, bnow_topn:archives(Name, bnow_var:int(Len))};
handle(topn, _, [Name])-> 
    {ok, bnow_topn:archives(Name)};
handle(ping, _, _)-> {ok, pong };
handle(getconf, App, [Key])->
    {ok, bnow:getconf_php(App, Key)};
handle(setconf, App, [Key, Value, PHPVal])->
    bnow:setconf(App, Key, Value, PHPVal);
handle(getenv, _, [Env])->
    {ok , 
    case bnow_var:get_app_env( bnow, bnow_var:atom( Env ) ) of
        {K , S} -> { list_to_binary(K) , list_to_binary(S) };
        L -> bnow_var:bin(L)
    end };
handle(listconf, App, [Key, Value])->
    bnow:listconf(App);

handle(login, _, [User, LoginData])->
    bnow_user:login(User, LoginData);
handle(login, _, [User, Password, LoginData])->
    bnow_user:login(User, Password, LoginData);

handle(get_topn, App, [Key|Other]) when is_binary(Key)->
    handle(get_topn, App, [[Key]|Other]);

handle(get_topn, _, [Keys|[ Start| [End | O ]]])->
    Keys2 = [ {K, K} || K <- Keys],
    Opts = case O of [] -> []; [O0|_] -> O0 end,
    bnow_webapi:fetch_data(topn, Keys2, Start, End, Opts);

handle(get_rrd, App, [Key|Other]) when is_binary(Key)->
    handle(get_rrd, App , [[Key]|Other]);

handle(get_rrd, _, [Keys|[ Start| [End | O]]])->
    Keys2 = [ {K, K} || K <- Keys],
    Opts = case O of [] -> []; [O0|_] -> O0 end,
    {Type, Params} = case proplists:lookup(<<"type">>, Opts) of
        {_, <<"collage">>} ->
            {collage, proplists:delete(<<"step">>, Opts)};
        _ -> {list, Opts}
    end,
    {ok, Ret} = bnow_webapi:fetch_data(rrd, Keys2, Start, End, Params),
    case Type of
        list ->
            Ret2 = lists:foldl(fun({K,{_,L0}}, R0)->
                    L2 = lists:foldl(fun({Time, {J, _}}, L1)->
                            [ {Time, J} | L1]
                        end, [], L0),
                    [{K, {kv, L2}} | R0]
            end, [], Ret),
            {ok, {kv, Ret2}};
        collage ->
            Ret2 = lists:foldl(fun({K,{Mode,L0}}, R0)->
                Begin = case L0 of
                        [{T1, _} |_] -> T1;
                        _ -> Start
                    end,
                {V, _} = lists:foldl(fun({_, {Vi,Vc}}, {Ri,Rc})->
                        bnow_rrd:merge_v(Vi, Vc, Ri, Rc, Mode)
                    end,{0,0}, L0),
                [{K,{kv,[ 
                    {<<"value">>,V},
                    {<<"func">>, bnow_var:bin(bnow_app:atom_to_func(Mode))},
                    {<<"start">>,Begin},
                    {<<"end">>,End}
                  ]}} | R0]
            end, [], Ret),
            {ok, {kv, Ret2}}
    end;

handle(Cmd, App, Args)->
    {error, api_not_exists}.

eval(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.


