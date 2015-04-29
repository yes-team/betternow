-module(bnow_app).
-include("defined.hrl").
-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-record(state, {filter_table}).

-define(fun2ms(F), ets:fun2ms(F)).
%-define(fun2ms(F), ets:match_spec_compile(ets:fun2ms(F))).

%-record(assert, {left=[], right=[], type=is, match_spec, sets=[], not_is=false}).
%-compile(export_all).
-export([start_link/0]).
-export([install/1, uninstall/1, list/0, update/1, info/1, status/1]).
-export([dir/0, dir/1, dir/2, parse_xml/2, compile_filter/1, match_assert/2,
        compile_filter_runtime/1, apply_set/2, load_beams/1, 
        unload_beams/1, atom_to_func/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("starting ~p......\n", [?MODULE]),
    Table = ets:new(filters, [public, bag, named_table]),
    mnesia:transaction(fun()->
        mnesia:foldl(fun(F, _)->
            load_filter(F)
        end, [], bnow_filter)
    end),
    bnow_app_edoc:build(),
    io:format("ok\n"),
    {ok, #state{filter_table = Table}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


compile_filter_runtime(F)->
    D2 = lists:foldl(fun(A, D)->
            if 
                is_record(A, bnow_filter) ->
                    [compile_filter_runtime(A) | D];
                true ->
                    [ A | D ]
            end
        end, [], F#bnow_filter.data),
    F#bnow_filter{data = D2}.

load_filter(F)->
    F2 = compile_filter_runtime(F),
    ets:insert(filters, {F2#bnow_filter.class, F2}),
    bnow_app_edoc:build(),
    true.

unload_filter(F)->
    F2 = compile_filter_runtime(F),
    ets:delete_object(filters, {F2#bnow_filter.class, F2}),
    bnow_app_edoc:build(),
    true.

list()->
    lists:foldr(fun(Path, L)->
            case filename:basename(Path) of
                [$.|_] -> L;
                Appid_l ->
                    Appid = list_to_atom(Appid_l),
                    case info(Appid) of
                        {ok, Appinf} -> [Appinf | L];
                        _ -> L
                    end
            end
        end, [], filelib:wildcard(dir("*"))).

dir()->
    {ok, Appdir} = application:get_env(bnow, appdir),
    Appdir.

dir(App)->
    filename:join(dir(), App).
    
dir(App, Path)->
    filename:join([dir(), App, Path]).
    
info_xml(Appid) when is_atom(Appid)->
    Xml = filename:join(dir(Appid), "app.xml"),
    case filelib:is_file(Xml) of
        true -> load_app_xml(Appid, Xml);
        _ -> {error, no_app_xml}
    end.
            
info(Appid) when is_atom(Appid)->
    Xml = filename:join(dir(Appid), "app.xml"),
    case filelib:is_file(Xml) of
        true ->
            case load_app_xml(Appid, Xml) of
                {ok, A} -> {ok, A#bnow_app{status = status(A)}};
                Err -> Err
            end;
        _ -> {error, no_app_xml}
    end.
    
status(App) when is_atom(App)->
    case info_xml(App) of
        {ok, Appinf} -> status(Appinf);
        _ -> destroyed
    end;
status(#bnow_app{config_hash=Ver} = App) when is_record(App, bnow_app)->
    case mnesia:transaction(fun()->
            mnesia:read(bnow_app, App#bnow_app.app)
        end) of
        {atomic, []} -> disabled;
        {atomic, [#bnow_app{config_hash=Ver} | _]} -> running;
        {atomic, [A2 | _]} when is_record(A2, bnow_app) -> running_outdated
    end.

install(App) when is_atom(App)-> {ok, Appinf} = info_xml(App), install(Appinf);
install(App) when is_record(App, bnow_app)->
    %io:format("~p\n\n", [App]),
%%    bnow_app_edoc:do( App),

%    active_bundle( rrd, App#bnow_app.filter ),
%    active_bundle( topn, App#bnow_app.filter ),
    mnesia:transaction(fun()->
        write(App),
        
        lists:foreach(fun(F)->
                write(F),
                load_filter(F)
            end, App#bnow_app.filter),
            
        lists:foreach(fun(M)->
                write(M)
            end, App#bnow_app.menu),

        load_beams(App#bnow_app.app),
            
        lists:foreach(fun(M)->
                write(M),
                {ok, _Pid} = bnow_plugin_sup:load(M)
            end, App#bnow_app.plugin)
    end).

%active_bundle( Type , Filter ) ->
%    Action = get_action( Filter , { Type , [] } ),
%    lists:foreach( fun(X) -> 
%        {update_args , K , _V , T } = X#bnow_action.args,
%        bnow_bundle:active( Type , K , T )
%    end , Action ).
%
%get_action(F , R) when is_list(F) ->
%    lists:foldl( fun get_action/2 , R , F )
%;get_action(F , R) when is_record( F , bnow_filter ) -> 
%    get_action(F#bnow_filter.data , R)
%;get_action(F , {T , R } ) when F#bnow_action.action =:= T ->
%    [F|R]
%;get_action(_ , {_T , R}) ->
%    R.
    

ensure_appname([$/|_] = N, App0)-> N;
ensure_appname(N, App0)->
     App = bnow_var:list(App0),
     lists:flatten(["/", App, "/", N]).

compile_action(#bnow_action{action=update} = A, F)->
    Arg = A#bnow_action.args,
    V0 = ensure_appname(Arg#update_args.name, F#bnow_filter.app),
    CName = magic:compile_string(V0),
    CValue = magic:compile_expr(Arg#update_args.value),
    A#bnow_action{compiled_args = {CName,CValue}, args=(Arg#update_args{name=V0})};

compile_action(#bnow_action{action=flow} = A, _F)->
    CName = magic:compile_string(A#bnow_action.args),
    A#bnow_action{compiled_args = CName};

compile_action(#bnow_action{action=topn} = A, F)->
    Arg = A#bnow_action.args,
    V0 = ensure_appname(Arg#topn_cmd.name, F#bnow_filter.app),
    CName = magic:compile_string(V0),
    CItem = magic:compile_string(Arg#topn_cmd.item),
    CValue = magic:compile_expr(Arg#topn_cmd.value),
    A#bnow_action{compiled_args = Arg#topn_cmd{
                name=CName, 
                item=CItem, 
                value=CValue,
                limit = bnow_var:int(Arg#topn_cmd.limit, 240),
                hours = bnow_var:int(Arg#topn_cmd.hours, 1),
                size = bnow_var:int(Arg#topn_cmd.size, 10)
            },
            args = Arg#topn_cmd{name=V0}};
compile_action(A, _) -> A.

write(I)-> mnesia:write(I).
update(App)->
    case status(App) of
        running_outdated ->
            {atomic,ok} = uninstall(App),
            {atomic,ok} = install(App),
            {ok, updated};
        _ -> {ok, already_new_version }
    end.
        
uninstall(App) when is_atom(App)->
    mnesia:transaction(fun()->
        mnesia:delete({bnow_app, App}),
        mnesia:delete({bnow_menu,App}),
        
        lists:foreach(fun(M)->
            bnow_plugin_sup:unload(M),
            mnesia:delete_object(M)
        end, mnesia:read(bnow_plugin, App)),
        
        lists:foreach(fun(F)->
            unload_filter(F),
            mnesia:delete({bnow_filter, F#bnow_filter.id})
        end, mnesia:match_object(#bnow_filter{app=App,_='_'})),
        unload_beams(App),
        ok
    end).

load_beams(A) when is_atom(A)->
    code:add_patha(dir(A, "plugin")),
    lists:foreach(fun(F)->
            M = list_to_atom(filename:rootname(filename:basename(F))),
            code:soft_purge(M),
            code:load_abs(filename:rootname(F))
    end, filelib:wildcard(dir(A, "plugin/*.beam"))).

unload_beams(A) when is_atom(A)->
    code:del_path(dir(A, "plugin")),
    lists:foreach(fun(F)->
            M = list_to_atom(filename:rootname(filename:basename(F))),
            code:soft_purge(M),
            code:delete(M)
    end, filelib:wildcard(dir(A, "plugin/*.beam"))).
%-----------------------------------------------------------------------    

parse_xml(Appid, String)->
    try
        {A, B} = xmerl_scan:string(String,[{space,normalize}, {encoding, "latin1"}]),
        parse_xml_data(Appid, A, B)
    catch _Err:_Why->
        io:format("~p:~p ~p\n", [_Err, _Why, erlang:get_stacktrace()]),
        {error, badxml}
    end.

parse_xml_data(Appid, A, B)->
    <<N:128>> = erlang:md5(term_to_binary({A,B})),
    Ver = list_to_binary(lists:flatten(io_lib:format("~-32.16.0B",[N]))),
    {#xmlElement{name=app}=App, _Misc} = {A, B},
    Appinf = lists:foldr(fun read_app_xml/2, #bnow_app{app=Appid, config_hash=Ver}, App#xmlElement.content),
    {ok, Appinf}.

load_app_xml(Appid, Path)->
    try
        {A, B} = xmerl_scan:file(Path,[{space,normalize}, {encoding, "latin1"}]),
        parse_xml_data(Appid, A, B)
    catch _Err:_Why->
        io:format("~p:~p ~p\n", [_Err, _Why, erlang:get_stacktrace()]),
        {error, badxml}
    end.

read_app_xml(#xmlElement{name=menu, attributes=Attrs}, Appinf)->
    M = lists:foldl(fun read_menu_xml_attr/2, #bnow_menu{app=Appinf#bnow_app.app}, Attrs),
    Appinf#bnow_app{menu = [M | Appinf#bnow_app.menu]};
    
%read_app_xml(#xmlElement{name=trigger, attributes=Attrs}, Appinf)->
%    M = lists:foldl(fun read_trigger_xml_attr/2, {undefined, undefined}, Attrs),
%    Appinf#bnow_app{trigger = [M | Appinf#bnow_app.trigger]};
%    
%read_app_xml(#xmlElement{name=action, attributes=Attrs}, Appinf)->
%    M = lists:foldl(fun read_action_xml_attr/2, #bnow_action{app=Appinf#bnow_app.app}, Attrs),
%    Appinf#bnow_app{action = [M | Appinf#bnow_app.action]};
    
read_app_xml(#xmlElement{name=name, content=[#xmlText{value=V} | _]}, Appinf)->
    Appinf#bnow_app{name = list_to_binary(V)};
    
read_app_xml(#xmlElement{name=plugin, attributes=Attrs}, Appinf)->
    M = lists:foldl(fun read_plugin_xml_attr/2, #bnow_plugin{app=Appinf#bnow_app.app}, Attrs),
    Appinf#bnow_app{plugin = [M | Appinf#bnow_app.plugin]};
    
read_app_xml(#xmlElement{name=filter, attributes=Attrs, content=Content}, Appinf)->
    F = new_filter(#bnow_filter{app=Appinf#bnow_app.app}, Attrs, Content),
    Appinf#bnow_app{filter = [F | Appinf#bnow_app.filter]};

read_app_xml(T, Appinf) when is_record(T, xmlText) -> Appinf;

read_app_xml(#xmlElement{name=K, content=[#xmlText{value=V} | _]}, Appinf)->
    Appinf#bnow_app{info = [{K,list_to_binary(V)} | Appinf#bnow_app.info]};

read_app_xml(Item , Appinf) when is_record(Item, xmlComment) -> Appinf;

read_app_xml(_Item, Appinf)->
    Appinf.

new_filter(F, Attrs, Content)->
    {ok, ID} = bnow_var:guid(10, fun(K)->
                    case mnesia:dirty_read(bnow_filter, F) of
                        [] -> true;
                        _ -> false
                    end
                end, 5),
    F2 = lists:foldl(fun read_filter_xml_attr/2, F#bnow_filter{id=ID}, Attrs),
    F3 = lists:foldl(fun read_filter_xml/2, F2, Content),
    F4 = compile_filter(F3),
    F4#bnow_filter{data = F4#bnow_filter.data }.

% -record(bnow_filter, {id, app, name, class, args, compiled_args, bind_actions=[]}).
%-----------------------------------------------------------------------    
read_filter_xml_attr(#xmlAttribute{name=class, value=V} = Atrr, M)->
    M#bnow_filter{class=bnow_var:bin(V)};
read_filter_xml_attr(_,M)->M.

read_filter_xml(#xmlElement{name=assert, attributes=Attrs, content=Content}, Filter)->
    Props = lists:foldl(fun read_xml_attr/2, [], Attrs),
    Sets = lists:foldl(fun read_filter_set_xml/2, [], Content),
    Filter#bnow_filter{data = [ {assert, Props, Sets} | Filter#bnow_filter.data]};

read_filter_xml(#xmlElement{name=set, attributes=Attrs, content=Content}, Filter)->
    S = lists:foldl(fun read_filter_set_xml_attr/2, #bnow_set{}, Attrs),
    Filter#bnow_filter{data = [S | Filter#bnow_filter.data]};

read_filter_xml(#xmlElement{name=filter, attributes=Attrs, content=Content}, Filter)->
    F = new_filter(#bnow_filter{app=Filter#bnow_filter.app,
            class=Filter#bnow_filter.class}, Attrs, Content),
    Filter#bnow_filter{data = [F | Filter#bnow_filter.data]};

read_filter_xml(#xmlElement{name=action, attributes=Attrs, content=Content}, Filter)->
    Attrs1 = lists:sort( fun
        (_X,Y) when Y#xmlAttribute.name =:= type ->  false
        ;(_,_) -> true
        end , Attrs ),
    A = lists:foldl(fun read_action_xml_attr/2, #bnow_action{}, Attrs1),
    Filter#bnow_filter{data = [A | Filter#bnow_filter.data]};

read_filter_xml(_, M)->M.

read_xml_attr(#xmlAttribute{name=K, value=V}, D)->
    [{K,V} | D].

read_filter_set_xml(#xmlElement{name=set, attributes=Attrs}, L)->
    M = lists:foldl(fun read_filter_set_xml_attr/2, #bnow_set{}, Attrs),
    [M | L];
read_filter_set_xml(_, M)->M.

read_filter_set_xml_attr(#xmlAttribute{name=var, value=D} = Atrr, S)->
    S#bnow_set{var=D};
read_filter_set_xml_attr(#xmlAttribute{name=value, value=D} = Atrr, S)->
    S#bnow_set{value=D};
read_filter_set_xml_attr(#xmlAttribute{name=modifier, value=D} = Atrr, S)->
    S#bnow_set{modifier=D};
read_filter_set_xml_attr(#xmlAttribute{name=type, value=D} = Atrr, S)->
    S#bnow_set{type=list_to_atom(D)};
read_filter_set_xml_attr(_,M)->M.

%-----------------------------------------------------------------------
read_action_xml_attr(#xmlAttribute{name=type, value="flow"} = Atrr, M)->
    M#bnow_action{action=flow};
read_action_xml_attr(#xmlAttribute{name=type, value="update"} = Atrr, M)->
    M#bnow_action{action=update, args=#update_args{}};
read_action_xml_attr(#xmlAttribute{name=type, value="plugin"} = Atrr, M)->
    M#bnow_action{action=plugin};
read_action_xml_attr(#xmlAttribute{name=type, value="top-n"} = Atrr, M)->
    M#bnow_action{action=topn, args=#topn_cmd{} };

read_action_xml_attr(#xmlAttribute{name=item, value=V} = Atrr, #bnow_action{action=flow}=M)->
    M#bnow_action{args=V};
read_action_xml_attr(#xmlAttribute{name=item, value=V} = Atrr, #bnow_action{action=plugin}=M)->
    M#bnow_action{args=list_to_atom(V)};

% item="/http-mon/request_host" record="${host}" value="1" hours="24" archive_limit="10"
read_action_xml_attr(#xmlAttribute{name=item, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{name=V} };
read_action_xml_attr(#xmlAttribute{name=record, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{item=V} };
read_action_xml_attr(#xmlAttribute{name=value, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{value=V} };
read_action_xml_attr(#xmlAttribute{name=hours, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{hours=V} };

read_action_xml_attr(#xmlAttribute{name=list_length, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{size=V} };
read_action_xml_attr(#xmlAttribute{name=archive_limit, value=V} = Atrr, 
                    #bnow_action{args=Args, action=topn}=M) when is_record(Args, topn_cmd)->
    M#bnow_action{args=Args#topn_cmd{limit=V} };

read_action_xml_attr(#xmlAttribute{name=function, value=V} = Atrr, 
                    #bnow_action{args=Args, action=update}=M) when is_record(Args, update_args)->
    M#bnow_action{args=Args#update_args{function=func_to_atom(V)} };
read_action_xml_attr(#xmlAttribute{name=value, value=V} = Atrr, 
                    #bnow_action{args=Args, action=update}=M) when is_record(Args, update_args)->
    M#bnow_action{args=Args#update_args{value=V} };
read_action_xml_attr(#xmlAttribute{name=item, value=V} = Atrr, 
                    #bnow_action{args=Args, action=update}=M) when is_record(Args, update_args)->
    M#bnow_action{args=Args#update_args{name=V} };
read_action_xml_attr(_,M)->M.

%-----------------------------------------------------------------------    
read_trigger_xml_attr(#xmlAttribute{name=filter, value=V} = Atrr, {F, A})->
    {list_to_atom(V), A};
read_trigger_xml_attr(#xmlAttribute{name=action, value=V} = Atrr, {F, A})->
    {F, list_to_atom(V)};
read_trigger_xml_attr(_,M)->M.

%-----------------------------------------------------------------------    
read_plugin_xml_attr(#xmlAttribute{name=module, value=V} = Atrr, M)->
    M2 = M#bnow_plugin{module=list_to_atom(V)};
read_plugin_xml_attr(_,M)->M.

%-----------------------------------------------------------------------
read_menu_xml_attr(#xmlAttribute{name=text, value=V} = Atrr, M)->
    M#bnow_menu{text=list_to_binary(V)};
read_menu_xml_attr(#xmlAttribute{name=desc, value=V} = Atrr, M)->
    M#bnow_menu{desc=list_to_binary(V)};
read_menu_xml_attr(#xmlAttribute{name=link, value=V} = Atrr, M)->
    M#bnow_menu{link=list_to_binary(V)};
read_menu_xml_attr(_,M)->M.

func_to_atom(Str)->func_to_atom_lower(string:to_lower(Str)).
func_to_atom_lower("avg")-> ?T_AVG;
func_to_atom_lower("sum")-> ?T_SUM;
func_to_atom_lower("max")-> ?T_MAX;
func_to_atom_lower("min")-> ?T_MIN;
func_to_atom_lower(_)-> ?T_SUM.

atom_to_func(?T_AVG) -> "avg";
atom_to_func(?T_SUM) -> "sum";
atom_to_func(?T_MAX) -> "max";
atom_to_func(?T_MIN) -> "min";
atom_to_func(_)-> "sum".

compile_filter(F) when is_record(F, bnow_filter) ->
    A2 = lists:map(fun(Arg)->
                case Arg of
                    {assert, Props, Sets} ->
                        S2 = lists:map(fun compile_set/1, Sets),

                        {Datom, NotIs} = lists:foldr(fun(V, {L, B})-> 
                                    if 
                                        V=:="not" -> {L,not B}; 
                                        true -> {list_to_atom(V), B} 
                                    end 
                            end, {is, false}, string:tokens(proplists:get_value(test, Props, "is"), " ")),

                        Assert = compile_assert(Datom, Props, F),
                        Assert#bnow_assert{sets=S2, not_is=NotIs};
                    A when is_record(A, bnow_set) -> compile_set(A);
                    A when is_record(A, bnow_action) -> compile_action(A, F);
                    A -> A
                end
            end,F#bnow_filter.data),
    F#bnow_filter{data=A2}.

string_assert(L)->
    K1 = magic:compile_string(proplists:get_value(left, L, "")),
    V1 = magic:compile_string(proplists:get_value(right, L, "")),
    #bnow_assert{left=K1, right=V1, mode=string}.

number_assert(L)->
    K1 = magic:compile_expr(proplists:get_value(left, L, 0)),
    V1 = magic:compile_expr(proplists:get_value(right, L, 0)),
    #bnow_assert{left=K1, right=V1, mode=number}.

compile_assert(is, Props, _)->
    A = string_assert(Props),
    A#bnow_assert{type=match_spec,
        test=?fun2ms(fun({M,M}) -> true end)};

compile_assert(isset, Props, _)->
    #bnow_assert{type=isset, 
	left=bnow_var:bin(proplists:get_value(left, Props, "undefined"))};

compile_assert(has, Props, _)->
    A = string_assert(Props),
    case A#bnow_assert.right of
        [{1, Bin}] ->
            A#bnow_assert{type=bm, test=Bin};
        _-> A#bnow_assert{type=has}
    end;

compile_assert(match, Props, _)->
    K1 = magic:compile_string(proplists:get_value(left, Props)),
    Right = proplists:get_value(right, Props),
    #bnow_assert{left=K1, right=Right, mode=string, type=re };

compile_assert('>', Props, _)->
    A = number_assert(Props),
    A#bnow_assert{type=match_spec, test=?fun2ms(fun({M,N}) when M>N-> true end)};

compile_assert('>=', Props, _)->
    A = number_assert(Props),
    A#bnow_assert{type=match_spec, test=?fun2ms(fun({M,N}) when M>=N-> true end)};

compile_assert('<', Props, _)->
    A = number_assert(Props),
    A#bnow_assert{type=match_spec, test=?fun2ms(fun({M,N}) when M<N-> true end)};

compile_assert('<=', Props, _)->
    A = number_assert(Props),
    A#bnow_assert{type=match_spec, test=?fun2ms(fun({M,N}) when M=<N-> true end)};

compile_assert('=', Props, _)->
    A = number_assert(Props),
    A#bnow_assert{type=match_spec, test=?fun2ms(fun({M,N}) when M==N-> true end)};

compile_assert(distinct, Props, F)->
    [V0|Args] = binary:split(bnow_var:bin(proplists:get_value(right, Props, "1 day")), <<":">>),
    Left = proplists:get_value(left, Props, ""),
    K1 = magic:compile_string(Left),
    case K1 of
        [compiled_str, {1, _}, _] -> #bnow_assert{type=pass};
        _ ->
            V1 = case re:run(V0, "^\s*([0-9]+)\s*(second|day|week)(?:s|)\s*$") of
                {match, [_, N,T]} ->
                    {bnow_var:atom(binary:part(V0, T)),
                    bnow_var:int(binary:part(V0, N))};
                _ ->
                    case bnow_var:int(V0) of
                        0 -> {day, 1};
                        N -> {second, N}
                    end
                end,
            App = bnow_var:bin(F#bnow_filter.app),
            Args2  = magic:compile_string(Args),
            LeftBin = bnow_var:bin(Left),
            #bnow_assert{left={<<"/", App/binary, 0:8>>, K1}, right={V1, Args2}, type=distinct, mode=string}
    end;

compile_assert(_, Props, F)->
    compile_assert(is, Props, F).

compile_set(#bnow_set{type=number}=S) when is_record(S, bnow_set)->
    C = magic:compile_expr(S#bnow_set.value),
    compile_set_modifier(S#bnow_set{value=C, var=bnow_var:bin(S#bnow_set.var)});
compile_set(S) when is_record(S, bnow_set)->
    C = magic:compile_string(S#bnow_set.value),
    compile_set_modifier(S#bnow_set{value=C, var=bnow_var:bin(S#bnow_set.var)}).

compile_set_modifier(#bnow_set{modifier=undefined} = S) -> S;
compile_set_modifier(S) ->
    Modifier = case string:tokens(S#bnow_set.modifier, ":") of
        [M | A] ->
            case string:tokens(M, "/") of
                [P | [Command | _ ]] ->
                    {bnow_var:atom(P), bnow_var:atom(Command), A};
                [P] ->
                    {bnow_modifier, bnow_var:atom(P), A}
            end;
        _ -> undefined
    end,
    S#bnow_set{modifier=Modifier}.

apply_set(S, D0) when is_record(S, bnow_set)->
    V2 = magic:exec(S#bnow_set.value, D0),
    V3 = case S#bnow_set.modifier of
        {Plugin, Command, Arg} -> 
            bnow_plugin:modifier(Plugin, Command, Arg, V2);
        undefined -> V2
         end,
    bnow_record:set_value(S#bnow_set.var, V3, D0).


assert_value(V, string,  D)->
    magic:exec(V, D);
assert_value(V, number,  D)->
    magic:exec(V, D).

match_assert(A, D)->
    D3 = lists:foldl(fun(S, D0)->
                             apply_set(S, D0)
                     end, D, A#bnow_assert.sets),
%%    {true, D3}.
    
    case do_match_assert(A, D) of
        N when N=/=A#bnow_assert.not_is-> 
            D3 = lists:foldl(fun(S, D0)->
                        apply_set(S, D0)
                end, D, A#bnow_assert.sets),
            {true, D3};
        {N, D2} when N=/=A#bnow_assert.not_is ->
            D3 = lists:foldl(fun(S, D2)->
                        apply_set(S, D2)
                end, D2, A#bnow_assert.sets),
            {true, D3};
        _ -> false
    end.

do_match_assert(#bnow_assert{type=distinct} = A, D)->
    Time = bnow_var:int(bnow_record:get_value(<<"@time">>, D, fun()->
                    bnow_timer:now() end)),
    {Left, K0} = A#bnow_assert.left,
    K = assert_value(K0, A#bnow_assert.mode, D),
    {TTL, V} = A#bnow_assert.right,
    %%io:format("------- ~p\n\n" , [V]),
    %%io:format("======= ~p\n\n" , [D]),
    Data = magic:exec(V, D),
    %%io:format("******* ~p\n\n" , ["hello"]),

    bnow_distinct:test(Time, <<Left/binary, K/binary>>, TTL, Data);


do_match_assert(#bnow_assert{type=re} = A, D)->
    Left = assert_value(A#bnow_assert.left, A#bnow_assert.mode, D),
    case re:run(Left, A#bnow_assert.right) of
        nomatch -> false;
        {match, Captured} ->
	    if 
            A#bnow_assert.sets=:=[] -> true;
            true ->
                {_, D2} = lists:foldl(fun(Part, {N, D0})->
                    {N+1, bnow_record:set_value(bnow_var:bin(N),
                            binary:part(Left, Part), D0)}
                end, {0, D}, Captured),
                {true, D2}
	    end
    end;


do_match_assert(#bnow_assert{type=pass} = A, D)-> true;
do_match_assert(#bnow_assert{type=isset} = A, D)->
    bnow_record:isset(A#bnow_assert.left, D);

do_match_assert(#bnow_assert{type=has} = A, D)->
    Left = assert_value(A#bnow_assert.left, A#bnow_assert.mode, D),
    Right = assert_value(A#bnow_assert.right, A#bnow_assert.mode, D),
    case binary:match(Left, Right) of
        nomatch -> false;
        _ -> true
    end;

do_match_assert(#bnow_assert{type=bm} = A, D)->
    Left = assert_value(A#bnow_assert.left, A#bnow_assert.mode, D),
    case binary:match(Left, A#bnow_assert.test) of
        nomatch -> false;
        _ -> true
    end;

do_match_assert(#bnow_assert{type=match_spec} = A, D)->
    Left = assert_value(A#bnow_assert.left, A#bnow_assert.mode, D),
    Right = assert_value(A#bnow_assert.right, A#bnow_assert.mode, D),
    case erlang:match_spec_test({Left, Right}, A#bnow_assert.test, table) of
        {ok, true, _, _} ->
            true;
        _ ->
            false
    end;

do_match_assert(_A, _D)-> erlang:display(_A#bnow_assert.type), false.
