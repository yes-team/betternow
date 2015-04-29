-module(bnow_app_edoc).
-include("defined.hrl").
-export([do/1]).
-compile(export_all).
%-record(bnow_app, {app, config_hash, name, info=[], filter=[], action=[], trigger=[], plugin=[], menu=[], status}).

%-record(bnow_filter, {id, app, class, data=[]}).

-define(RET,"\n").
-define(TAB,"    ").
-define(BIN(_B),"<<\"",_B,"\">>").
-define(FUN( FunName , Args , FunBody ), 
    FunName,"(", prepare_args( Args),") -> " ,?RET
    ,?TAB ,FunBody , "." , ?RET
    ,?RET
).
-define(FUN1( FunName , Args , FunBody, Sym ), 
    FunName,"(", prepare_args( Args),") -> " ,?RET
    ,?TAB ,FunBody , Sym , ?RET
).
-define(CUR_FUNC_LEVEL , [ integer_to_list(FuncLevel)]).
-define(NEXT_FUNC_LEVEL , FuncLevel +1 ).



do( App ) when is_record( App , bnow_app )->
    { F1 , F2 } = lists:foldr( fun prepare_filter/2, {[],[]} ,App#bnow_app.filter )
    ,Code = [
        "-module(" , bnow_var:list(App#bnow_app.app) , ")." ,?RET
        ,"-compile(export_all)." ,?RET
        ,?RET
        ,?FUN("get_hash" , [] , ?BIN( App#bnow_app.config_hash) )
        ,?FUN("get_author" , [] , ?BIN( proplists:get_value( author, App#bnow_app.info ) ) )
        ,?FUN("get_copyright" , [] , ?BIN( proplists:get_value( copyright, App#bnow_app.info ) ) )
        ,?FUN("get_desc" , [] , ?BIN( proplists:get_value( desc, App#bnow_app.info ) ) )
        ,F1
        ,F2
    ]
    ,write_file(Code).

prepare_args(Args) ->
    string:join( lists:map( fun(A) when is_binary(A) -> [?BIN(A)] ; (A) -> A end, Args ) , [","] ).

prepare_filter(Filter , {A1 , A2}) ->
    {
        [ [?FUN( "get_fid" , [Filter#bnow_filter.class] , ?BIN( Filter#bnow_filter.id ) ) ]| A1 ],
        [ prepare_filter_func( Filter#bnow_filter.data , {[] ,0}) |A2]
    }.

prepare_filter_func( F, FuncLevel) when is_record(F , bnow_filter) -> 
    "f",?CUR_FUNC_LEVEL,"(",?BIN( F#bnow_filter.class ),") -> " , ?RET
    ,lists:foldr( fun prepare_filter_func/2 , { [] , ?NEXT_FUNC_LEVEL } , F#bnow_filter.data )
;
prepare_filter_func( #bnow_assert{ type=isset ,not_is = false } = F , FuncLevel ) ->
    [
        ?TAB , "if (  )"
    ]
.

write_file(Code) ->
    erlang:display( file:write_file("/Users/guzhengxiao/dev/erl/bnow2/xxx.erl" ,  Code  ) ).


build() ->
%%    {atomic, Dict}  = mnesia:transaction(fun () ->
%%                                                 mnesia:foldl(fun(E, Dict0) ->
%%                                                                      Class = E#bnow_filter.class,
%%                                                                      dict:update(binary_to_list(Class), fun(Old) ->
%%                                                                                                 Old ++ [E]
%%                                                                                         end, E, Dict0)
%%                                                              end, dict:new(), bnow_filter)
%%                                         end),
    Dict = ets:foldl(fun({Class, E}, Dict0) ->
                             dict:update(Class, fun(Old) ->
                                                                        Old ++ [E]
                                                                end, [E], Dict0)
                     end, dict:new(), filters),

    Head = ["-module(bnow_filter_runtime).\n",
            "-export([handle_filter/2]).\n",
            "-include(\"defined.hrl\").\n\n"],
            
    New = dict:fold(fun(Class, Filters, Acc0) ->
                      Term = ["handle_filter(", io_lib:format("~p", [Class]),", D) -> \n",
                       "Filters = ", io_lib:format("~p", [Filters]), ",\n",
                       "[bnow_worker:exec_filter(F, D) || F <- Filters],\n",
                       "ok;\n\n"],
                      [Term | Acc0]
               end, [], Dict),

    Tail = ["handle_filter(Class, _) -> \n",
            "{error, no_such_class}.\n\n"
           ],
    ErlSrc = lists:flatten(Head ++ New ++ Tail),
    DR = file:write_file("data/bnow_filter_runtime.erl", list_to_binary(ErlSrc)),
    try
        IncDir = include_dir(),
        {module, bnow_filter_runtime} = dynamic_compile:load_from_string(
                ErlSrc, [{i, IncDir}, inline])
    catch _:Err->
        erlang:display({error, Err}),
        io:format("~s\n", [ErlSrc])
    end.

include_dir()->
    case code:lib_dir(bnow, include) of
        {error, _}-> "include";
        Path -> Path
    end.

    
    
