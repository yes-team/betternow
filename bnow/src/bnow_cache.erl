%%%-------------------------------------------------------------------
%%% @copyright 2013 ShopEx Network Technology Co, .Ltd
%%% @author liyouyou <liyouyou@shopex.cn>
%%% @doc
%%% bnow_cache.erl
%%% @end
%%% Created : 2013/10/28 02:47:15 liyouyou
%%% #Time-stamp: <syrett 2013-10-29 11:46:24>
%%%-------------------------------------------------------------------

-module(bnow_cache).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([insert_file/2]).

-define(SERVER, ?MODULE).
-record(state,{fd}).

-record(filelog, {type, data}).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle_call(Request, _From, S) ->
    io:format("[~p, ~p]Warning: handled call info:~p\n", [?MODULE , ?LINE , Request]),
    {reply, ok, S}.

handle_cast({write_file, Cmd, Data}, #state{fd=Fd} = S) ->
    FileLog = #filelog{type=Cmd, data=Data},
    file:write(Fd, [<<"\n">>, term_to_binary(FileLog)]),
    {noreply, S};
handle_cast(Request, S) ->
    io:format("[~p, ~p]Warning: handled cast info:~p\n", [?MODULE , ?LINE , Request]),
    {noreply, S}.

handle_info(Info, S) ->
    io:format("[~p, ~p]Warning: handled info info:~p\n", [?MODULE , ?LINE , Info]),
    {noreply, S}.


insert(Data) ->
    ok.


insert_file(Cmd, Data) ->
%%    gen_server:cast(?SERVER, {write_file, Cmd, Data}).
	ok.
%%%===================================================================
%%% Internel Functions
%%%===================================================================

init([]) ->
    %% file log
    File = bnow_var:get_app_env(bnow, filepath, "bnow_cache_file"),
    {ok, Fd} = file:open(File, [append, {delayed_write,4*1024,1000}]),

    {ok, #state{fd=Fd}}.

terminate(_Reason, S) ->
    file:sync(S#state.fd),
    file:close(S#state.fd),
    io:format("[~p, ~p]: terminate for reason:~p\n", [?MODULE, ?LINE, _Reason]),
    ok.

