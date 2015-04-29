-module(bnow_plugin).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_event/2, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, modifier/4]).
-record(state, {module, state}).
-include("defined.hrl").

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
	[{init, 1}, {handle_event, 3}, {terminate, 2}, {code_change, 3}];
behaviour_info(_Other) ->
	undefined.

start_link(Mod, Opts) ->
    gen_server:start_link({local, Mod}, ?MODULE, {Mod, Opts}, []).

init({Mod, Opts}) ->
    io:format("loading plugin: ~p\n", [Mod]),
    {ok, S} = Mod:init(Opts),
    {ok, #state{module = Mod, state = S }}.

handle_call({event, Action, Data}, _From, S) ->
    {reply, ok, S}.

handle_cast({event, Action, Data}, S) ->
    {noreply, State} = (S#state.module):handle_event(Data, Action, S#state.state),
    {noreply, S#state{state=State}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("info: ~p\n", [_Info]),
    {noreply, State}.

handle_event(A, Data)->
    gen_server:cast(A#bnow_action.args, {event, A, bnow_record:clone(Data)}).

terminate(_Reason, S) ->
    io:format("terminate: ~p, reason: ~p\n", [S#state.module, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

modifier(Plugin, Command, Arg, Value)->
    try
        case erlang:function_exported(Plugin, modifier, 3) of
            true ->
                bnow_var:bin(apply(Plugin, modifier, [Command,
                            bnow_var:bin(Value), Arg]));
            _ ->
                Value
        end
    catch _:_ -> Value end.
