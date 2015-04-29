-module(bnow_record).
-export([from_bin/1, clone/1, clone/2, close/1]).
-export([get_value/2 , get_value/3, set_value/3, to_list/1, class/1, time/1, isset/2]).

-include("defined.hrl").

clone(D, To_D) -> magic_record:clone(D, To_D).
clone(D)-> magic_record:clone(D).
close(D)-> magic_record:close(D).
get_value(K, D)-> magic_record:get_value(K, D, <<>>).
get_value(K, D, Default)-> magic_record:get_value(K, D, Default).
set_value(K, V, D)-> magic_record:set_value(K, V, D).
to_list(D) -> magic_record:to_list(D).
class(D) -> bnow_var:atom(magic_record:get_value(<<"@class">>, D, <<>>)).
time(D) -> magic_record:get_value(<<"@time">>, D, fun bnow_timer:now/0).
isset(K,D) -> magic_record:isset(K,D).

from_bin(Bin)->
	D = magic_record:new(),
%%    D = bnow_var:atom(bnow_var:get_app_env(bnow, application, fun magic_record:new/0)),
%    D = 'matrix_bnow',
%io:format( "==~p\n\n" , [Bin] ),
	D2 = parse_data_bin(Bin, D),
    ?dio(D2),
    ?dio({dict, get()}),
	case magic_record:get_value(<<"@time">>, D2, undefined) of
		undefined -> ?dio(1), magic_record:set_value(<<"@time">>, bnow_var:bin(bnow_timer:now()), D2);
		T1 when is_binary(T1) -> ?dio(2),magic_record:set_value( <<"@time">> , T1 , D2 );
		_ -> ?dio(3),D2
        end.

parse_data_bin(Bin, D) when is_binary(Bin) ->
    {ok, {L}} = msgpack:unpack(Bin),
    ?dio(D),
    parse_data_bin(L, D, []).
parse_data_bin([], D, Acc0) ->
    magic_record:set_value(Acc0, D),
    D;
parse_data_bin([{K, V}|T], D, Acc0) ->
    parse_data_bin(T, D, [{be_bin(K), be_bin(V)}|Acc0]).

%%parse_data_bin([], D) -> D;
%%
%%parse_data_bin([{K, V}|T], D) ->
%%	D2 = magic_record:set_value(be_bin(K), be_bin(V), D),
%%	parse_data_bin(T, D2).



be_list(X) when is_list(X)-> X;
be_list(X) when is_integer(X) -> integer_to_list(X);
be_list(X) when is_atom(X) -> atom_to_list(X);
be_list(X) when is_binary(X) -> binary_to_list(X);
be_list(X) -> lists:flatten(io_lib:format("~p",[X])).
    
be_bin(X) when is_binary(X)->X;
be_bin(X) when is_atom(X)->list_to_binary(atom_to_list(X));
be_bin(X) when is_integer(X)-> list_to_binary(integer_to_list(X));
be_bin(X)-> list_to_binary(be_list(X)).
