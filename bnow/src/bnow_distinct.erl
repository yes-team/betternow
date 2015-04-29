-module(bnow_distinct).
-include("defined.hrl").
-export([test/4, get/1 , get/2 , db/0]).

db() ->
    Key = "distinct"
    ,case erlang:get( {db_fp , Key } ) of
        undefined -> 
            case bnow_global:fetch( {db_fp , Key } ) of
                undefined -> 
                    {DistinctHost , DistinctPort} = bnow_var:get_app_env( bnow, distinct_server),
                    {ok, C} = eredis:start_link( DistinctHost , DistinctPort )
%                    ,{ok, Datadir} = application:get_env(bnow, datadir)
%                    ,Path = filename:join(Datadir, "leveldb/"++  Key  ++"_kv.db/" )
%                    ,filelib:ensure_dir(Path)
%                    ,{ok, Ref} = eleveldb:open(Path, [{create_if_missing, true} , {compression , true} ,{sst_block_size , 32*1024*1024}, {write_buffer_size, 256*1024*1024} , {cache_size , 1024*1024*1024} ,{use_bloomfilter , true} ])
                    ,bnow_global:store( { db_fp, Key } , C )
                    ,put( {fp , Key} , C )
                    ,C
                ;_Ref -> _Ref
            end
        ;_Ref -> _Ref
    end.

test(T, Key0, TTL, Data)->
    Key = bnow_var:bin(Key0),
%    bnow_bucket:apply(?MODULE, {test, T, Key, TTL, Data}, Key).
    Timezone = 8,
    Timeout = case TTL of
        N when is_integer(N) -> T + N;
        {second, N} -> T+ N;
        {day, N} -> (N+(T div 86400)) * 86400 - (Timezone * 3600);
        {week, N} -> (N + (T div (86400*7))) * 7*86400 - 3*86400 - (Timezone * 3600)
    end,
    case get( Key, T ) of
        not_found -> 
            More = bnow_var:bin(Data),
%%            eleveldb:put( bnow_bucket:db( distinct ) , Key , <<Timeout:32/big, More/binary>> , [] ),
            case eredis:q( db() , ["SET", Key , <<Timeout:32/big, More/binary>> ] ) of
                {error , Error} -> io:format("distinct error: ~p\n" , [ Error ]);
                _ -> eredis:q( db() , ["EXPIRE" , Key , Timeout - bnow_timer:now() ] )
            end,
            true;
        _ -> false
    end.

get(Key0) ->
    get( Key0 , bnow_timer:now() ).
get(Key0 , T ) ->
    Key = bnow_var:bin(Key0),
    %bnow_bucket:apply(?MODULE, {get, Key, bnow_timer:now()}, Key).
    case eredis:q( db() , [ "GET", Key ] ) of
    %case bnow_bucket:fetch( Key, []) of
        {ok,<<T2:32/big, Data/binary>>} when T2 > T ->
            {ok, T2, Data};
        _-> not_found
    end.

