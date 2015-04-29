-module(bnow_user).
-export([register/2,register/3, unregister/1, update/2, fetch/1, login/2,login/3, list/0]).
-include("defined.hrl").

register(Name, Params)->
    {ok,Seed} = bnow_var:guid(8),
    {atomic, U3} = mnesia:transaction(fun()->
            U = #bnow_user{
                     name = name(Name),
                     seed = Seed,
                     join_time = bnow_timer:now(),
                     is_admin = if
                        Name =:= <<"guzhengxiao">> -> true;
                        true -> false
                     end
                 },
            %U2 = update_record(U, [Params]),
            mnesia:write(U),
            U
        end),
    {ok, U3}.
register(Name, Password, Params)->
    {ok,Seed} = bnow_var:guid(8),
    {atomic, U3} = mnesia:transaction(fun()->
            U = #bnow_user{
                     name = name(Name),
                     seed = Seed,
                     join_time = bnow_timer:now()
                 },
            U2 = update_record(U, [{password, Password} | Params]),
            mnesia:write(U2),
            U2
        end),
    {ok, U3}.

unregister(Name)->
    {atomic, R} = mnesia:transaction(fun()->
            mnesia:delete({bnow_user, name(Name)})
        end),
    {ok, R}.

update(Name, Params)->
    {atomic, U3} = mnesia:transaction(fun()->
            [U|_] = mnesia:read(bnow_user, name(Name)),
            U2 = update_record(U, Params),
            mnesia:write(U2),
            U2
        end),
    {ok, U3}.

list()->
    {atomic, L1} = mnesia:transaction(fun()->
            mnesia:foldl(fun(U, L)->
                        [U#bnow_user{acl=[], seed=[], password=[], meta=[]} | L]
                end, [], bnow_user)
        end),
    {ok, L1}.

fetch(Name)->
    {atomic, R} = mnesia:transaction(fun()->
            case mnesia:read(bnow_user, name(Name)) of
                [U|_] -> {ok, U#bnow_user{seed=[], password=[]}};
                _ -> {error, not_exists}
            end
        end),
    R.

login(User,LoginData) ->
    Name = proplists:get_value( <<"user_id">> , proplists:get_value(<<"data">> , User) ),
    {atomic, Rst} = mnesia:transaction(fun()->
        case mnesia:read(bnow_user, name( Name )) of
            [U|_] ->
                mnesia:write(U#bnow_user{last_login={bnow_timer:now(), LoginData}}),
                true;
            _-> 
                ?MODULE:register( Name , User ),
                login( User,LoginData )
        end
    end),
    {ok, Rst}.
login(Name, Password, LoginData)->
    {atomic, Rst} = mnesia:transaction(fun()->
            case mnesia:read(bnow_user, name(Name)) of
                [U|_] ->
                    case encrypt_password(U#bnow_user.seed, Password) of
                        Pwd when Pwd=:=U#bnow_user.password -> 
                            mnesia:write(U#bnow_user{last_login={bnow_timer:now(), LoginData}}),
                            true;
                        _ -> false
                    end;
                _-> false
            end
        end),
    {ok, Rst}.

update_record(U, [])-> U;
update_record(U, [{Type, Data} | T])->
    U2 = apply_update(bnow_var:atom(Type), Data, U),
    update_record(U2, T).

apply_update(is_admin, _, U) when U#bnow_user.name =:= guzhengxiao ->
    U#bnow_user{is_admin=bnow_var:bool(true)};
apply_update(is_admin, Bool, U)->
    U#bnow_user{is_admin=bnow_var:bool(Bool)};

apply_update(password, Pwd, U) when is_binary(Pwd) and (byte_size(Pwd) >= 5)->
    Passwd_encypted = encrypt_password(U#bnow_user.seed, Pwd),
    U#bnow_user{password = Passwd_encypted};

apply_update(enabled, Bool, U)->
    U#bnow_user{enabled = bnow_var:bool(Bool)};

apply_update(meta, Meta, U)->
    U#bnow_user{meta = Meta};

apply_update(acl, Acl, U)->
    U#bnow_user{acl = Acl};

apply_update(_, _, U)-> U.

name(Name) -> bnow_var:atom(Name).

encrypt_password(Seed, Password)->
    erlang:md5(term_to_binary({Seed, Password})).
