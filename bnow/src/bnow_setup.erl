-module(bnow_setup).

-export([add_node/1, forget_node/1]).
-export([join/1]).
-export([init/0]).
-export([clear_db/0, all_tables/0]).
-export([install_table/1, install_table/2, reinstall_table/1]).
-compile(export_all).

-include("defined.hrl").
-include_lib("stdlib/include/qlc.hrl").
%%%===================================================================
%%% Common interface: init_system/join_system/upgrade
%%%===================================================================
init_system() ->
    init().

join_system(Node) ->
    join(Node).

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_node(Node)->
    pong = net_adm:ping(Node),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, Node, disc_copies),
    rpc:call(Node, mnesia, wait_for_tables, [schema]),
    forget_node(Node),
    [mnesia:add_table_copy(T, Node, disc_copies) || T <- all_tables()],
    ok.

forget_node(Node)->
    [mnesia:del_table_copy(T, Node) || T <- all_tables()].

clear_db()->
    try
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        mnesia:start()
    catch _:_->
            %% TODO: log error/warning here
            ok
    end.

join(N)->
    clear_db(),
    case rpc:call(N, ?MODULE, add_node, [node()]) of
        ok -> io:format("cluster joined\n");
        {error, Why} -> io:format("failed: ~s\n", [Why]);
        Other -> io:format("Unknow status: ~p\n", [Other])
    end,
    application:stop(bnow),
    application:start(bnow),
    ok.

init()->
    application:stop(bnow),
    clear_db(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    [install_table(T) || T <- all_tables()],
    mnesia:stop(),
    mnesia:start(),
    ok = mnesia:wait_for_tables(all_tables(), 1000),
    install_admin(),
    mnesia:stop(),
    application:start(bnow), 
    ok.

install_table(Table)->
    install_table([node()], Table).
    
install_table(Nodes, bnow_app )->
    mnesia:create_table(bnow_app , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_app)}
                                    ]);

install_table(Nodes, bnow_plugin )->
    mnesia:create_table(bnow_plugin , [
                                     {type, bag},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_plugin)}
                                    ]);
                                
install_table(Nodes, bnow_menu )->
    mnesia:create_table(bnow_menu , [
                                     {type, bag},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_menu)}
                                    ]);

install_table(Nodes, bnow_filter )->
    mnesia:create_table(bnow_filter , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_filter)}
                                    ]),
    mnesia:add_table_index(bnow_filter, class);

install_table(Nodes, bnow_action )->
    mnesia:create_table(bnow_action , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_action)}
                                    ]);

install_table(Nodes, bnow_appconf )->
    mnesia:create_table(bnow_appconf , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_appconf)}
                                    ]),
    mnesia:add_table_index(bnow_appconf, app);


install_table(Nodes, bnow_session )->
    mnesia:create_table(bnow_session , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_session)}
                                    ]);

install_table(Nodes, bnow_user )->
    mnesia:create_table(bnow_user , [
                                     {type, ordered_set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_user)}
                                    ]);

install_table(Nodes, bnow_topn )->
    mnesia:create_table(bnow_topn , [
                                     {type, ordered_set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_topn)}
                                    ]),
    mnesia:add_table_index(bnow_topn, name);

install_table(Nodes, bnow_cert )->
    mnesia:create_table(bnow_cert , [
                                     {type, ordered_set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_cert)}
                                    ]);
install_table(Nodes, bnow_memcache) ->
    mnesia:create_table(bnow_memcache , [
                                     {type, ordered_set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields,
                                             bnow_memcache)}
                                    ]);

install_table(Nodes, bnow_memcache_ver) ->
    mnesia:create_table(bnow_memcache_ver , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields,
                                             bnow_memcache_ver)}
                                    ]);

install_table(Nodes, bnow_notify )->
    mnesia:create_table(bnow_notify , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_notify)}
                                    ]);
install_table(Nodes, bnow_notify_token )->
    mnesia:create_table(bnow_notify_token , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_notify_token)}
                                    ]);
install_table(Nodes, bnow_bundle )->
    mnesia:create_table(bnow_bundle , [
                                     {type, set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_bundle)}
                                    ]);

install_table(Nodes, bnow_cursor )->
    mnesia:create_table(bnow_cursor , [
                                     {type, ordered_set},
                                     {disc_copies, Nodes},
                                     {attributes, record_info(fields, bnow_cursor)}
                                    ]);

install_table(_, _)->nothing.

install_admin()->
    A = case whereis(bnow_timer) of undefined-> bnow_timer:start(), true ; _->false end,
    {ok, Password} = bnow_var:guid(8),
    {ok, _} = bnow_user:register(admin, Password, [{is_admin, true}]),
    case A of true -> bnow_timer:stop() ; _ -> ok end,
    io:format("User \"admin\" created, password: \"~s\".\n", [Password]).

reinstall_table(Table)->
    mnesia:delete_table(Table),
    install_table(Table).

all_tables()-> [bnow_app, bnow_menu, bnow_user, bnow_plugin, bnow_filter, bnow_action,
    bnow_session, bnow_appconf,bnow_cert, bnow_memcache , bnow_memcache_ver , bnow_notify , bnow_notify_token, bnow_bundle, bnow_cursor  ].

