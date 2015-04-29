-record(bnow_app, {app, config_hash, name, info=[], filter=[], action=[], trigger=[], plugin=[], menu=[], status}).

-record(bnow_filter, {id, app, class, data=[]}).
-record(bnow_action, {action, args, compiled_args}).
-record(bnow_assert, {left=[], right=[], mode=string, type=match_spec, test, sets=[], not_is=false}).
-record(bnow_set, {var, value=[], type=string, modifier}).

-record(bnow_element, {id, type, app, key}).

-record(bnow_menu, {app, text="", link="#", desc=""}).
-record(bnow_plugin, {app, module, options=[]}).

-record(bnow_session, {sess_id, user=[], data, update_time, ttl=1800}).
-record(bnow_topn, {id, name, time, interval_hours, total=0, data=[], limit=240}).
-record(topn_cmd, {name, item, hours=1, value=1, size=10, interval_hours=1, limit=240}).
-record(bnow_appconf, {id, app, key, phpval, value}).
-record(bnow_user, {name, password, seed, enabled=true, is_admin=false, join_time, last_login, acl=[], meta=[]}).
-record(bnow_cert, {key, secret,  status=active, join_time = bnow_timer:now(),  acl=[], meta=[]}).
-record(bnow_notify, {id, key, time, filter, callback_id, action, readonly, token}).
-record(bnow_notify_token, {id, token, callback}).

%-record(bnow_bucket, {id , node , status=active}).

-record(bnow_item, {id, type, name}).
-record(bnow_alert, {interval=3600, left=[], right=[], mode=string, type=match_spec, test, sets=[], not_is=false}).

-record(bnow_event, {app, item, alert, action=[]}).

-record(bnow_memcache, {key, value ,flag, expr , ver}).
-record(bnow_memcache_ver , {k,v}).

-record(bnow_bundle , {key , conninfo}).
-record(bnow_cursor, {key, options=[]}).

%-define(BUCKET_NUM, 1).
-define(SYSTEM_COUNTER, "/system/incoming").

-define(T_AVG, 0).
-define(T_SUM, 1).
-define(T_MAX, 2).
-define(T_MIN, 3).

-record(update_args, {name, value=1, function=?T_SUM}).
-record(rrd_record, {time, value}).

-record(msg_state, {data, queue_msg_tag, mq_req_channel, listener_name}).

%%-define(dio(X), spawn(io, format, ["[~p, ~p] : ~p\n", [?MODULE, ?LINE, X]])).
-define(dio(X), skip).
