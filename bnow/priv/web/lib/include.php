<?php
error_reporting(E_ALL ^ E_NOTICE);
ini_set('open_basedir', $_SERVER['BNOW_BASEDIR'].PATH_SEPARATOR
    .$_SERVER['BNOW_APPDIR'].PATH_SEPARATOR.sys_get_temp_dir() );
ini_set('default_charset', 'utf-8');
ini_set("display_errors","On"); 
if(function_exists('date_default_timezone_set')){
    date_default_timezone_set('Asia/Shanghai');
}

$_SERVER['BNOW_LIBDIR'] = $_SERVER['BNOW_BASEDIR'].'/lib/';

function require_lib(){
    foreach(func_get_args() as $lib){
        require_once($_SERVER['BNOW_LIBDIR'].$lib);    
    }
}

require_lib('server.php', 'session.php', 'render.php');

function array_map_kv(&$array){
    if($array){
        $ret = array();
        foreach($array as $k=>$v){
            $ret[$v[0]] = $v[1];
        }
        $array = $ret;
        return $array;
    }
}
