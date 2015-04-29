<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
if(!isset($_GET['item'])){
    $_GET['item'][] = '/'.$_SERVER['BNOW_APP_NAME'].'/system/incoming';
}
foreach($_GET['item'] as $k=>$v){
    if(!$v){
        unset($_GET['item'][$k]);
    }
}

$end_time = strtotime($_GET['time']['end']);
$end_time = $end_time?$end_time:time();
$start_time = strtotime($_GET['time']['begin']);
$start_time = $start_time?$start_time:($end_time-3600);

$vars = array(
    'TITLE'=>'图表输出',
    'item_id'=>'url/nds.tgbus.com',
    'items'=>$_GET['item'],
    'start_time'=>$start_time,
    'end_time'=>$end_time,
);

bnow_render::page(basename(__FILE__, ".php").'.html', $vars);

