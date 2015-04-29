<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');

if(isset($_POST['app']) && $_POST['app']){
    $rst = bnow::call("bnow_sdk", 'app_cmd', $_POST['app'], $_POST['cmd']);
    $_SESSION['message'] = '操作成功';
    header('Location: '.$_SERVER['HTTP_ORIGIN'].$_SERVER['PHP_SELF']);
}else{
    require('config.php');
    $vars = array(
        'TITLE'=>'当前环境',
        'config'=>$config,
    );
    bnow_render::page('env.html', $vars);
}
