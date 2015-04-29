<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
if( $_POST['save'] ){
    $params = $_POST;
    if( !$params['username'] || $params['username'] != $_SESSION['user']['name'] ){
            $_SESSION['message'][] = '修改失败.';
            $params_broken = true;
    }
    if($params['password']){
        if(strlen($params['password'])<5){
            $_SESSION['message'][] = '密码过短.';
            $params_broken = true;
        }
        if($params['password'] != $params['password_re']){
            $_SESSION['message'][] = '两次输入密码不一致.';
            $params_broken = true;
        }
    }
    if(!$params_broken){
        $r = bnow::call('bnow_user', 'update', $_SESSION['user']['name'], $params);
        if($r){
            $_SESSION['message'][] = '账号'.$_SESSION['user']['name'].'信息已更新.';
            redirect('/profile.php');
        }
    }
}
bnow_render::page('userinfo.html', array('TITLE'=>'关于我','SHOW_TITLE'=>'关于我','action'=> 'profile','apps' => bnow::exec('bnow_app:list()'), 'user'=>bnow::call('bnow_user', 'fetch', $_SESSION['user']['name']) ));

