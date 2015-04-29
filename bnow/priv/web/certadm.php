<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
if(!$_SESSION['user']['is_admin']) exit('Access denined');

if($_GET['action']=='new'){
    $certinfo = array(
        'status' => 'active'
        ,'acl' => $_POST['acl']
    );
    if( $key = bnow::call('bnow_cert','new',$certinfo) )
        $_SESSION['message'][] = '证书 '.$key.' 生成';
    else
        $_SESSION['message'][] = '证书生成失败';
    redirect('/certadm.php');
//    $vars = array( 'TITLE'=>'生成证书');
//    bnow_render::page('certinfo.html', $vars);
}elseif($_GET['action']=='edit' && $_GET['key']){

    if($_POST['save']){
        $params = array(
            'status' => $_POST['status']
            ,'acl' => $_POST['acl']
        );
        if(!$params_broken){
            $r = bnow::call('bnow_cert','update', $_GET['key'], $params);
            if($r){
                $_SESSION['message'][] = '证书'.$_GET['key'].'已更新.';
                redirect('/certadm.php?action=edit&key='.$_GET['key']);
            }
        }
    }

    $vars = array( 'TITLE'=>'编辑账号');
    $vars['cert'] = bnow::call('bnow_cert', 'fetch', $_GET['key']);
    bnow_render::page('certinfo.html', $vars);
}elseif($_GET['action'] == 'alive'){
    $r = bnow::call('bnow_cert','alive', $_GET['key'] );
    if($r){
        $_SESSION['message'][] = '证书'.$_GET['key'].'已激活.';
        redirect('/certadm.php');
    }
}elseif($_GET['action'] == 'block'){
    $r = bnow::call('bnow_cert','block', $_GET['key'] );
    if($r){
        $_SESSION['message'][] = '证书'.$_GET['key'].'已停用.';
        redirect('/certadm.php');
    }
}elseif($_GET['action']=='delete'){
    $delkey = $_POST['key']?$_POST['key']:$_GET['key'];
    if( $delkey )
        $keys = is_array($delkey)?$delkey:array($delkey);
    foreach($keys as $key){
        $r = bnow::call('bnow_cert', 'delete', $key);
        if($r){
            $_SESSION['message'][] = '证书 '.$key.' 已删除.';
        }
    }
    redirect('/certadm.php');
}else{
    $vars = array( 'TITLE'=>'证书管理');
    $vars['certs'] = bnow::call("bnow_cert", 'list');
    bnow_render::page('certadm.html', $vars);
}
