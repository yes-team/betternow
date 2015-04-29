<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
if(!$_SESSION['user']['is_admin']) exit('Access denined');

if($_GET['action']=='new'){

    if($_POST['uname']){

        if(strlen($_POST['password'])<5){
            $_SESSION['message'][] = '密码过短.';
            $params_broken = true;
        }

        if($params['password'] != $params['password_re']){
            $_SESSION['message'][] = '两次输入密码不一致.';
            $params_broken = true;
        }

        if(!$params_broken){
            $r = bnow::call('bnow_user', 'register', $_POST['uname'], $_POST['password'], $_POST);
            if($r){
                $_SESSION['message'][] = '账号'.$_POST['uname'].'创建成功.';
                redirect('/useradm.php');
            }
        }
    }

    $vars = array( 'TITLE'=>'创建用户');
    $vars['apps'] = bnow::exec('bnow_app:list()');
    bnow_render::page('userinfo.html', $vars);
}elseif($_GET['action']=='edit' && $_GET['user']){

    if($_POST['save']){
        $params = $_POST;
        $params['is_admin'] = $params['is_admin'];
        $params['enabled'] = $params['enabled'];

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
            $r = bnow::call('bnow_user', 'update', $_GET['user'], $params);
            if($r){
                $_SESSION['message'][] = '账号'.$_GET['user'].'信息已更新.';
                redirect('/useradm.php?action=edit&user='.$_GET['user']);
            }
        }
    }

    $vars = array( 'TITLE'=>'编辑账号');
    $vars['apps'] = bnow::exec('bnow_app:list()');
    $vars['user'] = bnow::call('bnow_user', 'fetch', $_GET['user']);
    bnow_render::page('userinfo.html', $vars);
}elseif($_GET['action']=='delete'){

    if($_POST['user']){
        $users = is_array($_POST['user'])?$_POST['user']:array($_POST['user']);
    }elseif($_GET['user']){
        $users = is_array($_GET['user'])?$_GET['user']:array($_GET['user']);
    }

    foreach($users as $user){
        $r = bnow::call('bnow_user', 'unregister', $user);
        if($r){
            $_SESSION['message'][] = '账号'.$_GET['user'].'已删除.';
        }
    }
    redirect('/useradm.php');
}else{
    $vars = array( 'TITLE'=>'账号管理');
    $vars['members'] = bnow::call("bnow_user", 'list');
    bnow_render::page('useradm.html', $vars);
}
