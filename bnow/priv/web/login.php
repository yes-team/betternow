<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');

if($_SESSION['user']){
    redirect("/index.php");
}


$vars = array('TITLE'=>'身份验证','SHOW_TITLE'=>false);

$publickey = '6LfQidcSAAAAAFmuv-p_F1tNR-4riQb5KE-zmUEd';
$privatekey = '6LfQidcSAAAAAJoVEdoEI6USdy4NjAXBxwGudXa2';

require_lib('recaptchalib.php');
$recaptcha_error = '';

if($_POST['user']){
    if($_SESSION['loginfailed']>3){
        $resp = recaptcha_check_answer ($privatekey,
                                $_SERVER["REMOTE_ADDR"],
                                $_POST["recaptcha_challenge_field"],
                                $_POST["recaptcha_response_field"]);
        if (!$resp->is_valid) {
            $recaptcha_error = $resp->error;
            $check_captcha = false;
        }else{
            $check_captcha = true;
        }
    }else{
        $check_captcha = true;
    }

    if($check_captcha){
        $result = bnow::login($_POST['user'], $_POST['password'], $_SERVER['REMOTE_ADDR']);
        if($result){
            $_SESSION['user'] = $_POST['user'];
            unset($_SESSION['loginfailed']);
            if($_POST['remember_me']){
                bnow_session::cookie_remember();
            }
            redirect($_GET['redirect']);
        }else{
            $_SESSION['message'][] = '用户名/密码错误';
            $_SESSION['loginfailed'] = intval($_SESSION['loginfailed']) + 1;
        }
    }
}
$vars['capthcha'] = recaptcha_get_html($publickey, $recaptcha_error, true);
bnow_render::page('login.html', $vars);
