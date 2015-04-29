<?php
class bnow_session{

    static private $cookie_name = "sid";
    static private $has_write_back = false;
    static private $id;
    static private $ttl;

    static public function start(){
        if(!$_COOKIE[self::$cookie_name]){
            self::$id = uniqid('', true);
            self::cookie_once();
        }else{
            self::$id = $_COOKIE[self::$cookie_name];
        }
        register_shutdown_function(array('bnow_session', 'write'));
        self::read();
    }

    static public function cookie_remember(){
        self::$ttl = 3600*24*90;
        self::set_cookie(time() + self::$ttl);
    }

    static public function cookie_once(){
        self::set_cookie(0);
    }

    static private function set_cookie($time){
        setcookie(self::$cookie_name, self::$id, $time, '/');
    }

    static public function id(){
        return self::$id;
    }

    static public function read(){
        $sess = bnow::session_read(self::id());
        self::$ttl = $sess['ttl'];
        $_SESSION = unserialize($sess['data']);
        if($_SERVER['SCRIPT_NAME']!='login.php' || ($sess['user']['name'] && $sess['user']['enabled'])){
            $_SESSION['user'] = $sess['user'];
        }else{
            unset($_SESSION['user']);
            $_SESSION['message'][] = '账号'.$sess['user']['name'].'被禁用';
            redirect("/index.php");
        }
    }

    static public function write(){
        if(!self::$has_write_back){
            self::$has_write_back = true;
            $sess_data = $_SESSION;
            if(is_array($sess_data['user']) && $sess_data['user']['name']){
                $user = $sess_data['user']['name'];
                unset($sess_data['user']);
            }elseif(is_string($sess_data['user'])){
                $user = $sess_data['user'];
            }else{
                $user = '';
            }
            $data = serialize($sess_data);
            return bnow::session_write(self::id(), $user , $data, self::$ttl);
        }
    }

}

bnow_session::start();

function redirect($path){
    echo <<<EOF
    <html>
    <title>loading...</title>
    <meta http-equiv="refresh" content="0;url=$path" >
    </html>
EOF;
    exit;
}

if( !defined("I_AM_API") || I_AM_API != true )  {
    if(!$_SESSION['user'] && $_SERVER['SCRIPT_NAME']!='/login.php'){
        $org_path = $_SERVER['SCRIPT_NAME'];
        if($_SERVER['QUERY_STRING']){
            $org_path.='?'.$_SERVER['QUERY_STRING'];
        }
        redirect('/login.php?redirect='.urlencode($org_path));
    }
}
