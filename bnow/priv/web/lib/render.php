<?php
require_lib('smarty/smarty.class.php');
class bnow_render{
    
    static private $t = null;
    
    static private function t(){
        if(!self::$t){
            self::$t = new Smarty;
            self::$t->template_dir = array( 0 => $_SERVER['BNOW_BASEDIR'].'/template/');
            self::$t->compile_dir = sys_get_temp_dir();
            self::$t->caching = false;
            self::$t->left_delimiter = "<{";
            self::$t->right_delimiter = "}>";
            self::$t->registerResource('global', new bnow_resource($_SERVER['BNOW_BASEDIR'].'/template/'));
            self::$t->addPluginsDir($_SERVER['BNOW_BASEDIR'].'/lib/smarty_plugins/');
            if($_SERVER['BNOW_APP']){
                self::$t->default_resource_type = $_SERVER['BNOW_APP'];
                self::$t->registerResource($_SERVER['BNOW_APP'], 
                    new bnow_resource($_SERVER['BNOW_APPDIR'].'/'.$_SERVER['BNOW_APP'].'/web/template/'));
            }else{
                self::$t->default_resource_type = 'global';
            }
        }
        return self::$t;
    }
    
    static public function display($file, $vars=array()){
        echo self::fetch($file, $vars);
    }
    
    static public function fetch($file, $vars=array()){
        $t = self::t();
        foreach($vars as $k=>&$v){
            $t->assignByRef($k, $v);
        }
        return $t->fetch($file);
    }
    
    static public function page($file, $vars=array()){
        foreach(bnow::exec('bnow_app:list()') as $app){
            array_map_kv($app['info']);
            $vars['__APP__'][$app['app']] = $app;
        }
        if($vars['TITLE'] && !isset($vars['SHOW_TITLE'])){
            $vars['SHOW_TITLE'] = true;
        }
        $vars['__PAGE__'] = $file;
        $vars['__GLOBAL_MENUS__'] = array(
            array('link'=>'/index.php','text'=> '应用列表'),
        );
        if($_SESSION['user']['is_admin']){
            $vars['__GLOBAL_MENUS__'][] = array('link'=>'/useradm.php','text'=> '用户管理');
            $vars['__GLOBAL_MENUS__'][] = array('link'=>'/certadm.php','text'=> '证书管理');
            $vars['__GLOBAL_MENUS__'][] = array('link'=>'/notify.php','text'=> '通知管理');
        }
        $vars['__GLOBAL_MENUS__'][] = array('link'=>'/about.php','text'=> '关于本系统');
        $vars['__MESSAGE__'] = $_SESSION['message'];
        unset($_SESSION['message']);
        self::display('global:page.html', $vars);
    }

}


class bnow_resource extends Smarty_Resource_Custom {

    private $basedir;

    public function __construct($basedir) {
        $this->basedir = $basedir;
    }

    protected function fetch($name, &$source, &$mtime) {
        $file = $this->basedir.$name;
        if(is_file($file)){
            $source = file_get_contents($file);
            $mtime = filemtime($file);
        }
    }

    protected function fetchTimestamp($name) {
        $file = $this->basedir.$name;
        if(is_file($file)){
            return filemtime($file);
        }
    }
}
