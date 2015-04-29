<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');

//require(dirname(__FILE__).'/../include/record.php');

class bnow_notify
{

    private $api, $args;
    
    private $filter_type = array(
        'filter_avg' => array('range', 'threshold')
        ,'filter_num'=> array('range', 'threshold', 'num')
        ,'filter_rate'=> array('range', 'threshold')
    );
    function __construct($api=null, $args=array())
    {
        $this->api = $api;
        $this->args = $args;
    }

    public function exec()
    {
        return $this->{$this->api}();
    }
    #End Func exec
    
    
    private function add_token()
    {
        $token_id = $this->args['token_id'];
        $callback = $this->args['callback'];
        if(!$token_id) return array('error', 'msg'=>'token_id is empty');
        if(!$callback) return array('error', 'msg'=>'callback is empty');
        $token = md5($token_id.microtime().srand());
        $r = bnow::call("bnow_notify", 'add_token', $token_id, $token, $callback);
        if($r==true) {
            return array('token'=>$token);
        }else {
            return $r;
        }
    }

    private function list_token()
    {
        $offset = $this->args['offset'] ? $this->args['offset'] : 1;
        $limit = $this->args['limit'] ? $this->args['limit'] : 100;
        $token_id  = $this->args['token_id'] ? $this->args['token_id'] : "";
        //$tokens = 
        return bnow::call("bnow_notify", 'list_token', $offset, $limit, $token_id);
        //return enotify\enotify_record($tokens);
    }
    #End Func list_token
    #


    private function add_rule()
    {
        $filter = $this->args['filter'];
        $key = $this->args['key'];
        $this->args['interval'] = $this->args['interval'] ? $this->args['interval'] : $filter['range'];
        $interval = (int)$this->args['interval'];
        if($interval<1) return array('status'=>'error', 'msg'=>'interval is error!');
        $action = $this->args['action'];
        $callback_id = $this->args['callback_id'];
        $token_id = $this->args['token_id'];
        foreach( array('key', 'filter', 'interval', 'action', 'callback_id', 'token_id') as $v ) {
            if(!$$v) return array('status'=>'error', 'msg'=>$v .' is empty');
        }
        return bnow::call("bnow_notify", 'add_rule', $key, $token_id,  $interval, $filter, $callback_id, $action, 'false');
    }

    
    private function list_rule()
    {
        $pos  = $this->args['pos'] ? $this->args['pos'] : 1;
        //$arr = 
        $list = bnow::call("bnow_notify", 'list_rule', (int)$pos, 10, $this->args);
        foreach( $list as &$v ){
            $arr = array();
            foreach( $this->filter_type[$v['filter'][0]] as $tk => $atype ){
                $arr[$atype] = $v['filter'][$tk+1];
            }
            $v['filter'] = $arr;
        }
        return $list;
        //return enotify\enotify_record($arr);
    }

    private function get_rule()
    {
        if(!$this->args['token_id']) return array('status'=>'error', 'msg'=>'token_id is empty');
        if(!$this->args['key']) return array('status'=>'error', 'msg'=>'key is empty');
        $rules = bnow::call( "bnow_notify", 'list_rule', 1, 1, array('token_id'=>$this->args['token_id'], 'key'=>$this->args['key']) );
        return $arr;
        //$arr = enotify\enotify_record($rules);
        //return $arr[0];
    }

    private function del_rule()
    {
        if(!$this->args['token_id']) return array('status'=>'error', 'msg'=>'token_id is empty');
        if(!$this->args['key']) return array('status'=>'error', 'msg'=>'key is empty');
        return bnow::call("bnow_notify", 'del_rule', $this->args['key'], $this->args['token_id']);
    }
    #End Func del_rule
    
    
    private function fetch_rrd()
    {
        if(!$this->args['key']) return array('status'=>'error',' msg'=>'key is empty!');
        if(!$this->args['endtime'] || !$this->args['starttime'] || $this->args['endtime']<$this->args['starttime']) 
            return array('status'=>'error', 'msg'=>'time is error!');
        $interval = $this->args['interval'] ? (int)$this->args['interval'] : 10;
        $types = array(
            0 => 'avg',
            1 => 'sum',
            2 => 'max',
            3 => 'min'
        );
        $r  = bnow::call('bnow_notify', 'fetch_rrd', $this->args['key'], $this->args['starttime'], $this->args['endtime'], $interval);
        $return = array();
        $return['type'] = $types[$r[0]];
        foreach((array)$r[1] as $val) {
            $data[$val[0]] = array($val[0], $val[1][0]);
        }
        if($data)sort($data);
        $return['data'] = $data;
        return $return;
    }

    private function chart()
    {
        $vars = array(
                'items' => (array)$this->args['key'],
                'starttime' => $this->args['starttime'],
                'endtime' => $this->args['endtime'],
                'height'=> $this->args['height'],
                'width'=> $this->args['width'],
            );
        bnow_render::display('chart.html', $vars);
    }
    


}


$getAction = $_GET['action'];
if($getAction=='help'){
    bnow_render::page('notify/help.html', $vars);
}elseif($getAction=='rule'){
    if($_POST) {
        $bnownotify = new bnow_notify('add_rule', $_POST);
        $bnownotify->exec();
        $alert = true;
        $_SESSION['message'][] = '通知添加成功';
        redirect('/notify.php');
    }
    $filter = array(
        'type'  => array(
            'avg' => '总平均值',
            'num' => '峰值',
            'rate' => '增长率',
            ),   
    );
    $bnownotify = new bnow_notify('list_token', $_GET);
    $r = $bnownotify->exec();
    $tokens = array();
    foreach((array)$r as $row) {
        $tokens[$row['id']] = $row['id'];
    }
    $vars = array(
        'tokens' => $tokens,
        'filter' => $filter,
        'post' => $_POST,
        'alert' => $alert,
    );
    bnow_render::page('notify/rule.html', $vars);
}elseif($getAction=="del_token"){
    if($_GET['token_id']){
        bnow::call("bnow_notify", 'del_token', $_GET['token_id']);
        $_SESSION['message'][] = 'token删除成功';
        redirect('/notify.php?action=token_list');
    }
}elseif($getAction=='token'){
    if($_POST) {
        $bnownotify = new bnow_notify('add_token', $_POST);
        $r = $bnownotify->exec();
        $token = $r['token'];
        $alert = true;
        $_SESSION['message'][] = 'token添加成功';
        redirect('/notify.php?action=token_list');
    }
    if($_GET['token_id']) {
        $bnownotify = new bnow_notify('list_token', $_GET);
        $arr = $bnownotify->exec();
        $edit_token = $arr[0];
    }
    $callback = array(
        'tcp' => 'tcp',
        'http' => 'http',
        'apply' => 'apply',
    );
    $vars = array(
        'post' => $_POST,
        'token' => $token,
        'callback' => $callback,
        'edit_token' => $edit_token,
        'alert' => $alert,
    );
    if( !$alert || !$_POST ){
        bnow_render::page('notify/token.html', $vars);
        exit;
    }
}elseif($getAction=='token_list'){
    $bnownotify = new bnow_notify('list_token', $_GET);
    $tokens = $bnownotify->exec();
    $vars = array(
        'tokens' => $tokens,
    );
    bnow_render::page('notify/token-list.html', $vars);
    exit;
}elseif($getAction=='del_rule'){
    $bnownotify = new bnow_notify('del_rule', $_GET);
    $bnownotify->exec();
    $_SESSION['message'][] = '删除成功';
    redirect('/notify.php');
}else{

    $bnownotify = new bnow_notify('list_rule', $_GET);
    $list = $bnownotify->exec();

    $vars = array(
        'list' => $list,   
        'alert' => $alert,
    );

    bnow_render::page('notify/list.html', $vars);
}
