<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');

if(isset($_GET['action']) && $_GET['action'] == 'data-builder-send'){
    foreach(explode("\n", $_POST['data']) as $line){
        $p = strpos($line, ':');
        if($p && $p>1 && $line{$p-1}!='\\'){
            $data[trim(substr($line, 0, $p))] = ltrim(substr($line, $p+1));
        }else{
            $keys = array_keys($data);
            $count = count($keys);
            if($count>0){
                $last_k = $keys[$count-1];
                $data[$last_k] .= $line;
            }
        }
    }
    foreach($data as $k=>$v){
        $v = str_replace('\\:', ':', $v);
        $v = preg_replace_callback('/\#\{([^\}]+)\}/', msg_fun, $v);
        $data[$k] = $v;
    }
    echo bnow::call("bnow_sdk", 'handle_data', $data);
}else{
    $vars = array(
        'TITLE'=>'实时数据流',
        'SHOW_TITLE'=>false
    );
    bnow_render::page('desk.html', $vars);
}

function msg_fun($matches){
    $args = explode(':', $matches[1]);
    switch(array_shift($args)){
        case 'now': return time();
        case 'random':
            list($from, $to) = preg_split('/[^0-9]/', trim($args[0]));
            return rand($from+0, $to+0);
        default: return $matches[0];
    }
}
