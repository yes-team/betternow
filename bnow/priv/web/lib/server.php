<?php
class bnow{

    static private $sock = null;
    static private $records = null;

    static public function init(){
        require($_SERVER['BNOW_LIBDIR'].'records.php');
        self::$records = &$records;
    	self::$sock = fsockopen("127.0.0.1", $_SERVER['BNOW_APIGATE'], $errno, $errstr, 10);
    }
    
    static public function __callStatic($method, $org_args){
        $args = $org_args;
        array_unshift($args, $_SERVER['BNOW_APP']);
        $args = self::term_val($args);
        $out = pack('na*ca*', strlen($method), $method , 131 , $args);
        fwrite(self::$sock, pack('N',strlen($out)).$out);
        $rst = self::result();
        if(is_array($rst)){
            if($rst[0]=='ok'){
                if($rst[1]==''){
                    $rst = array();
                }else{
                    self::parse_record($rst[1]);
                }
                return $rst[1];
            }else{
                throw new bnow_api_exception($method, $org_args, $rst);
            }
        }elseif($rst!='ok'){
            throw new bnow_api_exception($method, $org_args, $rst);
        }
    }
    
    //http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
    static private function term_val($arg){
        switch(gettype($arg)){
        case 'integer':
            if($arg>=0 && $arg<255){
                return pack('cc',97,$arg);
            }else{
                return pack('cN',98,$arg);
            }   
        case 'double':
            return pack('ca31',99,$arg);
        case 'NULL':
            return pack('c',106);
        case 'string':
            return pack('cNa*', 109, strlen($arg), $arg);
        case 'array':
            if(array_keys($arg)===range(0,count($arg)-1)){
                $p=array();
                foreach($arg as $k=>$v){
                    $p[]=self::term_val($v);
                }
                return pack('cNa*c',108,count($p),implode('',$p),106);
            }else{
                $p=array();
                foreach($arg as $k=>$v){
                    $p[]=pack('cc',104,2).self::term_val($k).self::term_val($v);
                }
                return pack('cNa*c',108,count($p),implode('',$p),106);
            }
        }
    }
    
    static private function result(){
        $c = unpack('Nsize', fread(self::$sock,4) );
        $size_in = $c['size'];
        $rst = '';
        $size = 0;
        while($size<$size_in){
            $rcv = fread(self::$sock, $size_in-$size);
            $size+=strlen($rcv);
            $rst.=$rcv;
        }
        $rst = unserialize($rst);
        return $rst;
    }
    
    static private function parse_record(&$rst){
        if(is_array($rst)){
            if($rst[0]==='kv'){
                foreach($rst[1] as $k => $v){
                    $ret[$v[0]] = $v[1];
                    self::parse_record($ret[$v[0]]);
                }
                $rst = $ret;
            } else if(!is_array($rst[0]) 
                    && isset(self::$records[$rst[0]]) && is_array(self::$records[$rst[0]])
                    && count($rst)==count(self::$records[$rst[0]])+1){
                foreach(self::$records[$rst[0]] as $k=>$v){
                    $ret[$v] = $rst[$k+1];
                    self::parse_record($ret[$v]);
                }
                $rst = $ret;
            }else{
                foreach($rst as $k=>$v){
                    self::parse_record($rst[$k]);
                }
            }
        }
    }

    static public function getconf($key){
        return unserialize(self::__callStatic(__FUNCTION__, array($key)));
    }

    static public function setconf($key, $val){
        return unserialize( self::__callStatic(__FUNCTION__, array($key, $val, serialize($val))) );
    }

    static public function config(){
        $config = self::exec('application:get_all_env(bnow)');
        array_map_kv($config);
        $config['php_fcgi'][1] = join($config['php_fcgi'][1], ".");
        $config['web_listen'][0] = join($config['web_listen'][0], ".");
        return $config;
    }

    
}

bnow::init();

class bnow_api_exception extends Exception{

    private $erl_trace;
    
    function __construct($method, $args, $return){
        $this->args = $args;
        $this->method = $method;
        $this->return = $return;
        parent::__construct(implode(',',$return));
    }

    function __toString(){
        return '<pre>'. print_r($this->return, true). '</pre>';
    }
    
}
