<?php
$re = '/^-record\s*\(\s*([a-z\_]+)\s*,\s*\{(.*?)\}\s*\)\.$/';
$ret = array();
foreach(file($_SERVER['argv'][1]) as $line){
    if(preg_match($re,trim($line),$m)){
        $ret[$m[1]] = array();
        foreach(explode(',',$m[2]) as $c){
            preg_match('/^\s*([a-z\_]+)/',$c,$b);
            $ret[$m[1]][] = $b[1];
        }
    }
}

echo '<',"?php\n",'$records=';
var_export($ret);
echo ";";
