<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
$vars = array(
//    'config' => bnow::config(),
    'version' => bnow::version(),
//    'licence' => bnow::licence(),
    'TITLE'=>'关于Betternow',
    'SUB_TITLE'=>'高效的实时数据分析系统'
);

bnow_render::page('about.html',$vars);
