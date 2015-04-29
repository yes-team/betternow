<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
bnow_ui::init();
$a=  bnow::exec('bnow_app:list()');
var_dump($a);
