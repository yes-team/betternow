<?php
$GLOBALS['skip_auth'] = 1;
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
bnow_render::page('api.html', array('TITLE'=>'应用列表','SHOW_TITLE'=>false));
