<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
bnow_render::page('index.html', array('TITLE'=>'应用列表','SHOW_TITLE'=>false));
