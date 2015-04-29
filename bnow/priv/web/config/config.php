<?php
    define('BASE_PATH', realpath(dirname(__FILE__).DIRECTORY_SEPARATOR.'..'.DIRECTORY_SEPARATOR));
    define('LIB_PATH',BASE_PATH.'/lib/');
    define('STATIC_PATH',BASE_PATH.'/static/');
    define('CACHE_PATH',BASE_PATH.'/cache/');
    define('APP_PATH' ,BASE_PATH.'/app/');

    define('BNOW_HOST','127.0.0.1');
    define('BNOW_ABTEST_PORT',9188);
?>
