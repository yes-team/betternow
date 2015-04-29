<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');

unset($_SESSION['user']);
redirect('/');
