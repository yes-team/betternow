<?php
require($_SERVER['BNOW_BASEDIR'].'/lib/include.php');
    $xml = <<<EOF
<app>
    <name>myapp</name>
    <author>someone@somewhere.com</author>
    <copyright>mycompany</copyright>
    <desc>a short descrition of this app.</desc>

    <filter id="f1" class="http-scope" />
    <action id="a1" type="update" item="url/\${host}" value="status" function="sum" />
    <trigger filter="f1" action="a1" />

    <action id="a3" type="websocket" item="channel-1" />
    <trigger filter="f1" action="a3" />

    <filter id="f2" class="http-scope" >
        <assert left="\${uri}" right="^/goods-id/([0-9]+)/.*" test="match" >
            <set var="goods-id" value="a\${1}b" />
        </assert>
    </filter>

    <action id="a2" type="plugin" item="http_scope" />
    <trigger filter="f2" action="a2" />

    <filter id="f3" class="http-scope">
        <assert left="\${up-bytes} + \${down-bytes}" test="&gt;" right="15000" />
    </filter>
  
    <filter id="f4" class="http-scope">  
        <assert left="up-bytes" right="GET" test="isset"/>
    </filter>

    <plugin module="my_erlang_plugin" />

    <menu text="menu1" desc="" link="doc.php" />
    <menu text="menu2" desc="" link="step.php" />
    <menu text="menu3" desc="" link="appxml.php" />
    <menu text="menu4" desc="" link="websocket.php" />
    <menu text="menu5" desc="" link="online.php" />
</app>
EOF;

$vars =  array(
    'config'=>bnow::config(),
    'appxml'=>$xml,
    'TITLE'=>'开发手册',
); 
bnow_render::page('doc.html', $vars);
