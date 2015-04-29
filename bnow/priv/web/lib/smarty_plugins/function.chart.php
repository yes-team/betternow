<?php
function smarty_function_chart($params, $smarty) {
    if(isset($params['id']) && $params['id']){
        $element_id = $params['id'];
    }else{
        $element_id = ('chart_'.(intval($GLOBALS['smarty_charts_id']++)));
    }

    $width = isset($params['width']) ? $params['width'] : 480;
    $height = isset($params['height']) ? $params['height'] : 320;

    if(is_string($params['item'])){
        $params['item'] = array($params['item']);
    }

    $querys = array();

    foreach($params['item'] as $k=>$item){
        if(!is_numeric($k)){
            $querys[] = 'd['.urlencode($k).']='.urlencode($item);
        }else{
            $querys[] = 'd[]='.urlencode($item);
        }
    }

    if($params['end']){
        $querys[] = 'end='.urlencode($params['end']);
    }

    if($params['start']){
        $querys[] = 'start='.urlencode($params['start']);
    }

    if($params['step']){

        $querys[] = 'step='.urlencode($params['step']);
    }

    switch($params['mode']){
    case 'data':
        $html = smarty_function_chart_url('data?', $querys);
        break;
    case 'iframe':
        $html = "<iframe src=\""
            .smarty_function_chart_url('html?', $querys)."&width={$width}&height={$height}"
            ."\" style=\"border:none;width:{$width}px;height:{$height}px\"></iframe>";
        break;
    default:
        $url = smarty_function_chart_url('data?var=data&', $querys);
        $id_tag = $params['var']?"window.{$params['var']}=":'';
        $options = $params['options']?$params['options']:'{}';
        $data = $params['prepare']?"{$params['prepare']}(data)":'data';
        $html =<<<EOF
<div id="{$element_id}" style="width:{$width}px;height:{$height}px"></div>
<script>
\$(function(){
    \$.ajax('{$url}', {
        dataType:'script', 
        success: function(){  
EOF;

if (  $params['pre_data'] ) {
	$html.= $params['pre_data'];
}

$html .= <<<EOF
            var options = {
                chart: {

EOF;

if (  $params['charts_addon'] ) {
	$html.= $params['charts_addon'];
}

$html .= <<<EOF
                    animation: false,
                    shadow: false,
                    renderTo: '{$element_id}'
                },
        xAxis: {
            type: 'datetime',
            minPadding: 0.001,
            maxPadding: 0.001,
            dateTimeLabelFormats: {
                day: '%m-%d'
            },
            labels: {
                //step:1
            },
            //tickLength:20,
            tickInterval:86400000,
            lineColor: '#666'
            // tickPixelInterval: 150
        },

                series: {$data}
            };
//console.log( JSON.stringify(data[0]) )
            $.extend(true, options, $options);
            {$id_tag} new Highcharts.Chart(options);
        }
    });
});
</script>
EOF;
    }

    if(isset($params['dump']) && $params['dump']){
        return htmlspecialchars($html);
    }else{
        return $html;
    }
}

function smarty_function_chart_url($ext, $querys){
    $api_script = $_SERVER['BNOW_API_HOST'].'/api/';
    return $api_script.$ext.join('&', $querys);
}
