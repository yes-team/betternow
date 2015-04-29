<?php
function smarty_modifier_byte_size($bytes=0) {
    $mb = 1024*1024;

    if ($bytes > $mb)
    {
        $output = sprintf ("%01.2f",$bytes/$mb) . " MB";
    }
    elseif ( $bytes >= 1024 )
    {
        $output = sprintf ("%01.0f",$bytes/1024) . " Kb";
    }
    else
    {
        $output = $bytes . " bytes";
    }

    return $output;
}
