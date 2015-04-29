<?php
class Smarty_Internal_Compile_Link extends Smarty_Internal_CompileBase {

    public $required_attributes = array('a');

    public $optional_attributes = array('m', 'f','args');

    public $shorttag_order = array('a','m','f');

    public function compile($args, $compiler, $parameter)
    {
        $link = '';
        $_attr = $this->getAttributes($compiler, $args);
        $a = trim($_attr['a'],'\'');
        $link = '<?php echo "/?a=".urlencode('.$_attr['a'].')'.( ($_attr['m']?'."&m=".urlencode('.$_attr['m'].')':'') ).( ($_attr['f']?'."&f=".urlencode('.$_attr['f'].')':'' ) . '; ?>');
        return $link;
    }
}

?>
