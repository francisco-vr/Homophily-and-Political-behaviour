<?php
error_reporting(E_ALL & ~E_NOTICE);
header('Content-Type: text/html; charset=utf-8');

$QID = getId();
$Homo1 = getHomo1();
$Homo2 = getHomo2();
$Homo3 = getHomo3();
$Homo4 = getHomo4();
$Homo5 = getHomo5();
$Homo6 = getHomo6();
$Homo7 = getHomo7();
$Digit1 = getDigit1();
$Digit2 = getDigit2();
$Digit3 = getDigit3();
$Digit4 = getDigit4();
$Digit5 = getDigit5();
$Digit6 = getDigit6();
$Digit7 = getDigit7();
$Digit8 = getDigit8();
$Digit9 = getDigit9();
$Digit10 = getDigit10();
$Digit11 = getDigit11();
$Digit12 = getDigit12();
$Digit13 = getDigit13();
$Digit14 = getDigit14();


// execute R script from shell

#logConsole('genderQ', $genderQ, true);
#logConsole('econQ', $econQ, true);
#logConsole('mode1Q', $mode1Q, true);
#logConsole('mode2Q', $mode2Q, true);
#logConsole('pgQ', $pgQ, true);
#logConsole('idQ', $idQ, true);

$command = "Rscript /var/www/html/Random-block-1.R $QID $Homo1 $Homo2 $Homo3 $Homo4 $Homo5 $Homo6 $Homo7 $Digit1 $Digit2 $Digit3 $Digit4 $Digit5 $Digit6 $Digit7 $Digit8 $Digit9 $Digit10 $Digit11 $Digit12 $Digit13 $Digit14";
$out = trim(shell_exec($command));




#echo(explode(',', $out));
$data = explode(',', $out);

#logConsole('data', $data, true);


#header('Content-Type: application/json');
#echo(json_encode($datas));

echo "data0=" . $data[0] . "&";
echo "data1=" . $data[1] . "&";
echo "data2=" . $data[2];

echo "data3=" . $data[3] . "&";
echo "data4=" . $data[4];



#======================================================================================

function getId(){
	if(isset($_GET['QID'])){
		$str = trim($_GET['QID']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo1(){
	if(isset($_GET['Homo1'])){
		$str = trim($_GET['Homo1']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo2(){
	if(isset($_GET['Homo2'])){
		$str = trim($_GET['Homo2']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo3(){
	if(isset($_GET['Homo3'])){
		$str = trim($_GET['Homo3']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo4(){
	if(isset($_GET['Homo4'])){
		$str = trim($_GET['Homo4']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo5(){
	if(isset($_GET['Homo5'])){
		$str = trim($_GET['Homo5']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getHomo6(){
	if(isset($_GET['Homo6'])){
		$str = trim($_GET['Homo6']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}
function getHomo7(){
	if(isset($_GET['Homo7'])){
		$str = trim($_GET['Homo7']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit1(){
	if(isset($_GET['Digit1'])){
		$str = trim($_GET['Digit1']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit2(){
	if(isset($_GET['Digit2'])){
		$str = trim($_GET['Digit2']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit3(){
	if(isset($_GET['Digit3'])){
		$str = trim($_GET['Digit3']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit4(){
	if(isset($_GET['Digit4'])){
		$str = trim($_GET['Digit4']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit5(){
	if(isset($_GET['Digit5'])){
		$str = trim($_GET['Digit5']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit6(){
	if(isset($_GET['Digit6'])){
		$str = trim($_GET['Digit6']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit7(){
	if(isset($_GET['Digit7'])){
		$str = trim($_GET['Digit7']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit8(){
	if(isset($_GET['Digit8'])){
		$str = trim($_GET['Digit8']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit9(){
	if(isset($_GET['Digit9'])){
		$str = trim($_GET['Digit9']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit10(){
	if(isset($_GET['Digit10'])){
		$str = trim($_GET['Digit10']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit11(){
	if(isset($_GET['Digit11'])){
		$str = trim($_GET['Digit11']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit12(){
	if(isset($_GET['Digit12'])){
		$str = trim($_GET['Digit12']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit13(){
	if(isset($_GET['Digit13'])){
		$str = trim($_GET['Digit13']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getDigit14(){
	if(isset($_GET['Digit14'])){
		$str = trim($_GET['Digit14']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}


function writeLog($msg){

	@file_put_contents('./runr.log', date('Y-m-d h:i:s') . "\t" . $QID . "\t" . $msg . "\n");
}

function logConsole($name, $data = NULL, $jsEval = FALSE)
 {
      if (! $name) return false;

      $isevaled = false;
      $type = ($data || gettype($data)) ? 'Type: ' . gettype($data) : '';

      if ($jsEval && (is_array($data) || is_object($data)))
      {
           $data = 'eval(' . preg_replace('#[\s\r\n\t\0\x0B]+#', '', json_encode($data)) . ')';
           $isevaled = true;
      }
      else
      {
           $data = json_encode($data);
      }

      # sanitalize
      $data = $data ? $data : '';
      $search_array = array("#'#", '#""#', "#''#", "#\n#", "#\r\n#");
      $replace_array = array('"', '', '', '\\n', '\\n');
      $data = preg_replace($search_array,  $replace_array, $data);
      $data = ltrim(rtrim($data, '"'), '"');
      $data = $isevaled ? $data : ($data[0] === "'") ? $data : "'" . $data . "'";
    }
?>

