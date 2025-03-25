<?php if(count(get_included_files()) == 1) exit("No direct script access allowed");

define("PRODUCT_ID", '52621164');
define("PRODUCT_URL",'https://1.envato.market/Y9W6xm');

function getAPIRequest($data){
    $curl = curl_init();
    curl_setopt($curl, CURLOPT_POST, 1);
    if($data){
        curl_setopt($curl, CURLOPT_POSTFIELDS, http_build_query($data));
    }
	curl_setopt($curl, CURLOPT_URL, "https://api.nemosofts.com/token/MITB3PVT6ZRLUA3GBJCU5S78S/v1/api.php");
	curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
	curl_setopt($curl, CURLOPT_CONNECTTIMEOUT, 30); 
	curl_setopt($curl, CURLOPT_TIMEOUT, 30);
	$result = curl_exec($curl);
	curl_close($curl);
	return $result;
}

function getLatestVersion(){
    $data_array =  array(
    	'method_name' => "latest_version",
        'item_id' => PRODUCT_ID
    );
    $get_data = getAPIRequest($data_array);
    $response = json_decode($get_data, true);
    return $response;
}

function checkUpdate(){
	$data_array =  array(
	    "item_id"  => PRODUCT_ID
	);
	$get_data = getAPIRequest($data_array);
	$response = json_decode($get_data, true);
	return $response;
}

function verifyEnvatoPurchaseCode($license){
    $data_array =  array(
        'method_name' => "envato_purchase_code",
        "item_id"  => PRODUCT_ID,
		"license_code" => $license,
	);
    $get_data = getAPIRequest($data_array);
    $response = json_decode($get_data, true);
    return $response;
}

function verifyLicenseAndroid($license, $api_key, $package_name){
    $get_base_url = getBaseUrlPanel();
    $data_array =  array(
        'method_name' => 'android_app',
        'envato_purchase_code' => $license,
        'api_key' => $api_key,
        'buyer_admin_url' => $get_base_url,
        'package_name' => $package_name
    );
    $get_data = getAPIRequest($data_array);
    $response = json_decode($get_data, true);
    return $response;
}

function deactivateLicense($deactivate_password){
    $current_path = realpath(__DIR__);
	$license_file = $current_path.'/.lic';
	$data_array =  array(
	    'method_name' => "deactivate_license",
	    "deactivate_password" => $deactivate_password
	);
	$get_data = getAPIRequest($data_array);
	$response = json_decode($get_data, true);
	if($response['status']){
		@chmod($license_file, 0777);
		if(is_writeable($license_file)){
			unlink($license_file);
		}
	}
	return $response;
}

function activateLicense($license, $client, $create_lic = true){
    $get_base_url = getBaseUrlPanel();
    $data_array =  array(
        'method_name' => "activate_license",
		"item_id"  => PRODUCT_ID,
		"license_code" => $license,
		"client_name" => $client,
        'base_url' => $get_base_url,
	);
	$get_data = getAPIRequest($data_array);
	$response = json_decode($get_data, true);
	$current_path = realpath(__DIR__);
	$license_file = $current_path.'/.lic';
	if(!empty($create_lic)){
		if($response['status']){
			$licfile = trim('B4N1L9C5ITMKIRL');
			file_put_contents($license_file, $licfile, LOCK_EX);
		} else {
			@chmod($license_file, 0777);
			if(is_writeable($license_file)){
				unlink($license_file);
			}
		}
	}
	return $response;
}

function getIpFromThirdParty(){
	$curl = curl_init ();
	curl_setopt($curl, CURLOPT_URL, "http://ipecho.net/plain");
	curl_setopt($curl, CURLOPT_HEADER, 0);
	curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
	curl_setopt($curl, CURLOPT_CONNECTTIMEOUT, 10); 
	curl_setopt($curl, CURLOPT_TIMEOUT, 10);
	$response = curl_exec($curl);
	curl_close($curl);
	return $response;
}

function getBaseUrlPanel($array = false){

    $protocol = "http";
    $host = "";
    $port = "";
    $dir = "";

    // Get protocol
    if (array_key_exists("HTTPS", $_SERVER) && $_SERVER["HTTPS"] != "") {
        if ($_SERVER["HTTPS"] == "on") {
            $protocol = "https";
        } else {
            $protocol = "http";
        }
    } elseif (array_key_exists("REQUEST_SCHEME", $_SERVER) && $_SERVER["REQUEST_SCHEME"] != "") {
        $protocol = $_SERVER["REQUEST_SCHEME"];
    }

    // Get host
    if (array_key_exists("HTTP_X_FORWARDED_HOST", $_SERVER) && $_SERVER["HTTP_X_FORWARDED_HOST"] != "") {
        $host = trim(end(explode(',', $_SERVER["HTTP_X_FORWARDED_HOST"])));
    } elseif (array_key_exists("SERVER_NAME", $_SERVER) && $_SERVER["SERVER_NAME"] != "") {
        $host = $_SERVER["SERVER_NAME"];
    } elseif (array_key_exists("HTTP_HOST", $_SERVER) && $_SERVER["HTTP_HOST"] != "") {
        $host = $_SERVER["HTTP_HOST"];
    } elseif (array_key_exists("SERVER_ADDR", $_SERVER) && $_SERVER["SERVER_ADDR"] != "") {
        $host = $_SERVER["SERVER_ADDR"];
    }
    //elseif(array_key_exists("SSL_TLS_SNI", $_SERVER) && $_SERVER["SSL_TLS_SNI"] != "") { $host = $_SERVER["SSL_TLS_SNI"]; }

    // Get port
    if (array_key_exists("SERVER_PORT", $_SERVER) && $_SERVER["SERVER_PORT"] != "") {
        $port = $_SERVER["SERVER_PORT"];
    } elseif (stripos($host, ":") !== false) {
        $port = substr($host, (stripos($host, ":") + 1));
    }
    // Remove port from host
    $host = preg_replace("/:\d+$/", "", $host);

    // Get dir
    if (array_key_exists("SCRIPT_NAME", $_SERVER) && $_SERVER["SCRIPT_NAME"] != "") {
        $dir = $_SERVER["SCRIPT_NAME"];
    } elseif (array_key_exists("PHP_SELF", $_SERVER) && $_SERVER["PHP_SELF"] != "") {
        $dir = $_SERVER["PHP_SELF"];
    } elseif (array_key_exists("REQUEST_URI", $_SERVER) && $_SERVER["REQUEST_URI"] != "") {
        $dir = $_SERVER["REQUEST_URI"];
    }
    // Shorten to main dir
    if (stripos($dir, "/") !== false) {
        $dir = substr($dir, 0, (strripos($dir, "/") + 1));
    }

    // Create return value
    if (!$array) {
        if ($port == "80" || $port == "443" || $port == "") {
            $port = "";
        } else {
            $port = ":" . $port;
        }
        return htmlspecialchars($protocol . "://" . $host . $port . $dir, ENT_QUOTES);
    } else {
        return ["protocol" => $protocol, "host" => $host, "port" => $port, "dir" => $dir];
    }
}

?>