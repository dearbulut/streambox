<?php 
include("includes/db_helper.php");
include("includes/lb_helper.php"); 
include("language/api_language.php"); 

error_reporting(0);

$file_path = getBaseUrl();

$mysqli->set_charset('utf8mb4');

date_default_timezone_set("Asia/Colombo");

define("PACKAGE_NAME",$settings_details['envato_package_name']);

// For Api header
$API_NAME = 'NEMOSOFTS_APP';

}

$get_helper = get_api_data($_POST['data']);

// App details
if($get_helper['helper_name']=="app_details"){
    
    $jsonObj= array();
	$data_arr= array();
    
    $sql="SELECT * FROM tbl_settings WHERE id='1'";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    while($data = mysqli_fetch_assoc($result)){
        
        // App Details
        $data_arr['app_email'] = $data['app_email'];
        $data_arr['app_author'] = $data['app_author'];
        $data_arr['app_contact'] = $data['app_contact'];
        $data_arr['app_website'] = $data['app_website'];
        $data_arr['app_description'] = $data['app_description'];
        $data_arr['app_developed_by'] = $data['app_developed_by'];
        
        // Envato
        $data_arr['envato_api_key'] = $data['envato_api_key'];
        
        // is_
        $data_arr['is_rtl'] = $data['is_rtl'];
        $data_arr['is_maintenance'] = $data['is_maintenance'];
        $data_arr['is_screenshot'] = $data['is_screenshot'];
        $data_arr['is_apk'] = $data['is_apk'];
        $data_arr['is_vpn'] = $data['is_vpn'];
        $data_arr['is_xui_dns'] = $data['is_xui_dns'];
        $data_arr['is_xui_radio'] = $data['is_xui_radio'];
        $data_arr['is_stream_dns'] = $data['is_stream_dns'];
        $data_arr['is_stream_radio'] = $data['is_stream_radio'];
        $data_arr['is_local_storage'] = $data['is_local_storage'];
        $data_arr['is_select_activation_code'] = $data['is_select_activation_code'];
        
        // is_select
        $data_arr['is_select_xui'] = $data['is_select_xui'];
        $data_arr['is_select_stream'] = $data['is_select_stream'];
        $data_arr['is_select_playlist'] = $data['is_select_playlist'];
        $data_arr['is_select_device_id'] = $data['is_select_device_id'];
        $data_arr['is_select_single'] = $data['is_select_single'];
        
        // Ads Network
        $data_arr['ad_network'] = $data['ad_network'];
        
        $data_arr['publisher_id'] = ($data['ad_network'] == 'admob') ? $data_arr['publisher_id'] : "";
        
        // BannerAds
        $data_arr['banner_ad'] = $data['banner_ad'];
        if ($data['ad_network'] == 'admob') {
            $data_arr['banner_ad_id'] = $data['banner_ad_id'];
        } else {
            $data_arr['banner_ad_id'] = '';
        }
       
        // InterstitalAds
        $data_arr['interstital_ad'] = $data['interstital_ad'];
        $data_arr['interstital_ad_click'] = $data['interstital_ad_click'];
        if ($data['ad_network'] == 'admob') {
            $data_arr['interstital_ad_id'] = $data['interstital_ad_id'];
        } else {
            $data_arr['interstital_ad_id'] = '';
        }
        
        // AppUpdate
        $data_arr['app_update_status'] = $data['app_update_status'];
        $data_arr['app_new_version'] = $data['app_new_version'];
        $data_arr['app_update_desc'] = $data['app_update_desc'];
        $data_arr['app_redirect_url'] = $data['app_redirect_url'];
        
        // Custom Ads
        $data_arr['custom_ads'] = $data['custom_ads'];
        $data_arr['custom_ads_clicks'] = $data['custom_ads_clicks'];
        
        // App Themes
        $data_arr['is_theme'] = $data['is_theme'];
        $data_arr['is_epg'] = $data['is_epg'];
        
        // Dowload
        $data_arr['is_download'] = $data['is_dowload'];
       
        // TMDb Key
        $data_arr['tmdb_key'] = $data['account_delete_intruction'];
        
        array_push($jsonObj,$data_arr);
    }
    $row['details'] = $jsonObj;
    
    mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
    $sql="SELECT * FROM tbl_advertisement WHERE id='1'";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    while($data = mysqli_fetch_assoc($result)){
        
        $data_arr['ad_status'] = $data['ad_status'];
        
        // Ads Network
        $data_arr['ad_network'] = $data['ad_network'];
        
        // Publisher ID
        $data_arr['publisher_id'] = ($data['ad_network'] == 'admob') ? $data_arr['publisher_id'] : "";
        
        $data_arr['banner_movie'] = $data['banner_movie'];
        $data_arr['banner_series'] = $data['banner_series'];
        $data_arr['banner_epg'] = $data['banner_epg'];
        
        $data_arr['interstital_ad'] = $data['interstitial_post_list'];
        
        $data_arr['reward_ad_on_movie'] = $data['reward_ad_on_movie'];
        $data_arr['reward_ad_on_episodes'] = $data['reward_ad_on_episodes'];
        $data_arr['reward_ad_on_live'] = $data['reward_ad_on_live'];
        $data_arr['reward_ad_on_single'] = $data['reward_ad_on_single'];
        $data_arr['reward_ad_on_local'] = $data['reward_ad_on_local'];
        
        // BannerAds
        if ($data['ad_network'] == 'admob') {
            $data_arr['banner_ad_id'] = $data['admob_banner_unit_id'];
        } else {
            $data_arr['banner_ad_id'] = '';
        }
       
        // InterstitalAds
        $data_arr['interstital_ad_click'] = $data['interstital_ad_click'];
        if ($data['ad_network'] == 'admob') {
            $data_arr['interstital_ad_id'] = $data['admob_interstitial_unit_id'];
        } else {
            $data_arr['interstital_ad_id'] = '';
        }
        
        // RewardAds
        $data_arr['reward_minutes'] = $data['reward_minutes'];
        if ($data['ad_network'] == 'admob') {
            $data_arr['reward_ad_id'] = $data['admob_reward_ad_unit_id'];
        } else {
            $data_arr['reward_ad_id'] = '';
        }
        
        array_push($jsonObj,$data_arr);
    }
    $row['ads_details'] = $jsonObj;
    
    mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
	$sql="SELECT * FROM tbl_xui_dns WHERE tbl_xui_dns.status='1' ORDER BY tbl_xui_dns.id DESC";
	$result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while ($data2 = mysqli_fetch_assoc($result)){
            
            $data_arr['id'] = $data2['id'];
            $data_arr['dns_title'] = $data2['dns_title'];
            $data_arr['dns_base'] = $data2['dns_base'];
            
            array_push($jsonObj, $data_arr);
        }
    }
	$row['xui_dns'] = $jsonObj;
	
	mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
	$sql="SELECT * FROM tbl_stream_dns WHERE tbl_stream_dns.status='1' ORDER BY tbl_stream_dns.id DESC";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while ($data3 = mysqli_fetch_assoc($result)){
            
            $data_arr['id'] = $data3['id'];
            $data_arr['dns_title'] = $data3['dns_title'];
            $data_arr['dns_base'] = $data3['dns_base'];
            
            array_push($jsonObj, $data_arr);
        }
    }
	$row['stream_dns'] = $jsonObj;

    mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
	$sql="SELECT * FROM tbl_xui_dns_block WHERE tbl_xui_dns_block.status='1' ORDER BY tbl_xui_dns_block.id DESC";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while ($data4 = mysqli_fetch_assoc($result)){
            $data_arr['dns_base'] = $data4['dns_base'];
            array_push($jsonObj, $data_arr);
        }
    }
	$row['xui_dns_block'] = $jsonObj;

    mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
	$sql="SELECT * FROM tbl_custom_ads WHERE tbl_custom_ads.status='1' AND tbl_custom_ads.ads_type ='popup' ORDER BY RAND() DESC LIMIT 1";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while ($data5 = mysqli_fetch_assoc($result)){
            
            $data_arr['ads_type'] = $data5['ads_type'];
            $data_arr['ads_title'] = $data5['ads_title'];
            $data_arr['ads_image'] =  $file_path.'images/'.$data5['ads_image'];
            $data_arr['ads_redirect_type'] = $data5['ads_redirect_type'];
            $data_arr['ads_redirect_url'] = $data5['ads_redirect_url'];
            
        	array_push($jsonObj, $data_arr);
        }
    }
	$row['popup_ads'] = $jsonObj;
	
	mysqli_free_result($result);
	$jsonObj = array();
	$data_arr = array();
	
	$sql="SELECT * FROM tbl_notification WHERE tbl_notification.`id` ORDER BY tbl_notification.`id` DESC";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while ($data6 = mysqli_fetch_assoc($result)){
            
            $data_arr['id'] = $data6['id'];
          	$data_arr['notification_title'] = $data6['notification_title'];
          	$data_arr['notification_msg'] = $data6['notification_msg']; 
          	$data_arr['notification_description'] = $data6['notification_description']; 
            $data_arr['notification_on'] = calculate_time_span($data6['notification_on'],true);
            array_push($jsonObj, $data_arr);
        }
    }
	$row['notification_data'] = $jsonObj;

    $set[$API_NAME] = $row;
	header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
	die();
}
else if($get_helper['helper_name']=="post_report"){
    
    $jsonObj= array();

    $user_name=cleanInput($get_helper['user_name']);
	$user_pass=cleanInput($get_helper['user_pass']);
	$report_title=cleanInput($get_helper['report_title']);
	$report_msg=cleanInput($get_helper['report_msg']);
    
	$data = array(
        'user_name' => $user_name,
        'user_pass'  =>  $user_pass,
        'report_title'  =>  $report_title,
        'report_msg'  =>  $report_msg,
        'report_on'  =>  strtotime(date('d-m-Y h:i:s A'))
    );
    $qry = Insert('tbl_reports',$data);
    
	$set[$API_NAME][]=array('MSG'=> $app_lang['report_success'],'success'=> '1');
  	header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
	die();
}
else if($get_helper['helper_name']=="get_interstitial") {

	$jsonObj= array();	

    $sql="SELECT * FROM tbl_custom_ads WHERE tbl_custom_ads.status='1' AND tbl_custom_ads.ads_type ='interstitial' ORDER BY RAND() DESC LIMIT 1";
	$result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
	if(mysqli_num_rows($result) > 0){
	    while($data = mysqli_fetch_assoc($result)){
          	$row['ads_image'] = $file_path.'images/'.$data['ads_image'];
          	$row['ads_redirect_type'] = $data['ads_redirect_type'];
          	$row['ads_redirect_url'] = $data['ads_redirect_url'];
          	
          	array_push($jsonObj,$row);
        }
	}
	$set[$API_NAME] = $jsonObj;
	header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
	die();
}
else if($get_helper['helper_name']=="get_poster") {

	$jsonObj= array();	

    $sql="SELECT * FROM tbl_poster_gallery WHERE tbl_poster_gallery.status='1' AND tbl_poster_gallery.poster_type ='movie_ui' ORDER BY RAND() DESC";
	$result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while($data = mysqli_fetch_assoc($result)){
            $row['poster_image'] = $file_path.'images/'.$data['poster_image'];
            array_push($jsonObj,$row);
        }
    }
	$set[$API_NAME] = $jsonObj;
	header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
	die();
}
else if($get_helper['helper_name']=="get_device_user"){
    
    $jsonObj= array();
    
    $device_id=isset($get_helper['device_id']) ? trim($get_helper['device_id']) : 0;

    $sql="SELECT * FROM tbl_users WHERE tbl_users.status='1' AND tbl_users.device_id='".$device_id."' ORDER BY tbl_users.id DESC";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while($data = mysqli_fetch_assoc($result)){
            
            $row['id'] = $data['id'];
            $row['user_type'] = $data['user_type'];
            $row['user_name'] = $data['user_name'];
            $row['user_password'] = $data['user_password'];
            $row['dns_base'] = $data['dns_base'];
            $row['device_id'] = $data['device_id'];
            array_push($jsonObj,$row);
        }
    }
    $set[$API_NAME] = $jsonObj;
    header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
    die();
}

else if($get_helper['helper_name']=="get_activation_code"){
    
    $jsonObj= array();
    
    $activation_code = isset($get_helper['activation_code']) ? trim($get_helper['activation_code']) : 0;

    $sql="SELECT * FROM tbl_activation_code WHERE tbl_activation_code.status='1' AND tbl_activation_code.activation_code='".$activation_code."' ORDER BY tbl_activation_code.id DESC";
    $result = mysqli_query($mysqli,$sql) or die(mysqli_error($mysqli));
    if(mysqli_num_rows($result) > 0){
        while($data = mysqli_fetch_assoc($result)){
            
            $row['id'] = $data['id'];
            $row['user_type'] = $data['user_type'];
            $row['user_name'] = $data['user_name'];
            $row['user_password'] = $data['user_password'];
            $row['dns_base'] = $data['dns_base'];
            $row['activation_code'] = $data['activation_code'];
            array_push($jsonObj,$row);
        }
    }
    $set[$API_NAME] = $jsonObj;
    header( 'Content-Type: application/json; charset=utf-8' );
    echo $val= str_replace('\\/', '/', json_encode($set,JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
    die();
}

else {
    $get_helper = get_api_data($_POST['data']);
}
