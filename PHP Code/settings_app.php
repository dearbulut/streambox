<?php $page_title="Settings App";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    if(!isset($_SESSION['admin_type'])){
        if($_SESSION['admin_type'] == 0){
            session_destroy();
            header( "Location:index.php");
            exit;
        }
    }
    
    $privacy_policy_file_path = getBaseUrl().'data.php?privacy_policy';
    $terms_file_path = getBaseUrl().'data.php?terms';
    
    $qry="SELECT * FROM tbl_settings where id='1'";
    $result=mysqli_query($mysqli,$qry);
    $settings_data=mysqli_fetch_assoc($result);
    
    if(isset($_POST['submit_general'])){
        
        $data = array(
            'app_email'  =>  $_POST['app_email'],
            'app_author'  =>  $_POST['app_author'],
            'app_contact'  =>  $_POST['app_contact'],
            'app_website'  =>  $_POST['app_website'],
            'app_developed_by'  =>  $_POST['app_developed_by'],
            'app_description'  =>  addslashes($_POST['app_description'])
        );
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class'] = "success";
        header( "Location:settings_app.php");
        exit;
        
    } else if(isset($_POST['app_submit'])){
        
        $data = array(
            'is_dowload'  => isset($_POST['is_dowload']) && $_POST['is_dowload']  ? 'true' : 'false',
            'is_rtl'  => isset($_POST['is_rtl']) && $_POST['is_rtl']  ? 'true' : 'false',
            'is_maintenance'  => isset($_POST['is_maintenance']) && $_POST['is_maintenance']  ? 'true' : 'false',
            'is_screenshot'  => isset($_POST['is_screenshot']) && $_POST['is_screenshot']  ? 'true' : 'false',
            'is_apk'  => isset($_POST['is_apk']) && $_POST['is_apk']  ? 'true' : 'false',
            'is_vpn'  => isset($_POST['is_vpn']) && $_POST['is_vpn']  ? 'true' : 'false',
            
            'is_select_xui'  => isset($_POST['is_select_xui']) && $_POST['is_select_xui']  ? 'true' : 'false',
            'is_select_stream'  => isset($_POST['is_select_stream']) && $_POST['is_select_stream']  ? 'true' : 'false',
            'is_select_playlist'  => isset($_POST['is_select_playlist']) && $_POST['is_select_playlist']  ? 'true' : 'false',
            'is_select_device_id'  => isset($_POST['is_select_device_id']) && $_POST['is_select_device_id']  ? 'true' : 'false',
            'is_select_single'  => isset($_POST['is_select_single']) && $_POST['is_select_single']  ? 'true' : 'false',
            'is_local_storage'  => isset($_POST['is_local_storage']) && $_POST['is_local_storage']  ? 'true' : 'false',
            'is_select_activation_code'  => isset($_POST['is_select_activation_code']) && $_POST['is_select_activation_code']  ? 'true' : 'false',

            'is_xui_dns'  => isset($_POST['is_xui_dns']) && $_POST['is_xui_dns']  ? 'true' : 'false',
            'is_xui_radio'  => isset($_POST['is_xui_radio']) && $_POST['is_xui_radio']  ? 'true' : 'false',
            
            'is_stream_dns'  => isset($_POST['is_stream_dns']) && $_POST['is_stream_dns']  ? 'true' : 'false',
            'is_stream_radio'  => isset($_POST['is_stream_radio']) && $_POST['is_stream_radio']  ? 'true' : 'false'
        );

        
        $settings_edit = Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg'] = "11";
        $_SESSION['class'] = "success";
        header("Location:settings_app.php");
        exit;
        
    } else if(isset($_POST['policy_submit'])){
        
        $data = array('app_privacy_policy'  =>  addslashes($_POST['app_privacy_policy']));
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class'] = "success";
        header( "Location:settings_app.php");
        exit;
        
    } else if(isset($_POST['terms_submit'])){
        
        $data = array('app_terms'  =>  addslashes($_POST['app_terms']));
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class'] = "success";
        header( "Location:settings_app.php");
        exit;
        
    } else if(isset($_POST['notification_submit'])) {
        
        $data = array(
          'onesignal_app_id' => trim($_POST['onesignal_app_id']),
          'onesignal_rest_key' => trim($_POST['onesignal_rest_key']),
        );
        
        $settings_edit = Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg'] = "11";
        $_SESSION['class'] = "success";
        header("Location:settings_app.php");
        exit;
        
    } else if(isset($_POST['app_update_submit'])){
        
        $data = array(
            'app_update_status'  =>  ($_POST['app_update_status']) ? 'true' : 'false',
            'app_new_version'  =>  trim($_POST['app_new_version']),
            'app_update_desc'  =>  trim($_POST['app_update_desc']),
            'app_redirect_url'  =>  trim($_POST['app_redirect_url'])
        );
        
        $settings_edit = Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success'; 
        header("Location:settings_app.php");
        exit;
        
    }
?>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb align-items-center">
                <li class="breadcrumb-item d-inline-flex"><a href="dashboard.php"><i class="ri-home-4-fill"></i></a></li>
                <li class="breadcrumb-item d-inline-flex active" aria-current="page"><?php echo (isset($page_title)) ? $page_title : "" ?></li>
            </ol>
        </nav>
            
        <div class="card">
            <div class="card-body p-0">                    
                <div class="nsofts-setting">
                    <div class="nsofts-setting__sidebar">
                        <div class="nav flex-column nav-pills" id="nsofts_setting" role="tablist" aria-orientation="vertical">
                            <button class="nav-link active" id="nsofts_setting_1" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_1" type="button" role="tab" aria-controls="nsofts_setting_1" aria-selected="true">
                                <i class="ri-list-settings-line"></i>
                                <span>General</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_2" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_2" type="button" role="tab" aria-controls="nsofts_setting_2" aria-selected="false">
                                <i class="ri-settings-5-line"></i>
                                <span>App Settings</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_4" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_4" type="button" role="tab" aria-controls="nsofts_setting_4" aria-selected="false">
                                <i class="ri-survey-line"></i>
                                <span>Privacy Policy</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_5" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_5" type="button" role="tab" aria-controls="nsofts_setting_5" aria-selected="false">
                                <i class="ri-survey-line"></i>
                                <span>Terms & Conditions</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_6" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_6" type="button" role="tab" aria-controls="nsofts_setting_6" aria-selected="false">
                                <i class="ri-notification-3-line"></i>
                                <span>Notification</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_7" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_7" type="button" role="tab" aria-controls="nsofts_setting_7" aria-selected="false">
                                <i class="ri-refresh-line"></i>
                                <span>App Update</span>
                            </button>

                        </div>
                    </div>
                    <div class="nsofts-setting__content">
                        <div class="tab-content">
                            
                            <!--General Settings-->
                            <div class="tab-pane fade show active" id="nsofts_setting_content_1" role="tabpanel" aria-labelledby="nsofts_setting_1" tabindex="0">
                                <form action="" name="settings_general" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">General Settings</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Email</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_email" id="app_email" value="<?php echo $settings_data['app_email']?>" >
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Author</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_author" id="app_author" value="<?php echo $settings_data['app_author']?>" >
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Contact</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_contact" id="app_contact" value="<?php echo $settings_data['app_contact']?>" >
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Website</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_website" id="app_website" value="<?php echo $settings_data['app_website']?>" >
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Developed By</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_developed_by" id="app_developed_by" value="<?php echo $settings_data['app_developed_by']?>" >
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Description</label>
                                        <div class="col-sm-10">
                                            <textarea name="app_description" id="app_description" class="form-control" ><?php echo stripslashes($settings_data['app_description']); ?></textarea>
                                        </div>
                                    </div>
                                    <button type="submit" name="submit_general" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--App Settings-->
                            <div class="tab-pane fade" id="nsofts_setting_content_2" role="tabpanel" aria-labelledby="nsofts_setting_2" tabindex="0">
                                <form action="" name="settings_app" method="POST" enctype="multipart/form-data">
                                    <div class="row mb-4">
                                        <div class="col-md-6">
                                             <h4 class="mb-4">Active Settings</h4>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Xtream codes</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_xui" name="is_select_xui" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_xui']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_xui" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">1-Stream</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_stream" name="is_select_stream" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_stream']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_stream" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">M3U Playlist</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_playlist" name="is_select_playlist" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_playlist']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_playlist" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Device ID</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_device_id" name="is_select_device_id" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_device_id']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_device_id" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Activation Code</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_activation_code" name="is_select_activation_code" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_activation_code']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_activation_code" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Single Stream</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_select_single" name="is_select_single" value="true" class="nsofts-switch__label" <?php if($settings_data['is_select_single']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_select_single" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Local Storage</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_local_storage" name="is_local_storage" value="true" class="nsofts-switch__label" <?php if($settings_data['is_local_storage']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_local_storage" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                        <div class="col-md-6">
                                            <h4 class="mb-4">App Settings</h4>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Download Video</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_dowload" name="is_dowload" value="true" class="nsofts-switch__label" <?php if($settings_data['is_dowload']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_dowload" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">RTL</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_rtl" name="is_rtl" value="true" class="nsofts-switch__label" <?php if($settings_data['is_rtl']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_rtl" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">App Maintenance</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_maintenance" name="is_maintenance" value="true" class="cbx hidden" <?php if($settings_data['is_maintenance']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_maintenance" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Sccrenshot block</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_screenshot" name="is_screenshot" value="true" class="cbx hidden" <?php if($settings_data['is_screenshot']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_screenshot" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Developer block</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_apk" name="is_apk" value="true" class="cbx hidden" <?php if($settings_data['is_apk']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_apk" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">VPN block</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_vpn" name="is_vpn" value="true" class="cbx hidden" <?php if($settings_data['is_vpn']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_vpn" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <div class="row mb-4">
                                        <div class="col-md-6">
                                            <h4 class="mb-4">Extream codes Settings</h4>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Active DNS</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_xui_dns" name="is_xui_dns" value="true" class="nsofts-switch__label" <?php if($settings_data['is_xui_dns']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_xui_dns" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Active Radio</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_xui_radio" name="is_xui_radio" value="true" class="nsofts-switch__label" <?php if($settings_data['is_xui_radio']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_xui_radio" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                        <div class="col-md-6">
                                            <h4 class="mb-4">1-Stream Settings</h4>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Active DNS</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_stream_dns" name="is_stream_dns" value="true" class="nsofts-switch__label" <?php if($settings_data['is_stream_dns']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_stream_dns" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-5 col-form-label">Active Radio</label>
                                                <div class="col-sm-7">
                                                    <div class="nsofts-switch d-flex align-items-center">
                                                        <input type="checkbox" id="is_stream_radio" name="is_stream_radio" value="true" class="nsofts-switch__label" <?php if($settings_data['is_stream_radio']=='true'){ echo 'checked'; }?>/>
                                                        <label for="is_stream_radio" class="nsofts-switch__label"></label>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                    <button type="submit" name="app_submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>

                            <!--Privacy Policy-->
                            <div class="tab-pane fade" id="nsofts_setting_content_4" role="tabpanel" aria-labelledby="nsofts_setting_4" tabindex="0">
                                <form action="" name="settings_policy" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Privacy Policy</h4>
                                    <div class="pb-clipboard mb-2">
                                        <span class="pb-clipboard__url"><span id="clipboard_policy"><?=$privacy_policy_file_path ?></span></span>
                                        <a class="pb-clipboard__link btn_policy" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                                <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                            </svg>
                                        </a>
                                    </div>
                                    <div>
                                        <textarea name="app_privacy_policy" id="app_privacy_policy" rows="5" class="nsofts-editor mb-4">
                                            <?php echo stripslashes($settings_data['app_privacy_policy']); ?>
                                        </textarea>
                                    </div>
                                    <button type="submit" name="policy_submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--Terms & Conditions-->
                            <div class="tab-pane fade" id="nsofts_setting_content_5" role="tabpanel" aria-labelledby="nsofts_setting_5" tabindex="0">
                                <form action="" name="settings_terms" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Terms & Conditions</h4>
                                    <div class="pb-clipboard mb-2">
                                        <span class="pb-clipboard__url"><span id="clipboard_terms"><?=$terms_file_path ?></span></span>
                                        <a class="pb-clipboard__link btn_terms" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                                <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                            </svg>
                                        </a>
                                    </div>
                                    <div>
                                        <textarea name="app_terms" id="app_terms" rows="5" class="nsofts-editor mb-4">
                                           <?php echo stripslashes($settings_data['app_terms']); ?>
                                        </textarea>
                                    </div>
                                    <button type="submit" name="terms_submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--Notification-->
                            <div class="tab-pane fade" id="nsofts_setting_content_6" role="tabpanel" aria-labelledby="nsofts_setting_6" tabindex="0">
                                <form action="" name="settings_notification" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Notification</h4>
                                    <div class="mb-3 row">
                                        <label class="col-sm-2 col-form-label">OneSignal App ID</label>
                                        <div class="col-sm-10">
                                            <div class="nsofts-input-icon nsofts-input-icon--left">
                                                <label for="onesignal_app_id" class="nsofts-input-icon__left">
                                                    <i class="ri-key-2-line"></i>
                                                </label>
                                                <input type="text" name="onesignal_app_id" id="onesignal_app_id" value="<?php echo $settings_data['onesignal_app_id']; ?>"  class="form-control">
                                            </div>
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label class="col-sm-2 col-form-label">OneSignal Rest Key</label>
                                        <div class="col-sm-10">
                                            <div class="nsofts-input-icon nsofts-input-icon--left">
                                                <label for="onesignal_rest_key" class="nsofts-input-icon__left">
                                                    <i class="ri-key-2-line"></i>
                                                </label>
                                                <input type="text" name="onesignal_rest_key" id="onesignal_rest_key" value="<?php echo $settings_data['onesignal_rest_key']; ?>"   class="form-control">
                                            </div>
                                        </div>
                                    </div>
                                    <button type="submit" name="notification_submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--App Update-->
                            <div class="tab-pane fade" id="nsofts_setting_content_7" role="tabpanel" aria-labelledby="nsofts_setting_7" tabindex="0">
                                <form action="" name="settings_app_update" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">App Update</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">ON/OFF</label>
                                        <div class="col-sm-10">
                                            <div class="nsofts-switch d-flex align-items-center">
                                                <input type="checkbox" id="app_update_status" name="app_update_status" value="true" class="nsofts-switch__label" <?php if($settings_data['app_update_status']=='true'){ echo 'checked'; }?>/>
                                                <label for="app_update_status" class="nsofts-switch__label"></label>
                                            </div>
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">New App Version Code</label>
                                        <div class="col-sm-10">
                                            <input type="number" min="1" name="app_new_version" id="app_new_version" required="" class="form-control" value="<?php echo $settings_data['app_new_version'];?>">
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Description</label>
                                        <div class="col-sm-10">
                                            <textarea name="app_update_desc"  class="form-control"><?php echo stripslashes($settings_data['app_update_desc']); ?></textarea>
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">App Link</label>
                                        <div class="col-sm-10">
                                            <input type="text" name="app_redirect_url" id="app_redirect_url" required="" class="form-control" value="<?php echo $settings_data['app_redirect_url'];?>">
                                        </div>
                                    </div>
                                    <button type="submit" name="app_update_submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>

                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?>
<script>
    document.addEventListener("DOMContentLoaded", function () {
        const tabLinks = document.querySelectorAll('#nsofts_setting .nav-link');
        const tabContents = document.querySelectorAll('.tab-pane');
        
        // Load last active tab from localStorage
        const activeTab = localStorage.getItem('activeTabApp') || '#nsofts_setting_content_1'; // Default to General tab
        const activeTabLink = document.querySelector(`#nsofts_setting [data-bs-target="${activeTab}"]`);
        const activeTabContent = document.querySelector(activeTab);
        
        // Check if the tab exists, otherwise fallback to General tab
        if (activeTabLink && activeTabContent) {
            tabLinks.forEach(link => link.classList.remove('active'));
            tabContents.forEach(content => content.classList.remove('show', 'active'));
            activeTabLink.classList.add('active');
            activeTabContent.classList.add('show', 'active');
        }

        // Listen for tab click events and save to localStorage
        tabLinks.forEach(link => {
            link.addEventListener('click', function () {
                const targetTab = this.getAttribute('data-bs-target');
                localStorage.setItem('activeTabApp', targetTab);
            });
        });
    });
</script>
<script type="text/javascript">
    $(document).ready(function(event) {
        
        $(document).on("click", ".btn_policy", function(e) {
            var el = document.getElementById('clipboard_policy');
            var successful = copyToClipboard(el);
            if (successful) {
                $.notify('Copied!', { position:"top right",className: 'success'} );
            } else {
                $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
            }
        });
        
        $(document).on("click", ".btn_terms", function(e) {
            var el = document.getElementById('clipboard_terms');
            var successful = copyToClipboard(el);
            if (successful) {
                $.notify('Copied!', { position:"top right",className: 'success'} );
            } else {
                $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
            }
        });

    });
</script>