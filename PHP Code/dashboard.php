<?php $page_title="Dashboard";
    include("includes/header.php");
    require("includes/lb_helper.php");

    $qry_notification="SELECT COUNT(*) as num FROM tbl_notification";
    $total_notification = mysqli_fetch_array(mysqli_query($mysqli,$qry_notification));
    $total_notification = $total_notification['num'];
    
    $qry_custom_ads="SELECT COUNT(*) as num FROM tbl_custom_ads";
    $total_custom_ads = mysqli_fetch_array(mysqli_query($mysqli,$qry_custom_ads));
    $total_custom_ads = $total_custom_ads['num'];

    $qry_dns="SELECT COUNT(*) as num FROM tbl_xui_dns";
    $total_xui_dns = mysqli_fetch_array(mysqli_query($mysqli,$qry_dns));
    $total_xui_dns = $total_xui_dns['num'];
    
    $qry_stream_dns="SELECT COUNT(*) as num FROM tbl_stream_dns";
    $total_stream_dns = mysqli_fetch_array(mysqli_query($mysqli,$qry_stream_dns));
    $total_stream_dns = $total_stream_dns['num'];
    
    $qry_blocklist="SELECT COUNT(*) as num FROM tbl_xui_dns_block";
    $total_blocklist = mysqli_fetch_array(mysqli_query($mysqli,$qry_blocklist));
    $total_blocklist = $total_blocklist['num'];
    
    $qry_activation_code="SELECT COUNT(*) as num FROM tbl_activation_code";
    $total_activation_code = mysqli_fetch_array(mysqli_query($mysqli,$qry_activation_code));
    $total_activation_code = $total_activation_code['num'];

    $qry_device_id="SELECT COUNT(*) as num FROM tbl_users";
    $total_device_id = mysqli_fetch_array(mysqli_query($mysqli,$qry_device_id));
    $total_device_id = $total_device_id['num'];

    $qry_re="SELECT COUNT(*) as num FROM tbl_reports";
    $total_reports = mysqli_fetch_array(mysqli_query($mysqli,$qry_re));
    $total_reports = $total_reports['num'];
    
    $qry_store="SELECT COUNT(*) as num FROM tbl_policy_deletion";
    $total_store_policy = mysqli_fetch_array(mysqli_query($mysqli, $qry_store));
    $total_store_policy = $total_store_policy['num'];
    
    $qry_admin="SELECT COUNT(*) as num FROM tbl_admin";
    $total_admin = mysqli_fetch_array(mysqli_query($mysqli, $qry_admin));
    $total_admin = $total_admin['num'];
    
    
    $sql_reports="SELECT * FROM tbl_reports ORDER BY tbl_reports.`id` DESC LIMIT 8";
    $result_reports=mysqli_query($mysqli,$sql_reports);
    
    $qry="SELECT * FROM tbl_settings where id='1'";
    $result=mysqli_query($mysqli,$qry);
    $settings_data=mysqli_fetch_assoc($result);
    
    
?>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        
        <div class="row g-4">
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Notification</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_notification); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-warning text-white text-lg">
                                    <i class="ri-notification-2-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Custom Ads</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_custom_ads); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-danger text-white text-lg">
                                    <i class="ri-advertisement-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Extream codes</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_xui_dns); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-success text-white text-lg">
                                    <i class="ri-xing-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">1-Stream</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_stream_dns); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-info text-white text-lg">
                                    <i class="ri-mist-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Blocklist</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_blocklist); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-primary text-white text-lg">
                                    <i class="ri-spam-3-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Device ID</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_device_id); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-warning text-white text-lg">
                                    <i class="ri-group-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Activation Code</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_activation_code); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-danger text-white text-lg">
                                    <i class="ri-group-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Reports</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_reports); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-success text-white text-lg">
                                    <i class="ri-feedback-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Store Policy</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_store_policy); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-info text-white text-lg">
                                    <i class="ri-alarm-warning-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="col-xl-3 col-sm-6 col-12">
                <div class="card card-badge">
                    <div class="card-body">
                        <div class="row">
                            <div class="col">
                                <span class="h6 font-semibold text-muted text-sm d-block mb-2">Admin</span>
                                <span class="h3 font-bold mb-0"><?php echo thousandsNumberFormat($total_admin); ?></span>
                            </div>
                            <div class="col-auto">
                                <div class="icon-shape bg-primary text-white text-lg">
                                    <i class="ri-admin-line"></i>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div class="row g-4 mt-2">
            <div class="col-lg-7 col-md-6">
                <div class="card card-dashboard h-100">
                    <div class="card-body p-4">
                        <div class="d-flex justify-content-between align-items-center">
                            <div class="me-2">
                                <h5 class="mb-4">App Theme</h5>
                            </div>
                        </div>
                        <div>
                            <?php  if ($settings_data['is_theme'] == '2') { ?>
                                <img src="assets/images/themes/Glossy.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else if ($settings_data['is_theme'] == '3') { ?>
                                <img src="assets/images/themes/BlackPanther.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else if ($settings_data['is_theme'] == '4') { ?>
                                <img src="assets/images/themes/MovieUI.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else if ($settings_data['is_theme'] == '5') { ?>
                                <img src="assets/images/themes/VUI.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else if ($settings_data['is_theme'] == '6') { ?>
                                <img src="assets/images/themes/ChristmasUI.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else if ($settings_data['is_theme'] == '7') { ?>
                                <img src="assets/images/themes/HalloweenUI.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;" >
                            <?php } else { ?>
                                <img src="assets/images/themes/OneUI.png" alt="" style="width: 100%; height: auto; min-width: 100%; min-height: 100%; max-width: 100%; border-radius: 10px;">
                            <?php } ?>
                        </div>
                    </div>
                </div>
            </div>  
            <div class="col-lg-5 col-md-6">
                <div class="card card-dashboard h-100">
                    <div class="card-body p-4">
                        <div class="d-flex align-items-center justify-content-between">
                            <h5 class="mb-0">New Reports</h5>
                        </div>
                        <?php if(mysqli_num_rows($result_reports) > 0){ ?>
                        
                            <?php $i=0; while($row=mysqli_fetch_array($result_reports)) { ?>
                            
                                <div class="d-flex align-items-center mt-4">
                                    <span class="d-block fw-semibold"><?php echo $row['report_title'];?></span>
                                    <span><td><?php echo calculate_time_span($row['report_on']);?></td></span>
                                </div>
                                
                            <?php $i++; } ?> 
                            
                        <?php } else { ?>
                            <ul class="p-2">
                                <h3 class="text-center">No data found !</h3>
                            </ul>
                        <?php } ?>
                    </div>
                </div>
            </div> 
        </div>

    </div>
</main>
<!-- End: main -->
<?php include("includes/footer.php");?> 