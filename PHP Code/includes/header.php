<?php
include("includes/db_helper.php");
include("includes/session_check.php");

$currentFile = $_SERVER["SCRIPT_NAME"];
$parts = Explode('/', $currentFile);
$currentFile = $parts[count($parts) - 1];

$requestUrl = $_SERVER["REQUEST_URI"];
$urlparts = Explode('/', $requestUrl);
$redirectUrl = $urlparts[count($urlparts) - 1];

$mysqli->set_charset("utf8mb4");
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">

    <!-- Website Title -->
    <title><?php echo (isset($page_title)) ? $page_title.' | '.APP_NAME : APP_NAME; ?></title>
    
    <!-- Favicon --> 
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="32x32">
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="192x192">

    <!-- IOS Touch Icons -->
    <link rel="apple-touch-icon" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="152x152" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="180x180" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="167x167" href="images/<?php echo APP_LOGO;?>">

    <!-- Google fonts -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css2?family=Ubuntu:wght@300;400;500;700&display=swap" rel="stylesheet">

    <!-- Vendor styles -->
    <link rel="stylesheet" href="assets/vendors/bootstrap/bootstrap.min.css" type="text/css">
    <link rel="stylesheet" href="assets/vendors/perfect-scrollbar/perfect-scrollbar.min.css" type="text/css">
    <link rel="stylesheet" href="assets/vendors/remixicon/remixicon.min.css" type="text/css">
    <link rel="stylesheet" href="assets/vendors/quill/quill.min.css" type="text/css">
    <link rel="stylesheet" href="assets/vendors/select2/select2.min.css" type="text/css">

   <!-- Main style -->
   <link rel="stylesheet" href="assets/css/styles.css?v=1.0.0" type="text/css">
    
</head>
<body>
    
    <!-- Loader -->
    <div id="nsofts_loader">
        <div class="text-center">
            <i class="ri-3x ri-donut-chart-line nsofts-loader-icon"></i>
            <span class="d-block">Loading</span>
        </div>
    </div>

    <!-- Start: header -->
    <header id="nsofts_header">
        <a href="javascript:void(0)" id="nsofts_hamburger_top">
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 18 18" width="18" height="18" class="nsofts-hamburger">
                <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-1" />
                <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-2" />
                <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-3" />
            </svg>
        </a>
        <a href="javascript:void(0)" id="nsofts_brand" class="text-truncate"><?php echo APP_NAME;?></a>

        <!-- Header options -->
        <ul class="nsofts-header-nav ms-auto">
            <li class="nsofts-header-nav__item">
                <a href="javascript:void(0)" id="nsofts_theme_toggler" class="nsofts-header-nav__link">
                    <i class="ri-moon-fill nsofts-theme-dark"></i>
                    <i class="ri-sun-fill nsofts-theme-light"></i>
                </a>
            </li>
            <li class="nsofts-header-nav__item dropdown">
                <a href="javascript:void(0)" class="nsofts-header-nav__link" role="button" data-bs-toggle="dropdown" aria-expanded="false">
                    <i class="ri-user-fill"></i>
                </a>
                <div class="dropdown-menu mt-3">
                    <div class="px-3 py-2">
                        <div class="nsofts-avatar">
                            <div class="nsofts-avatar__image">
                                <?php if (PROFILE_IMG !='' AND  file_exists('images/'.PROFILE_IMG)) { ?>
                                    <img src="images/<?php echo PROFILE_IMG; ?>" alt="">
                                <?php } else { ?>
                                    <img src="assets/images/user_photo.png" alt="">
                                <?php } ?>
                            </div>
                            <div class="ps-2">
                                <span class="d-block fw-semibold"><?php echo (isset($_SESSION['admin_name'])) ? $_SESSION['admin_name'] : ""; ?></span>
                                <?php if(isset($_SESSION['admin_type'])){?>
                                    <?php if($_SESSION['admin_type'] == 1){?>
                                        <span class="d-block">ADMIN</span>
                                    <?php } else if($_SESSION['admin_type'] == 0){?>
                                        <span class="d-block">EDITOR</span>
                                    <?php } else if($_SESSION['admin_type'] == 3){?>
                                        <span class="d-block">SUPER ADMIN</span>
                                    <?php } ?>
                                <?php } ?>
                            </div>
                        </div>
                    </div>
                    <div class="dropdown-divider"></div>
                    <a class="dropdown-item dropdown-item--group" href="auth_my_profile.php">
                        <i class="ri-user-line"></i>
                        <span>My Profile</span>
                    </a>
                    <a class="dropdown-item dropdown-item--group" href="settings.php">
                        <i class="ri-settings-line"></i>
                        <span>Settings</span>
                    </a>
                    <div class="dropdown-divider"></div>
                    <a class="dropdown-item dropdown-item--group" href="logout.php">
                        <i class="ri-shut-down-line"></i>
                        <span>Logout</span>
                    </a>
                </div>
            </li>
        </ul>
    </header>
    <!-- End: header -->

    <!-- Start: sidebar -->
    <aside id="nsofts_sidebar">
        <nav class="nsofts-sidebar-nav" data-scroll="true">
            <ul>
                <li class="nsofts-sidebar-nav__item">
                    <a href="javascript:void(0)" id="nsofts_hamburger" class="nsofts-sidebar-menu">
                        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 18 18" width="18" height="18" class="nsofts-hamburger">
                            <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-1" />
                            <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-2" />
                            <line x1="0" y1="50%" x2="100%" y2="50%" class="nsofts-hamburger__bar-3" />
                        </svg>
                    </a>
                </li>
                
                <li class="nsofts-sidebar-nav__item">
                    <a href="dashboard.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "dashboard.php") { ?>active<?php } ?>">
                        <i class="ri-home-4-line nsofts-sidebar-nav__icon"></i>
                        <span class="nsofts-sidebar-nav__text">Dashboard</span>
                    </a>
                </li>
                
                <li class="nsofts-sidebar-nav__item">
                    <a href="manage_notification.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_notification.php" or $currentFile == "notification_onesignal.php") { ?>active<?php } ?>">
                        <i class="ri-notification-2-line nsofts-sidebar-nav__icon"></i>
                        <span class="nsofts-sidebar-nav__text">Notification</span>
                    </a>
                </li>
                
                <?php if(isset($_SESSION['admin_type']) && $_SESSION['admin_type'] != 0){?>
                
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_ads.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_ads.php" or $currentFile == "create_ads.php") { ?>active<?php } ?>">
                            <i class="ri-advertisement-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Custom Ads</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_dns.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_dns.php" or $currentFile == "create_dns.php") { ?>active<?php } ?>">
                            <i class="ri-xing-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Extream codes</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_dns_stream.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_dns_stream.php" or $currentFile == "create_dns_stream.php") { ?>active<?php } ?>">
                            <i class="ri-mist-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">1-Stream</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_blocklist.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_blocklist.php" or $currentFile == "create_blocklist.php") { ?>active<?php } ?>">
                            <i class="ri-spam-3-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">DNS Blocklist</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_users.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_users.php" or $currentFile == "create_user.php") { ?>active<?php } ?>">
                            <i class="ri-group-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Device ID to Login</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_users_activation.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_users_activation.php" or $currentFile == "create_user_activation.php") { ?>active<?php } ?>">
                            <i class="ri-group-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Activation Code to Login</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_report.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_report.php" or $currentFile == "create_report.php") { ?>active<?php } ?>">
                            <i class="ri-feedback-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Reports</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_data_deletion.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_data_deletion.php") { ?>active<?php } ?>">
                            <i class="ri-alarm-warning-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Store Policy</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="manage_admin.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "manage_admin.php" or $currentFile == "create_admin.php") { ?>active<?php } ?>">
                            <i class="ri-admin-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Admin</span>
                        </a>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item nsofts-has-menu">
                        <a href="javascript:void(0)" class="nsofts-sidebar-nav__link <?php if ($currentFile == "settings.php" or $currentFile == "settings_app.php" 
                        or $currentFile == "settings_app_ui.php" or $currentFile == "settings_api.php" or $currentFile == "settings_web.php" or $currentFile == "create_sidebar.php" or $currentFile == "settings_ads.php" or $currentFile == "manage_movie_ui.php") { ?>open active<?php } ?>">
                            <i class="ri-settings-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Settings</span>
                        </a>
                        <ul class="nsofts-submenu <?php if ($currentFile == "settings.php" or $currentFile == "settings_app.php" or $currentFile == "settings_app_ui.php" or $currentFile == "settings_api.php" 
                        or $currentFile == "settings_web.php" or $currentFile == "create_sidebar.php" or $currentFile == "settings_ads.php" or $currentFile == "manage_movie_ui.php") { ?>show<?php } ?>">
                            <li><a href="settings.php" class="nsofts-submenu__link <?php if ($currentFile == "settings.php") { ?>active<?php } ?>">Panel Settings</a></li>
                            <li><a href="settings_app.php" class="nsofts-submenu__link <?php if ($currentFile == "settings_app.php") { ?>active<?php } ?>">App Settings</a></li>
                            <li><a href="settings_app_ui.php" class="nsofts-submenu__link <?php if ($currentFile == "settings_app_ui.php") { ?>active<?php } ?>">App UI</a></li>
                            <li><a href="settings_api.php" class="nsofts-submenu__link <?php if ($currentFile == "settings_api.php") { ?>active<?php } ?>">Api Settings</a></li>
                            <li><a href="settings_web.php" class="nsofts-submenu__link <?php if ($currentFile == "settings_web.php" or $currentFile == "create_sidebar.php") { ?>active<?php } ?>">Web Settings</a></li>
                            <li><a href="settings_ads.php" class="nsofts-submenu__link <?php if ($currentFile == "settings_ads.php") { ?>active<?php } ?>">Advertisement</a></li>
                        </ul>
                    </li>
                    
                    <li class="nsofts-sidebar-nav__item">
                        <a href="verification.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "verification.php") { ?>active<?php } ?>">
                            <i class="ri-shield-check-line nsofts-sidebar-nav__icon"></i>
                            <span class="nsofts-sidebar-nav__text">Verification</span>
                        </a>
                    </li>
                    
                <?php } ?>
                
                <li class="nsofts-sidebar-nav__item">
                    <a href="urls.php" class="nsofts-sidebar-nav__link <?php if ($currentFile == "urls.php") { ?>active<?php } ?>">
                        <i class="ri-links-line nsofts-sidebar-nav__icon"></i>
                        <span class="nsofts-sidebar-nav__text">URLs</span>
                    </a>
                </li>
                
            </ul>
        </nav>
    </aside>
    <!-- End: sidebar -->