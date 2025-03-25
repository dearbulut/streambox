<?php $page_title="Settings App UI";
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
    
    $qry="SELECT * FROM tbl_settings where id='1'";
    $result=mysqli_query($mysqli,$qry);
    $settings_data=mysqli_fetch_assoc($result);
    
    if(isset($_POST['app_theme'])){
        
        $data = array('is_theme'  =>  $_POST['is_theme']);
        
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success'; 
        header( "Location:settings_app_ui.php");
        exit;
    } else if(isset($_POST['app_theme_epg'])){
        
        $data = array('is_epg'  =>  $_POST['is_epg']);
        
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success'; 
        header( "Location:settings_app_ui.php");
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
                                <i class="ri-pencil-ruler-2-line"></i>
                                <span>App Themes</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_2" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_2" type="button" role="tab" aria-controls="nsofts_setting_2" aria-selected="false">
                                <i class="ri-settings-5-line"></i>
                                <span>EPG Page UI</span>
                            </button>
                        
                        </div>
                    </div>
                    <div class="nsofts-setting__content">
                        <div class="tab-content">
                            
                            <!--App Themes-->
                            <div class="tab-pane fade show active" id="nsofts_setting_content_1" role="tabpanel" aria-labelledby="nsofts_setting_1" tabindex="0">
                                <form action="" name="settings_theme" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">App Themes</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">App UI</label>
                                        <div class="col-sm-10">
                                            <select name="is_theme" id="is_theme" class="form-select" required>
                                                <option value="">-- Select App UI --</option>
                                                <option value="1" <?php if ($settings_data['is_theme'] == '1') { ?>selected<?php } ?>>OneUI</option>
                                                <option value="2" <?php if ($settings_data['is_theme'] == '2') { ?>selected<?php } ?>>Glossy</option>
                                                <option value="3" <?php if ($settings_data['is_theme'] == '3') { ?>selected<?php } ?>>Black Panther</option>
                                                <option value="4" <?php if ($settings_data['is_theme'] == '4') { ?>selected<?php } ?>>MovieUI</option>
                                                <option value="5" <?php if ($settings_data['is_theme'] == '5') { ?>selected<?php } ?>>VUI</option>
                                                <option value="6" <?php if ($settings_data['is_theme'] == '6') { ?>selected<?php } ?>>ChristmasUI</option>
                                                <option value="7" <?php if ($settings_data['is_theme'] == '7') { ?>selected<?php } ?>>HalloweenUI</option>
                                            </select>
                                        </div>
                                    </div>
                                    <button type="submit" name="app_theme" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                    <div class="row g-4 mb-3 mt-3">
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/OneUI.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/OneUI.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">OneUI</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/ChristmasUI.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/ChristmasUI.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">ChristmasUI</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/VUI.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/VUI.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">VUI</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/HalloweenUI.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/HalloweenUI.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">HalloweenUI</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <div class="nsofts-image-card">
                                                <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                    <img src="assets/images/themes/MovieUI.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                </div>
                                                <div class="nsofts-image-card__content">
                                                    <div class="position-relative">
                                                        <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                            <span class="d-block text-truncate fs-6 fw-semibold pe-2">MovieUI</span>
                                                            <div class="nsofts-image-card__option d-flex">
                                                                <a href="manage_movie_ui.php" class="btn border-0 text-danger" data-bs-toggle="tooltip" data-bs-placement="top" title="Edit Theme">
                                                                    <i class="ri-pencil-fill"></i>
                                                                </a>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/Glossy.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/Glossy.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">Glossy</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <a href="assets/images/themes/BlackPanther.png" data-bs-toggle="tooltip" data-bs-placement="top" title="View UI">
                                                <div class="nsofts-image-card">
                                                    <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                        <img src="assets/images/themes/BlackPanther.png" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                    </div>
                                                    <div class="nsofts-image-card__content">
                                                        <div class="position-relative">
                                                            <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                                <span class="d-block text-truncate fs-6 fw-semibold pe-2">Black Panther</span>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </div>
                                            </a>
                                        </div>
                                    </div>
                                </form>
                            </div>
                            
                            
                            <!--EPG Page UI-->
                            <div class="tab-pane fade" id="nsofts_setting_content_2" role="tabpanel" aria-labelledby="nsofts_setting_2" tabindex="0">
                                <form action="" name="settings_epg" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">EPG Page UI</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">App UI</label>
                                        <div class="col-sm-10">
                                            <select name="is_epg" id="is_epg" class="form-select" required>
                                                <option value="">-- Select Page UI --</option>
                                                <option value="1" <?php if ($settings_data['is_epg'] == '1') { ?>selected<?php } ?>>UI ONE</option>
                                                <option value="2" <?php if ($settings_data['is_epg'] == '2') { ?>selected<?php } ?>>UI TWO</option>
                                            </select>
                                        </div>
                                    </div>
                                    <button type="submit" name="app_theme_epg" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                    <div class="row g-4 mb-3 mt-3">
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <div class="nsofts-image-card">
                                                <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                    <img src="assets/images/themes/epg_one.jpg" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                </div>
                                                <div class="nsofts-image-card__content">
                                                    <div class="position-relative">
                                                        <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                            <span class="d-block text-truncate fs-6 fw-semibold pe-2">UI ONE</span>
                                                        </div>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                        <div class="col-lg-6 col-sm-6 col-xs-12">
                                            <div class="nsofts-image-card">
                                                <div class="nsofts-image-card__cover" style="width: 100%; height: 220px; min-width: 100%; min-height: 100%; max-width: 100%;">
                                                    <img src="assets/images/themes/epg_two.jpg" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                                </div>
                                                <div class="nsofts-image-card__content">
                                                    <div class="position-relative">
                                                        <div class="d-flex align-items-center justify-content-between nsofts-image-card__content__text">
                                                            <span class="d-block text-truncate fs-6 fw-semibold pe-2">UI TWO</span>
                                                        </div>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
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
        const activeTab = localStorage.getItem('activeTabUI') || '#nsofts_setting_content_1'; // Default to General tab
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
                localStorage.setItem('activeTabUI', targetTab);
            });
        });
    });
</script>