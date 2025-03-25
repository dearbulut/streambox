<?php $page_title="Settings API";
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
    
    if(isset($_POST['submit_general'])){
        
        $data = array('account_delete_intruction'  =>  $_POST['account_delete_intruction']);
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg']="11";
        $_SESSION['class'] = "success";
        header( "Location:settings_api.php");
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
                                <span>Themoviedb</span>
                            </button>
                        </div>
                    </div>
                    <div class="nsofts-setting__content">
                        <div class="tab-content">
                            <!--Themoviedb Settings-->
                            <div class="tab-pane fade show active" id="nsofts_setting_content_1" role="tabpanel" aria-labelledby="nsofts_setting_1" tabindex="0">
                                <form action="" name="settings_general" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Themoviedb Settings</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">TMDb Access Token <a style="color: #f44336c7;" href="https://docs.nemosofts.com/streambox-app/admin-panel/themoviedb" target="_blank">link</a></label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="account_delete_intruction" id="account_delete_intruction" value="<?php echo $settings_data['account_delete_intruction']?>" >
                                        </div>
                                    </div>
                                    <button type="submit" name="submit_general" class="btn btn-primary" style="min-width: 120px;">Save</button>
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