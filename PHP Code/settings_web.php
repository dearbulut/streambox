<?php $page_title="Settings Web";
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
    
    $qry="SELECT * FROM tbl_web_settings where id='1'";
    $result=mysqli_query($mysqli,$qry);
    $settings_data=mysqli_fetch_assoc($result);
    
    $sql_query="SELECT * FROM tbl_sidebar ORDER BY tbl_sidebar.`id` DESC";
    $result_data=mysqli_query($mysqli,$sql_query) or die(mysqli_error($mysqli));

    if(isset($_POST['submit_general'])){
        
        if($_FILES['web_favicon']['name']!=""){
            
            $img_res=mysqli_query($mysqli,"SELECT * FROM tbl_web_settings WHERE id='1'");
            $img_row=mysqli_fetch_assoc($img_res);
            if($img_row['web_favicon']!=""){
                unlink('images/'.$img_row['web_favicon']);
            }
            
            $ext = pathinfo($_FILES['web_favicon']['name'], PATHINFO_EXTENSION);
            $favicon_image=rand(0,99999)."_web_favicon.".$ext;
            $tpath1='images/'.$favicon_image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["web_favicon"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['web_favicon']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $favicon_image=$settings_data['web_favicon'];
        }
        
        if($_FILES['web_logo_1']['name']!=""){
            
            $img_res=mysqli_query($mysqli,"SELECT * FROM tbl_web_settings WHERE id='1'");
            $img_row=mysqli_fetch_assoc($img_res);
            if($img_row['web_logo_1']!=""){
                unlink('images/'.$img_row['web_logo_1']);
            }
            
            $ext = pathinfo($_FILES['web_logo_1']['name'], PATHINFO_EXTENSION);
            $logo_image=rand(0,99999)."_web_favicon.".$ext;
            $tpath1='images/'.$logo_image;
            
            $tmp = $_FILES['web_logo_1']['tmp_name'];
            move_uploaded_file($tmp, $tpath1);
            
        } else {
            $logo_image=$settings_data['web_logo_1'];
        }

        $data = array(
            'site_name'  =>  cleanInput($_POST['site_name']),
            'site_description'  =>  cleanInput($_POST['site_description']),
            'site_keywords'  =>  cleanInput($_POST['site_keywords']),
            'web_favicon'  =>  $favicon_image,
            'web_logo_1'  =>  $logo_image,
            'copyright_text'  =>  cleanInput($_POST['copyright_text']),
            'header_code'  =>  htmlentities(trim($_POST['header_code'])),
            'footer_code'  => htmlentities(trim($_POST['footer_code']))
        );
        
        $settings_edit = Update('tbl_web_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg'] = "11";
        header("Location:settings_web.php");
        exit;
        
    } else if(isset($_POST['submit_terms'])){
        
        $data = array(
            'terms_of_use_page_title'  =>  cleanInput($_POST['terms_of_use_page_title']),
            'terms_of_use_content'  =>  addslashes($_POST['terms_of_use_content']),
        );
        
        $settings_edit = Update('tbl_web_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg'] = "11";
        header("Location:settings_web.php");
        exit;
    }else if(isset($_POST['submit_privacy'])){
        
        $data = array(
            'privacy_page_title'  =>  cleanInput($_POST['privacy_page_title']),
            'privacy_content'  =>  addslashes($_POST['privacy_content'])
        );
        
        $settings_edit = Update('tbl_web_settings', $data, "WHERE id = '1'");
        
        $_SESSION['msg'] = "11";
        header("Location:settings_web.php");
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
                                <span>Sidebar</span>
                            </button>
                            <button class="nav-link" id="nsofts_setting_3" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_3" type="button" role="tab" aria-controls="nsofts_setting_3" aria-selected="false">
                                <i class="ri-survey-line"></i>
                                <span>Privacy Policy</span>
                            </button>
                            <button class="nav-link" id="nsofts_setting_4" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_4" type="button" role="tab" aria-controls="nsofts_setting_4" aria-selected="false">
                                <i class="ri-survey-line"></i>
                                <span>Terms & Conditions</span>
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
                                       <a style="color: #f44336c7;" href="https://sbox.envatonemosofts.com/" target="_blank">StreamBox - Best IPTV Video Streaming Website</a>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Name</label>
                                        <div class="col-sm-10">
                                           <input type="text" name="site_name" id="site_name" value="<?= $settings_data['site_name']?>"  class="form-control">
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Description</label>
                                        <div class="col-sm-10">
                                            <textarea rows="3" name="site_description" class="form-control" required=""><?php echo stripslashes($settings_data['site_description']); ?></textarea>
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Keywords</label>
                                        <div class="col-sm-10">
                                            <input type="text" name="site_keywords" id="site_keywords" value="<?php echo $settings_data['site_keywords']; ?>"  class="form-control" required="required">
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Favicon</label>
                                        <div class="col-sm-10">
                                            <div class="row">
                                                <div class="col-md-5">
                                                    <input type="file" class="form-control-file" name="web_favicon" value="fileupload" accept=".png, .jpg, .JPG .PNG" onchange="fileValidation()" id="fileupload">
                                                    <p class="control-label-help hint_lbl">(Recommended resolution: 16x16 or 32x32)</p>
                                                </div>
                                                <div class="col-md-3">
                                                    <?php if($settings_data['web_favicon']!='' AND file_exists('images/'.$settings_data['web_favicon'])) { ?>
                                                        <div class="fileupload_img" id="imagePreview">
                                                            <img  type="image" src="images/<?=$settings_data['web_favicon']?>" style="width: 50px;height: 50px"   alt="image" />
                                                        </div>
                                                    <?php }else{ ?>
                                                        <div class="fileupload_img" id="imagePreview">
                                                            <img type="image" src="assets/images/300x300.jpg" style="width: 50px; height: 50px"  alt="image" />
                                                        </div>
                                                    <?php } ?>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Logo</label>
                                        <div class="col-sm-10">
                                            <div class="row">
                                                <div class="col-md-5">
                                                    <input type="file" class="form-control-file" name="web_logo_1" value="fileupload2" accept=".svg, .SVG" onchange="fileValidation2()" id="fileupload2">
                                                    <p class="control-label-help hint_lbl">(Recommended resolution: 1000x500)</p>
                                                </div>
                                                <div class="col-md-3">
                                                    <?php if($settings_data['web_logo_1']!='' AND file_exists('images/'.$settings_data['web_logo_1'])) { ?>
                                                        <div class="fileupload_img" id="imagePreview2">
                                                            <img  type="image" src="images/<?=$settings_data['web_logo_1']?>" style="width: 50px;height: 50px"   alt="image" />
                                                        </div>
                                                    <?php }else{ ?>
                                                        <div class="fileupload_img" id="imagePreview2">
                                                            <img type="image" src="assets/images/300x300.jpg" style="width: 50px; height: 50px"  alt="image" />
                                                        </div>
                                                    <?php } ?>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Copyright Text</label>
                                        <div class="col-sm-10">
                                            <input type="text" name="copyright_text" id="copyright_text" value="<?php echo $settings_data['copyright_text']; ?>"  class="form-control" required="required">
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Header Code</label>
                                        <div class="col-sm-10">
                                            <textarea rows="6" name="header_code" class="form-control"  placeholder="Custom CSS or JS Script" ><?php echo html_entity_decode($settings_data['header_code']); ?></textarea>
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Footer Code</label>
                                        <div class="col-sm-10">
                                            <textarea rows="6" name="footer_code" class="form-control" placeholder="Custom CSS or JS Script"><?php echo html_entity_decode($settings_data['footer_code']); ?></textarea>
                                        </div>
                                    </div>
                                    
                                    <button type="submit" name="submit_general" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--Web Sidebar-->
                            <div class="tab-pane fade" id="nsofts_setting_content_2" role="tabpanel" aria-labelledby="nsofts_setting_2" tabindex="0">
                                <form action="" name="settings_general" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Sidebar</h4>
                                    <a href="create_sidebar.php?add=yes" class="btn btn-primary d-inline-flex align-items-center justify-content-center">
                                        <i class="ri-add-line"></i>
                                        <span class="ps-1 text-nowrap d-none d-sm-block">Create Sidebar</span>
                                    </a>
                                    </br>
                                    </br>
                                    <?php if(mysqli_num_rows($result_data) > 0){ ?>
                                    <div class="row">
                                        <?php $i=0; while($row=mysqli_fetch_array($result_data)) { ?>
                                            <div class="col-lg-3 col-sm-6">
                                                <div class="nsofts-card-light p-3">
                                                    <h5 class="mb-2"><?php echo $row['sidebar_title'];?></h5></p>
                                                    <div class="d-flex">
                                                        <a href="create_sidebar.php?sidebar_id=<?php echo $row['id'];?>" class="btn btn-outline-primary rounded-pill me-2 btn-icon" data-bs-toggle="tooltip" data-bs-placement="top" title="Edit">
                                                            <i class="ri-pencil-line"></i>
                                                        </a>
                                                        <a href="javascript:void(0)" class="btn btn-outline-danger rounded-pill me-2 btn-icon btn_delete" data-id="<?php echo $row['id'];?>" data-table="tbl_sidebar" data-bs-toggle="tooltip" data-bs-placement="top" title="Delete">
                                                            <i class="ri-delete-bin-line"></i>
                                                        </a>
                                                    </div>
                                                </div>
                                            </div>
                                        <?php $i++; } ?>
                                    </div>
                                    <?php }else{ ?>
                                        <ul class="p-5">
                                            <h1 class="text-center">No data found</h1>
                                        </ul>
                                    <?php } ?>
                                </form>
                            </div>
                            
                            <!--Privacy Policy-->
                            <div class="tab-pane fade" id="nsofts_setting_content_3" role="tabpanel" aria-labelledby="nsofts_setting_3" tabindex="0">
                                <form action="" name="submit_privacy" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Privacy Policy</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Page title</label>
                                        <div class="col-sm-10">
                                           <input type="text" name="privacy_page_title" id="privacy_page_title" value="<?= $settings_data['privacy_page_title']?>"  class="form-control">
                                        </div>
                                    </div>
                                    <div>
                                        <textarea name="privacy_content" id="privacy_content" rows="5" class="nsofts-editor mb-4">
                                           <?php echo stripslashes($settings_data['privacy_content']); ?>
                                        </textarea>
                                    </div>
                                    <button type="submit" name="submit_privacy" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <!--Terms & Conditions-->
                            <div class="tab-pane fade" id="nsofts_setting_content_4" role="tabpanel" aria-labelledby="nsofts_setting_4" tabindex="0">
                               <form action="" name="settings_privacy"  method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">Terms & Conditions</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Page title</label>
                                        <div class="col-sm-10">
                                           <input type="text" name="terms_of_use_page_title" id="terms_of_use_page_title" value="<?= $settings_data['terms_of_use_page_title']?>"  class="form-control">
                                        </div>
                                    </div>
                                    <div>
                                        <textarea name="terms_of_use_content" id="terms_of_use_content" rows="5" class="nsofts-editor mb-4">
                                           <?php echo stripslashes($settings_data['terms_of_use_content']); ?>
                                        </textarea>
                                    </div>
                                    <button type="submit" name="submit_terms" class="btn btn-primary" style="min-width: 120px;">Save</button>
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
        const activeTab = localStorage.getItem('activeTabWeb') || '#nsofts_setting_content_1'; // Default to General tab
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
                localStorage.setItem('activeTabWeb', targetTab);
            });
        });
    });
</script>
<script type="text/javascript">
function fileValidation2(){
        var fileInput = document.getElementById('fileupload2');
        var filePath = fileInput.value;
        var allowedExtensions = /(\.svg|.SVG)$/i;
        if(!allowedExtensions.exec(filePath)){
            if(filePath!='')
            fileInput.value = '';
            $.notify('Please upload file having extension .svg, .SVG only!', { position:"top right",className: 'error'} ); 
            return false;
         }else {
            if (fileInput.files && fileInput.files[0]) {
                var reader = new FileReader();
                reader.onload = function(e) {
                    $("#imagePreview2").find("img").attr("src", e.target.result);
                };
                reader.readAsDataURL(fileInput.files[0]);
            }
        }
    }
</script>