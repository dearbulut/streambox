<?php $page_title="Settings Admin panel";
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
    
    $qry_smtp = "SELECT * FROM tbl_smtp_settings where id='1'";
    $result_smtp = mysqli_query($mysqli, $qry_smtp);
    $row = mysqli_fetch_assoc($result_smtp);
    
    if(isset($_POST['submit_general'])){
        
        $img_res=mysqli_query($mysqli,"SELECT * FROM tbl_settings WHERE id='1'");
        $img_row=mysqli_fetch_assoc($img_res);
        
        if($_FILES['app_logo']['name']!=""){
            
           
            if($img_row['app_logo']!=""){
                unlink('images/'.$img_row['app_logo']);
            }
            
            $ext = pathinfo($_FILES['app_logo']['name'], PATHINFO_EXTENSION);
            $app_logo=rand(0,99999)."_logo.".$ext;
            $tpath1='images/'.$app_logo;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["app_logo"]["tmp_name"], $tpath1, 80);
            } else{
                $tmp = $_FILES['app_logo']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $app_logo = $img_row['app_logo'];
        }

        $data = array(
            'app_name'  =>  cleanInput($_POST['app_name']),
            'app_logo'  =>  $app_logo,
            'site_description'  =>  cleanInput($_POST['site_description']),
            'site_keywords'  =>  cleanInput($_POST['site_keywords']),
        );
        
        $settings_edit=Update('tbl_settings', $data, "WHERE id = '1'");
        $_SESSION['msg']="11";
        header( "Location:settings.php");
        exit;
    }
    
    if (isset($_POST['submit'])) {
        $key = ($_POST['smtpIndex'] == 'gmail') ? '0' : '1';
        $password = '';
        if ($_POST['smtp_password'][$key] != '') {
            $password = $_POST['smtp_password'][$key];
        } else {
            if ($key == 0) {
                $password = $row['smtp_gpassword'];
            } else {
                $password = $row['smtp_password'];
            }
        }
        if ($key == 0) {
            $data = array(
                'smtp_type'  =>  'gmail',
                'smtp_ghost'  =>  $_POST['smtp_host'][$key],
                'smtp_gemail'  =>  $_POST['smtp_email'][$key],
                'smtp_gpassword'  =>  $password,
                'smtp_gsecure'  =>  $_POST['smtp_secure'][$key],
                'gport_no'  =>  $_POST['port_no'][$key]
            );
        } else {
            $data = array(
                'smtp_type'  =>  'server',
                'smtp_host'  =>  $_POST['smtp_host'][$key],
                'smtp_email'  =>  $_POST['smtp_email'][$key],
                'smtp_password'  =>  $password,
                'smtp_secure'  =>  $_POST['smtp_secure'][$key],
                'port_no'  =>  $_POST['port_no'][$key]
            );
        }
        $sql = "SELECT * FROM tbl_smtp_settings WHERE id='1'";
        $res = mysqli_query($mysqli, $sql) or die(mysqli_error($mysqli));
        if (mysqli_num_rows($res) > 0) {
            $update = Update('tbl_smtp_settings', $data, "WHERE id = '1'");
        } else {
            $insert = Insert('tbl_smtp_settings', $data);
        }
        $_SESSION['class'] = "success";
        $_SESSION['msg'] = "11";
        header("Location:settings.php");
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
                            <button class="nav-link" id="nsofts_setting_1" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_1" type="button" role="tab" aria-controls="nsofts_setting_1" aria-selected="false">
                                <i class="ri-list-settings-line"></i>
                                <span>General</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_3" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_3" type="button" role="tab" aria-controls="nsofts_setting_3" aria-selected="false">
                                <i class="ri-paint-brush-line"></i>
                                <span>Custom Style</span>
                            </button>
                            
                            <button class="nav-link" id="nsofts_setting_2" data-bs-toggle="pill" data-bs-target="#nsofts_setting_content_2" type="button" role="tab" aria-controls="nsofts_setting_2" aria-selected="false">
                                <i class="ri-mail-settings-line"></i>
                                <span>SMTP Settings</span>
                            </button>
                        </div>
                    </div>
                    <div class="nsofts-setting__content">
                        <div class="tab-content">
                            <div class="tab-pane fade show active" id="nsofts_setting_content_1" role="tabpanel" aria-labelledby="nsofts_setting_1" tabindex="0">
                                <form action="" name="settings_general" method="POST" enctype="multipart/form-data">
                                    <h4 class="mb-4">General Settings</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site name</label>
                                        <div class="col-sm-10">
                                            <input type="text" class="form-control" name="app_name" id="app_name" value="<?php echo $settings_data['app_name']?>" required="required">
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Description</label>
                                        <div class="col-sm-10">
                                            <textarea rows="4" name="site_description" class="form-control" required=""><?php echo stripslashes($settings_data['site_description']); ?></textarea>
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site Keywords</label>
                                        <div class="col-sm-10">
                                            <input type="text" name="site_keywords" id="site_keywords" value="<?php echo $settings_data['site_keywords']; ?>"  class="form-control" required="required">
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Site logo</label>
                                        <div class="col-sm-10">
                                            <div class="row">
                                                <div class="col-md-5">
                                                    <input type="file" class="form-control-file" name="app_logo" value="fileupload" accept=".png, .jpg, .JPG .PNG" onchange="fileValidation()" id="fileupload">
                                                    <p class="control-label-help hint_lbl">(Recommended resolution: 512x512)</p>
                                                </div>
                                                <div class="col-md-3">
                                                    <?php if($settings_data['app_logo']!='' AND file_exists('images/'.$settings_data['app_logo'])) { ?>
                                                        <div class="fileupload_img" id="imagePreview">
                                                            <img  type="image" src="images/<?=$settings_data['app_logo']?>" style="width: 50px;height: 50px"   alt="image" />
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
                                    <button type="submit" name="submit_general" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <div class="tab-pane fade" id="nsofts_setting_content_3" role="tabpanel" aria-labelledby="nsofts_setting_3" tabindex="0">
                                <form action="" method="post" id="custom_style">
                                    <h4 class="mb-4">Custom CSS or JS Script</h4>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Header Code</label>
                                        <div class="col-sm-10">
                                            <textarea rows="8" name="header_code" class="form-control"  placeholder="Custom CSS or JS Script" ><?php echo html_entity_decode($settings_data['header_code']); ?></textarea>
                                        </div>
                                    </div>
                                    <div class="mb-3 row">
                                        <label for="" class="col-sm-2 col-form-label">Footer Code</label>
                                        <div class="col-sm-10">
                                            <textarea rows="8" name="footer_code" class="form-control" placeholder="Custom CSS or JS Script"><?php echo html_entity_decode($settings_data['footer_code']); ?></textarea>
                                        </div>
                                    </div>
                                    <button type="submit" name="submit_style" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                </form>
                            </div>
                            
                            <div class="tab-pane fade" id="nsofts_setting_content_2" role="tabpanel" aria-labelledby="nsofts_setting_2" tabindex="0">
                                <h4 class="mb-4">SMTP Settings</h4>
                                
                                <div class="row">
                                    <form action="" method="post" class="form form-horizontal" enctype="multipart/form-data">
                                        <div class="mb-3 row">
                                            <label class="col-sm-2 col-form-label">SMTP Type</label>
                                            <div class="col-md-3">
                                                <div class="form-check">
                                                    <input type="radio" name="smtp_type" id="gmail" class="form-check-input" value="gmail" <?php if ($row['smtp_type'] == 'gmail') { echo ' checked="" id="disabledFieldsetCheck"';} ?>>
                                                    <label class="form-check-label" for="gmail"> Gmail SMTP</label>
                                                </div>
                                            </div>
                                            <div class="col-md-3">
                                                <div class="form-check">
                                                    <input type="radio" name="smtp_type" id="server" class="form-check-input" value="server" <?php if ($row['smtp_type'] == 'server') { echo ' checked="" disabled="disabled"';} ?>>
                                                    <label class="form-check-label" for="server">Server SMTP</label>
                                                </div>
                                                </div>
                                        </div>
                                        <input type="hidden" name="smtpIndex" value="<?= $row['smtp_type'] ?>">
                                        
                                        <div class="gmailContent" <?php if ($row['smtp_type'] == 'gmail') { echo 'style="display:block"'; } else { echo 'style="display:none"'; } ?>>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">SMTP Host</label>
                                                <div class="col-sm-10">
                                                    <input type="text" name="smtp_host[]" class="form-control col-md-7" value="<?= $row['smtp_ghost'] ?>" placeholder="mail.example.in" <?php if ($row['smtp_type'] == 'gmail') {echo 'required';} ?>> 
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">Email</label>
                                                <div class="col-sm-10">
                                                    <input type="text" name="smtp_email[]" class="form-control col-md-7" value="<?= $row['smtp_gemail'] ?>" placeholder="info@example.com" <?php if ($row['smtp_type'] == 'gmail') { echo 'required';} ?>>
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">Password</label>
                                                <div class="col-sm-10">
                                                    <input type="password" name="smtp_password[]" class="form-control col-md-7" value="" placeholder="********">
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label class="col-sm-2 col-form-label">SMTPSecure</label>
                                                <div class="col-md-3">
                                                    <select name="smtp_secure[]" class="select2 form-control" <?php if ($row['smtp_type'] == 'gmail') { echo 'required';} ?>>
                                                        <option value="tls" <?php if ($row['smtp_gsecure'] == 'tls') {
                                                            echo 'selected';
                                                        } ?>>TLS</option>
                                                        <option value="ssl" <?php if ($row['smtp_gsecure'] == 'ssl') {
                                                            echo 'selected';
                                                        } ?>>SSL</option>
                                                    </select>
                                                </div>
                                                <div class="col-md-3">
                                                    <input type="text" name="port_no[]" class="form-control" value="<?= $row['gport_no'] ?>" placeholder="Enter Port No." <?php if ($row['smtp_type'] == 'gmail') { echo 'required';} ?>>
                                                </div>
                                            </div>
                                        </div>
                                        
                                        <div class="serverContent" <?php if ($row['smtp_type'] == 'server') { echo 'style="display:block"'; } else { echo 'style="display:none"'; } ?>>
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">SMTP Host</label>
                                                <div class="col-sm-10">
                                                    <input type="text" name="smtp_host[]" id="smtp_host" class="form-control col-md-7" value="<?= $row['smtp_host'] ?>" placeholder="mail.example.in" <?php if ($row['smtp_type'] == 'server') { echo 'required';} ?>>
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">Email</label>
                                                <div class="col-sm-10">
                                                    <input type="text" name="smtp_email[]" id="smtp_email" class="form-control col-md-7" value="<?= $row['smtp_email'] ?>" placeholder="info@example.com" <?php if ($row['smtp_type'] == 'server') { echo 'required';} ?>>
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label for="" class="col-sm-2 col-form-label">Password</label>
                                                <div class="col-sm-10">
                                                    <input type="password" name="smtp_password[]" id="smtp_password" class="form-control col-md-7" value="" placeholder="********">
                                                </div>
                                            </div>
                                            
                                            <div class="mb-3 row">
                                                <label class="col-sm-2 col-form-label">SMTPSecure</label>
                                                <div class="col-md-3">
                                                    <select name="smtp_secure[]" class="select2 form-control" <?php if ($row['smtp_type'] == 'server') { echo 'required';} ?>>
                                                        <option value="tls" <?php if ($row['smtp_secure'] == 'tls') { echo 'selected'; } ?>>TLS</option>
                                                        <option value="ssl" <?php if ($row['smtp_secure'] == 'ssl') { echo 'selected'; } ?>>SSL</option>
                                                    </select>
                                                </div>
                                                <div class="col-md-3">
                                                    <input type="text" name="port_no[]" id="port_no" class="form-control" value="<?= $row['port_no'] ?>" placeholder="Enter Port No." <?php if ($row['smtp_type'] == 'server') { echo 'required';} ?>>
                                                </div>
                                            </div>
                                        </div>
                                        
                                        <input type="hidden" id="server_data" data-stuff='<?php echo htmlentities(json_encode($row)); ?>'>
                                        <button type="submit" name="submit" class="btn btn-primary" style="min-width: 120px;">Save</button>
                                    </form>
                                    
                                    <form action="" method="post" id="check_smtp_form" class="mt-5">
                                        <div class="alert alert-info" role="alert">
                                            <h4 class="mb-4">Check Mail Configuration</h4>
                                            <p>Send test mail to your email to check Email functionality work or not.</p>
                                            <hr>
                                            <div class="mb-3 row">
                                                 <div class="form-group">
                                                    <label class="control-label">Email <span style="color: red">*</span>:-</label>
                                                    <input type="text" name="email" class="form-control" autocomplete="off" placeholder="info@example.com" required="">
                                                </div>
                                            </div>
                                            <button type="submit" name="btn_send" class="btn btn-primary" style="min-width: 120px;">Send</button>
                                        </div>
                                    </form>
                                </div>
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
        const activeTab = localStorage.getItem('activeTab1') || '#nsofts_setting_content_1'; // Default to General tab
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
                localStorage.setItem('activeTab1', targetTab);
            });
        });
    });
</script>

<script type="text/javascript">
$("input[name='smtp_type']").on("click", function(e) {

    var checkbox = $(this);
    
    $("input[name='smtp_password[]']").attr("required", false);
    
    e.preventDefault();
    e.stopPropagation();
    
    var _val = $(this).val();
    if (_val == 'gmail') {
      swal({
        title: "Are you sure?",
        type: "warning",
        confirmButtonClass: 'btn btn-primary m-2',
        cancelButtonClass: 'btn btn-danger m-2',
        buttonsStyling: false,
        showCancelButton: true,
        confirmButtonText: "Yes",
        cancelButtonText: "No",
        closeOnConfirm: false,
        closeOnCancel: false,
        showLoaderOnConfirm: false
      }).then(function(result) {
        if (result.value) {
            
          checkbox.attr("disabled", true);
          checkbox.prop("checked", true);
          $("#server").attr("disabled", false);
          $("#server").prop("checked", false);
          $(".serverContent").hide();
          $(".gmailContent").show();
          $(".serverContent").find("input").attr("required", false);
          $(".gmailContent").find("input").attr("required", true);
          $("input[name='smtpIndex']").val('gmail');
          swal.close();
        } else {
          swal.close();
        }
      });
    } else {
        
      swal({
        title: "Are you sure?",
        type: "warning",
        confirmButtonClass: 'btn btn-primary m-2',
        cancelButtonClass: 'btn btn-danger m-2',
        buttonsStyling: false,
        showCancelButton: true,
        confirmButtonText: "Yes",
        cancelButtonText: "No",
        closeOnConfirm: false,
        closeOnCancel: false,
        showLoaderOnConfirm: false
        
      }).then(function(result) {
        if (result.value) {
          checkbox.attr("disabled", true);
          checkbox.prop("checked", true);
          $("#gmail").attr("disabled", false);
          $("#gmail").prop("checked", false);
          $(".gmailContent").hide();
          $(".serverContent").show();
          $("input[name='smtpIndex']").val('server');
          $(".serverContent").find("input").attr("required", true);
          $(".gmailContent").find("input").attr("required", false);
          swal.close();
        } else {
          swal.close();
        }
      });
    }
});

$("#check_smtp_form").on("submit", function(e) {
    e.preventDefault();
    
    var email = $(this).find("input[name='email']").val();
    
    swal({
        title: "Are you sure?",
        text: 'Email will receive to ' + email,
		type: "warning",
		confirmButtonClass: 'btn btn-primary m-2',
        cancelButtonClass: 'btn btn-danger m-2',
        buttonsStyling: false,
		showCancelButton: true,
		confirmButtonText: "Yes",
		cancelButtonText: "No",
		closeOnConfirm: false,
		closeOnCancel: false,
		showLoaderOnConfirm: true
    }).then(function(result) {
        if (result.value) {
            $.ajax({
                type: 'post',
                url: 'processData.php',
                dataType: 'json',
                data: {'action': 'check_smtp','email': email},
                success: function(res) {
                    console.log(res);
                    location.reload();
                }
            });
        } else {
            swal.close();
        }
    });
});
</script>