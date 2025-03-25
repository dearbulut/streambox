<?php 
    $page_title="Edit Profile";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    $page_save='Save';
    
    if(isset($_SESSION['id'])){
		$qry="select * from tbl_admin where id='".$_SESSION['id']."'";
		$result=mysqli_query($mysqli,$qry);
		$row=mysqli_fetch_assoc($result);
	}
	
	if(isset($_POST['submit_data'])){
	    
	    if(isset($_FILES['image']) && $_FILES['image']['name'] != ""){
	        
            $img_res=mysqli_query($mysqli,'SELECT * FROM tbl_admin WHERE id='.$_SESSION['id'].'');
            $img_res_row=mysqli_fetch_assoc($img_res);
            
            if($img_res_row['image']!=""){
                unlink('images/'.$img_res_row['image']);
            }
            
            $ext = pathinfo($_FILES['image']['name'], PATHINFO_EXTENSION);
            $image=rand(0,99999)."_profile.".$ext;
            $tpath1='images/'.$image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["image"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['image']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
            $data = array( 
                'username'  =>  $_POST['username'],
                'email'  =>  $_POST['email'],
                'image'  =>  $image
            );
            
        } else {
            $data = array( 
                'username'  =>  $_POST['username'],
                'email'  =>  $_POST['email']
            );
        }
        
        $channel_edit=Update('tbl_admin', $data, "WHERE id = '".$_SESSION['id']."'");
        
		$_SESSION['msg']="11"; 
		header( "Location:auth_my_profile.php");
		exit;
	}
	
	if(isset($_POST['submit_password'])){
	    
        if($_POST['register_confirm_password'] != $_POST['register_password']){
            
            $_SESSION['msg']="error_pass_not_match_admin";
            $_SESSION['class']='error'; 
            
        } else if($_POST['register_confirm_password'] == $_POST['register_password']){
            
            $data = array('password'  =>  md5(trim($_POST['register_password'])));
            
            $channel_edit=Update('tbl_admin', $data, "WHERE id = '".$_SESSION['id']."'");
            
            $_SESSION['msg']="11"; 
            $_SESSION['class']='success'; 
        }
        
        header( "Location:logout.php");
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
            
        <div class="row g-4">
            <div class="col-12">
                <div class="card h-100">
                    <div class="card-body p-4">
                        <h5 class="mb-3"><?=$page_title ?></h5>
                        <form action="" name="editprofile" method="POST" enctype="multipart/form-data">
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">User name</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="username" class="nsofts-input-icon__left">
                                            <i class="ri-user-line"></i>
                                        </label>
                                        <input type="text" name="username" placeholder="Enter your user name" value="<?php echo $row['username'];?>" class="form-control" autocomplete="off" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Email</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="email" class="nsofts-input-icon__left">
                                            <i class="ri-at-line"></i>
                                        </label>
                                        <input type="text" name="email" value="<?= $row['email']?>" placeholder="Enter your email address" class="form-control" autocomplete="off" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label for="" class="col-sm-2 col-form-label">Select Image</label>
                                <div class="col-sm-10">
                                    <div class="row">
                                        <div class="col-md-4">
                                            <input type="file" class="form-control" id="fileupload" accept="image/*">
                                            <p class="control-label-help hint_lbl">(Recommended resolution: 512x512)</p>
                                        </div>
                                        <div class="col-md-3">
                                            <div class="fileupload_img" id="imagePreview">
                                                 <?php if($row['image']!='' AND file_exists('images/'.$row['image'])) {?>
                                                    <img  type="image" src="images/<?php echo PROFILE_IMG; ?>" style="width: 50px;height: 50px"   alt="image" />
                                                <?php } else { ?>
                                                    <img type="image" src="assets/images/user_photo.png" style="width: 50px;height: 50px"   alt="image" />
                                                <?php } ?>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <button type="submit" name="submit_data" class="btn btn-primary" style="min-width: 120px;">Save changes</button>
                        </form>
                    </div>
                </div>
            </div>
        </div>
        
        <hr>
        
        <div class="row g-4">
            <div class="col-12">
                <div class="card h-100">
                    <div class="card-body p-4">
                        <h5 class="mb-3">Change Password</h5>
                        <form action="" name="passwordprofile" method="POST" enctype="multipart/form-data">
                            <div class="alert alert-warning alert-dismissible" role="alert">
                              <h6 class="alert-heading d-flex align-items-center fw-bold mb-1">Ensuer that these requirements are met</h6>
                              <p class="mb-0">Minimum 8 characters long, uppercase & symbol</p>
                            </div>
                            
                            <div class="row g-3">
                              <div class="col-md-6">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="register_password" class="nsofts-input-icon__left">
                                            <i class="ri-lock-line"></i>
                                        </label>
                                        <input type="text" name="register_password" id="register_password" class="form-control" placeholder="Enter your new password" autocomplete="off" required>
                                    </div>
                              </div>
                              <div class="col-md-6">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="register_confirm_password" class="nsofts-input-icon__left">
                                            <i class="ri-lock-line"></i>
                                        </label>
                                        <input type="text" name="register_confirm_password" id="register_confirm_password" class="form-control" placeholder="Enter your new confirm password" autocomplete="off" required>
                                    </div>
                              </div>
                            </div>
                            <button type="submit" name="submit_password" class="btn btn-primary mt-3" style="min-width: 120px;">Change Password</button>
                        </form>
                    </div>
                </div>
            </div>
        </div>
        
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?> 