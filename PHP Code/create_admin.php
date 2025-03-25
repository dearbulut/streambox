<?php 
    $page_title=(isset($_GET['user_id'])) ? 'Edit User' : 'Create User';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    if(!isset($_SESSION['admin_type'])){
        if($_SESSION['admin_type'] == 0){
            session_destroy();
            header( "Location:index.php");
            exit;
        }
    }
    
    $page_save=(isset($_GET['user_id'])) ? 'Save changes' : 'Create';
    
    if(isset($_POST['submit']) and isset($_GET['add'])){
        
        if($_FILES['image']['name']!=""){
            
            $ext = pathinfo($_FILES['image']['name'], PATHINFO_EXTENSION);
            $image=rand(0,99999)."_admin.".$ext;
            $tpath1='images/'.$category_image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["image"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['image']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $image='';
        }
        
        if($_POST['password']!=""){
            $data = array( 
                'username'  =>  $_POST['username'],
                'email'  =>  $_POST['email'],
                'admin_type'  =>  $_POST['admin_type'],
                'password'  =>  md5(trim($_POST['password'])),
                'image'  =>  $image
            );
        } else {
            $data = array( 
                'username'  =>  $_POST['username'],
                'email'  =>  $_POST['email'],
                'admin_type'  =>  $_POST['admin_type'],
                'image'  =>  $image
            );
        }
        
        $qry = Insert('tbl_admin',$data);
        
        $_SESSION['msg']="10";
        $_SESSION['class']='success';
        header( "Location:manage_admin.php");
        exit;
    }
    
    if(isset($_GET['user_id'])){
        $qry="SELECT * FROM tbl_admin where id='".$_GET['user_id']."'";
        $result=mysqli_query($mysqli,$qry);
        $row=mysqli_fetch_assoc($result);
    }
    
    if(isset($_POST['submit']) and isset($_POST['user_id'])){
        
        if($_FILES['image']['name']!=""){
            
            if($row['image']!=""){
                unlink('images/'.$row['image']);
            }
            
            $ext = pathinfo($_FILES['image']['name'], PATHINFO_EXTENSION);
            $image=rand(0,99999)."_admin.".$ext;
            $tpath1='images/'.$category_image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["image"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['image']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $image = $row['image'];
        }
        
        $data = array( 
            'username'  =>  $_POST['username'],
            'email'  =>  $_POST['email'],
            'admin_type'  =>  $_POST['admin_type'],
            'image'  =>  $image
        );
        
        $category_edit=Update('tbl_admin', $data, "WHERE id = '".$_POST['user_id']."'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success';
        header( "Location:create_admin.php?user_id=".$_POST['user_id']);
        exit;
    }
    
    if(isset($_POST['submit_password']) and isset($_POST['user_id'])){
	    
        if($_POST['register_confirm_password'] != $_POST['register_password']){
            
            $_SESSION['msg']="error_pass_not_match_admin";
            $_SESSION['class']='error'; 
            
        } else if($_POST['register_confirm_password'] == $_POST['register_password']){
            
            $data = array('password'  =>  md5(trim($_POST['register_password'])));
            
            $category_edit=Update('tbl_admin', $data, "WHERE id = '".$_POST['user_id']."'");
            
            $_SESSION['msg']="11"; 
            $_SESSION['class']='success'; 
        }
        
        header( "Location:create_admin.php?user_id=".$_POST['user_id']);
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
                        <form action="" name="addeditcategory" method="POST" enctype="multipart/form-data">
                            <input  type="hidden" name="user_id" value="<?=(isset($_GET['user_id'])) ? $_GET['user_id'] : ''?>" />
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Admin Type</label>
                                <div class="col-sm-10">
                                    <select name="admin_type" id="admin_type" class="form-select" required>
                                        <?php if(isset($_GET['user_id'])){ ?>
                                            <?php if($row['admin_type'] != '3') { ?>
                                                <option value="1" <?php if($row['admin_type']=='1'){?>selected<?php }?>>ADMIN</option>
                                                <option value="0" <?php if($row['admin_type']=='0'){?>selected<?php }?>>EDITOR</option>
                                            <?php } else { ?>
                                                <option value="3" <?php if($row['admin_type']=='3'){?>selected<?php }?>>SUPER ADMIN</option>
                                            <?php } ?>
                                        <?php } else { ?>
                                            <option value="1">ADMIN</option>
                                            <option value="0">EDITOR</option>
                                        <?php } ?>
                                    </select>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Username</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="username" class="nsofts-input-icon__left">
                                            <i class="ri-user-line"></i>
                                        </label>
                                        <input type="text" name="username" placeholder="Enter your user name" class="form-control" value="<?php if(isset($_GET['user_id'])){echo $row['username'];}?>" required>
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
                                        <input type="text" name="email" placeholder="Enter your email address" class="form-control" value="<?php if(isset($_GET['user_id'])){echo $row['email'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <?php if(!isset($_GET['user_id'])) {?>
                                <div class="mb-3 row">
                                    <label class="col-sm-2 col-form-label">Password</label>
                                    <div class="col-sm-10">
                                        <div class="nsofts-input-icon nsofts-input-icon--left">
                                            <label for="password" class="nsofts-input-icon__left">
                                                <i class="ri-lock-line"></i>
                                            </label>
                                            <input type="text" name="password" id="password" placeholder="Enter your password" class="form-control"  autocomplete="off" <?php if(!isset($_GET['user_id'])){?>required<?php } ?>>
                                        </div>
                                    </div>
                                </div>
                            <?php } ?>
                            
                            <div class="mb-3 row">
                                <label for="" class="col-sm-2 col-form-label">Select Image</label>
                                <div class="col-sm-10">
                                    <div class="row">
                                        <div class="col-md-4">
                                            <input type="file" name="image"  class="form-control" id="fileupload" accept="image/*">
                                            <p class="control-label-help hint_lbl">(Recommended resolution: 512x512)</p>
                                        </div>
                                        <div class="col-md-3">
                                            <div class="fileupload_img" id="imagePreview">
                                                <?php if(isset($_GET['user_id'])) {?>
                                                    <?php if($row['image']!='' AND file_exists('images/'.$row['image'])) {?>
                                                        <img  type="image" src="images/<?php echo PROFILE_IMG; ?>" style="width: 50px;height: 50px"   alt="image" />
                                                    <?php } else { ?>
                                                        <img type="image" src="assets/images/user_photo.png" style="width: 50px;height: 50px"   alt="image" />
                                                    <?php } ?>
                                                
                                                <?php } else {?>
                                                    <img type="image" src="assets/images/user_photo.png" style="width: 50px;height: 50px"   alt="image" />
                                                <?php } ?>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            
                            <button type="submit" name="submit" class="btn btn-primary" style="min-width: 120px;"><?=$page_save?></button>
                            
                        </form>
                    </div>
                </div>
            </div>
        </div>
        
        <?php if(isset($_GET['user_id'])) {?>
        
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
        
        <?php } ?>
        
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?> 