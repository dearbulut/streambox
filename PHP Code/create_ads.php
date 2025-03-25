<?php 
    $page_title=(isset($_GET['ads_id'])) ? 'Edit Custom Ads' : 'Create Custom Ads';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    $page_save=(isset($_GET['ads_id'])) ? 'Save' : 'Create';
    
    if(isset($_POST['submit']) and isset($_GET['add'])){
        
        if($_FILES['ads_image']['name']!=""){
            
            $ext = pathinfo($_FILES['ads_image']['name'], PATHINFO_EXTENSION);
            $ads_image=rand(0,99999)."_ads.".$ext;
            $tpath1='images/'.$ads_image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["ads_image"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['ads_image']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $ads_image='';
        }
        
        $base_url = htmlentities(trim($_POST['ads_redirect_url']));
        $data = array( 
            'ads_type'  =>  cleanInput($_POST['ads_type']),
            'ads_title'  =>  cleanInput($_POST['ads_title']),
            'ads_redirect_type'  =>  cleanInput($_POST['ads_redirect_type']),
            'ads_redirect_url'  =>  $base_url,
            'ads_image'  =>  $ads_image
        );
        $qry = Insert('tbl_custom_ads',$data);
        
        $_SESSION['msg']="10";
        $_SESSION['class']='success';
        header( "Location:manage_ads.php");
        exit;
    }
    
    if(isset($_GET['ads_id'])){
        $qry="SELECT * FROM tbl_custom_ads where id='".$_GET['ads_id']."'";
        $result=mysqli_query($mysqli,$qry);
        $row=mysqli_fetch_assoc($result);
    }
    
    if(isset($_POST['submit']) and isset($_POST['ads_id'])){
        
        if($_FILES['ads_image']['name']!=""){
            
            $ext = pathinfo($_FILES['ads_image']['name'], PATHINFO_EXTENSION);
            $ads_image=rand(0,99999)."_ads.".$ext;
            $tpath1='images/'.$ads_image;
            
            if($ext!='png')  {
                $pic1=compress_image($_FILES["ads_image"]["tmp_name"], $tpath1, 80);
            } else {
                $tmp = $_FILES['ads_image']['tmp_name'];
                move_uploaded_file($tmp, $tpath1);
            }
            
        } else {
            $ads_image=$row['ads_image'];
        }
        
        $base_url = htmlentities(trim($_POST['ads_redirect_url']));
        $data = array( 
            'ads_type'  =>  cleanInput($_POST['ads_type']),
            'ads_title'  =>  cleanInput($_POST['ads_title']),
            'ads_redirect_type'  =>  cleanInput($_POST['ads_redirect_type']),
            'ads_redirect_url'  =>  $base_url,
            'ads_image'  =>  $ads_image
        );
        $category_edit=Update('tbl_custom_ads', $data, "WHERE id = '".$_POST['ads_id']."'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success';
        header( "Location:create_ads.php?ads_id=".$_POST['ads_id']);
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
                        <h5 class="mb-4"><?=$page_title ?></h5>
                        <form action="" name="addeditcategory" method="POST" enctype="multipart/form-data">
                            <input  type="hidden" name="ads_id" value="<?=(isset($_GET['ads_id'])) ? $_GET['ads_id'] : ''?>" />
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Ads Type</label>
                                <div class="col-sm-10">
                                    <select name="ads_type" id="ads_type" class="form-select" required>
                                        <?php if(isset($_GET['ads_id'])){ ?>
                                            <option value="popup" <?php if($row['ads_type']=='popup'){?>selected<?php }?>>Popup</option>	       
                                        <?php } else { ?>
                                            <option value="popup">Popup</option>
                                        <?php } ?>
                                    </select>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">URL Action Type</label>
                                <div class="col-sm-10">
                                    <select name="ads_redirect_type" id="ads_redirect_type" class="form-select" required>
                                        <?php if(isset($_GET['ads_id'])){ ?>
                                            <option value="external" <?php if($row['ads_redirect_type']=='external'){?>selected<?php }?>>External URL</option>
                                            <option value="internal" <?php if($row['ads_redirect_type']=='internal'){?>selected<?php }?>>Internal URL</option>	       
                                        <?php } else { ?>
                                            <option value="external">External URL</option>
                                            <option value="internal">Internal URL</option>
                                        <?php } ?>
                                    </select>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Title</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="ads_title" class="nsofts-input-icon__left">
                                            <i class="ri-text"></i>
                                        </label>
                                        <input type="text" name="ads_title" class="form-control" placeholder="Enter your title" value="<?php if(isset($_GET['ads_id'])){echo $row['ads_title'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">URL</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="ads_title" class="nsofts-input-icon__left">
                                            <i class="ri-links-line"></i>
                                        </label>
                                        <input type="text" name="ads_redirect_url" class="form-control" placeholder="Enter your url" value="<?php if(isset($_GET['ads_id'])){echo $row['ads_redirect_url'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Select Image</label>
                                <div class="col-sm-10">
                                    <input type="file" class="form-control" id="fileupload" name="ads_image" accept="image/*" <?php if(!isset($_GET['ads_id'])){?>required<?php } ?>>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">&nbsp;</label>
                                <div class="col-sm-10">
                                    <div class="fileupload_img" id="imagePreview">
                                        <?php if(isset($_GET['ads_id'])) {?>
                                            <img src="images/<?php echo $row['ads_image'];?>" class="col-sm-4 img-thumbnail" onerror="this.src='assets/images/600x300.jpg';"  loading="lazy" alt="">
                                        <?php }else{?>
                                            <img class="col-sm-4 img-thumbnail" type="image" src="assets/images/600x300.jpg" alt="image" />
                                        <?php } ?>
                                    </div>
                                </div>
                            </div>
                            
                            <button type="submit" name="submit" class="btn btn-primary" style="min-width: 120px;"><?=$page_save?></button>
                            
                        </form>
                    </div>
                </div>
            </div>
        </div>
    </div>
</main>
<!-- End: main -->
<?php include("includes/footer.php");?> 