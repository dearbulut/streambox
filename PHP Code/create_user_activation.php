<?php 
    $page_title=(isset($_GET['user_id'])) ? 'Edit User' : 'Create User';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
	require_once("thumbnail_images.class.php");
	
	$page_save=(isset($_GET['user_id'])) ? 'Save' : 'Create';
	
	$activationCode = generateStrong(10,"d");
	
	if(isset($_POST['submit']) and isset($_GET['add'])){
	    
	    $base_url = htmlentities(trim($_POST['dns_base']));
        $data = array(
            'user_type'=> cleanInput($_POST['user_type']),							 
            'user_name'  => cleanInput($_POST['user_name']),
            'user_password'  =>  cleanInput($_POST['user_password']),
            'dns_base'  =>  $base_url,
            'activation_code'  =>  cleanInput($_POST['activation_code']),
            'registered_on'  =>  strtotime(date('d-m-Y h:i:s A')),
            'status'  =>  '1'
        );
        
        $qry = Insert('tbl_activation_code',$data);
        
        $_SESSION['msg']="10";
        $_SESSION['class']='success'; 
        header("location:manage_users_activation.php");	 
        exit;
    }
	
	if(isset($_GET['user_id'])){
        $user_qry="SELECT * FROM tbl_activation_code where id='".$_GET['user_id']."'";
        $user_result=mysqli_query($mysqli,$user_qry);
        $user_row=mysqli_fetch_assoc($user_result);
    }
    
    if(isset($_POST['submit']) and isset($_POST['user_id'])){

        $base_url = htmlentities(trim($_POST['dns_base']));
        $data = array(
            'user_type'=> cleanInput($_POST['user_type']),										 
            'user_name'  => cleanInput($_POST['user_name']),
            'user_password'  =>  cleanInput($_POST['user_password']),
            'dns_base'  =>  $base_url,
            'activation_code'  =>  cleanInput($_POST['activation_code']),
        );
        
        $user_edit=Update('tbl_activation_code', $data, "WHERE id = '".$_POST['user_id']."'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success'; 
        header("Location:create_user_activation.php?user_id=".$_POST['user_id']);
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
                        <form action="" name="addedituser" method="POST" enctype="multipart/form-data">
                            <input type="hidden" name="user_id" value="<?=(isset($_GET['user_id'])) ? $_GET['user_id'] : ''?>" />
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Panel Type</label>
                                <div class="col-sm-10">
                                        <select name="user_type" id="user_type" class="form-select" required>
                                            <?php if(isset($_GET['user_id'])){ ?>
                                                <option value="xui" <?php if($user_row['user_type']=='xui'){?>selected<?php }?>>Xtream Codes or XUI</option> 
                                                <option value="stream" <?php if($user_row['user_type']=='stream'){?>selected<?php }?>>1-Stream</option> 
                                            <?php } else { ?>
                                                <option value="xui">Xtream Codes or XUI</option> 
                                                <option value="stream">1-Stream</option>
                                            <?php } ?> 
                                    </select>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Username</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="user_name" class="nsofts-input-icon__left">
                                            <i class="ri-user-line"></i>
                                        </label>
                                        <input type="text" name="user_name" placeholder="Enter your user name" class="form-control" value="<?php if(isset($_GET['user_id'])){echo $user_row['user_name'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Password</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="user_password" class="nsofts-input-icon__left">
                                            <i class="ri-lock-line"></i>
                                        </label>
                                        <input type="text" name="user_password" id="user_password" placeholder="Enter your password" class="form-control" value="<?php if(isset($_GET['user_id'])){echo $user_row['user_password'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">DNS</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="user_password" class="nsofts-input-icon__left">
                                            <i class="ri-links-line"></i>
                                        </label>
                                        <input type="text" name="dns_base" id="dns_base" placeholder="Enter your dns url" class="form-control" value="<?php if(isset($_GET['user_id'])){echo $user_row['dns_base'];}?>" required>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <input type="hidden" name="activation_code" id="activation_code" value="<?php if(isset($_GET['user_id'])){echo $user_row['activation_code'];} else { echo $activationCode; }?>" required>
                                <label class="col-sm-2 col-form-label">Activation Code</label>
                                <div class="col-sm-10">
                                    <div class="pb-clipboard mb-2">
                                        <span class="pb-clipboard__url"><a class="fw-bold text-decoration-none" style="color: var(--ns-primary);"></a><span id="clipboard_delete"><?php if(isset($_GET['user_id'])){echo $user_row['activation_code'];} else { echo $activationCode; }?></span></span>
                                        <a class="pb-clipboard__link btn_delete_ac" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy Activation Code">
                                            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                                <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                            </svg>
                                        </a>
                                    </div>
                                </div>
                            </div>
                            
                            
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">&nbsp;</label>
                                <div class="col-sm-10">
                                    <button type="submit" name="submit" class="btn btn-primary" style="min-width: 120px;"><?=$page_save?></button>
                                </div>
                            </div>
                            
                        </form>
                    </div>
                </div>
            </div>
        </div>
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?> 
<script type="text/javascript">
    $(document).on("click", ".btn_delete_ac", function(e) {
        var el = document.getElementById('clipboard_delete');
        var successful = copyToClipboard(el);
        if (successful) {
            $.notify('Copied!', { position:"top right",className: 'success'} );
        } else {
            $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
        }
    });
</script>