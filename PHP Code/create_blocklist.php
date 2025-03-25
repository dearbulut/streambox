<?php 
    $page_title=(isset($_GET['dns_id'])) ? 'Edit DNS Blocklist' : 'Create DNS Blocklist';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    $page_save=(isset($_GET['dns_id'])) ? 'Save' : 'Create';
    
    if(isset($_POST['submit']) and isset($_GET['add'])){

        $base_url = htmlentities(trim($_POST['dns_base']));
        $data = array('dns_base'  =>  $base_url);
        $qry = Insert('tbl_xui_dns_block',$data);
        
        $_SESSION['msg']="10";
        $_SESSION['class']='success';
        header( "Location:manage_blocklist.php");
        exit;
    }
    
    if(isset($_GET['dns_id'])){
        $qry="SELECT * FROM tbl_xui_dns_block where id='".$_GET['dns_id']."'";
        $result=mysqli_query($mysqli,$qry);
        $row=mysqli_fetch_assoc($result);
    }
    
    if(isset($_POST['submit']) and isset($_POST['dns_id'])){

        $base_url = htmlentities(trim($_POST['dns_base']));
        $data = array('dns_base'  =>  $base_url);
        $category_edit=Update('tbl_xui_dns_block', $data, "WHERE id = '".$_POST['dns_id']."'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success';
        header( "Location:create_blacklist.php?dns_id=".$_POST['dns_id']);
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
                            <input  type="hidden" name="dns_id" value="<?=(isset($_GET['dns_id'])) ? $_GET['dns_id'] : ''?>" />
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Domain Names</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="dns_base" class="nsofts-input-icon__left">
                                            <i class="ri-spam-3-line"></i>
                                        </label>
                                        <input type="text" name="dns_base" class="form-control" placeholder="Enter domain names" value="<?php if(isset($_GET['dns_id'])){echo $row['dns_base'];}?>" required>
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