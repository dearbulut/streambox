<?php 
    $page_title=(isset($_GET['sidebar_id'])) ? 'Edit Sidebar' : 'Add Sidebar';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    $page_save=(isset($_GET['sidebar_id'])) ? 'Save' : 'Create';
    
    if(isset($_POST['submit']) and isset($_GET['add'])){
        
        $sidebar_type = trim($_POST['sidebar_type']);

        $sidebar_editor ='';
        $sidebar_html ='';
        $img_type ='';
        
        $sidebar_image_link = htmlentities(trim($_POST['sidebar_image_link']));
         
        if($sidebar_type=='visual_editor'){
            $sidebar_editor = addslashes(trim($_POST['sidebar_editor']));
        }else if($sidebar_type=='html_code'){
             $sidebar_html = addslashes(trim($_POST['sidebar_html']));
        }else{
            if($_FILES['sidebar_image']['name']!=""){
            
                $ext = pathinfo($_FILES['sidebar_image']['name'], PATHINFO_EXTENSION);
                $sidebar_image=rand(0,99999)."_sideba.".$ext;
                $tpath1='images/'.$sidebar_image;
                
                if($ext!='png')  {
                    $pic1=compress_image($_FILES["sidebar_image"]["tmp_name"], $tpath1, 80);
                }else{
                    $tmp = $_FILES['sidebar_image']['tmp_name'];
                    move_uploaded_file($tmp, $tpath1);
                }
                
            }else{
                $sidebar_image='';
            }
        }
        
        $data = array( 
            'sidebar_title'  =>  cleanInput($_POST['sidebar_title']),
            'sidebar_type'  =>  $sidebar_type,
            'sidebar_icon'  =>  $_POST['sidebar_icon'],
            'sidebar_editor'  =>  $sidebar_editor,
            'sidebar_html'  =>  $sidebar_html,
            'sidebar_image'  =>  $sidebar_image,
            'sidebar_image_link'  =>  $sidebar_image_link
        );  
        
        $qry = Insert('tbl_sidebar',$data);
        
        $_SESSION['msg']="10";
        $_SESSION['class']='success';
        header( "Location:create_sidebar.php?add=yes");
        exit;
    }
    
    if(isset($_GET['sidebar_id'])){
        $qry="SELECT * FROM tbl_sidebar where id='".$_GET['sidebar_id']."'";
        $result=mysqli_query($mysqli,$qry);
        $row=mysqli_fetch_assoc($result);
    }
    
    if(isset($_POST['submit']) and isset($_POST['sidebar_id'])){
        
        $sidebar_type = trim($_POST['sidebar_type']);

        $sidebar_editor ='';
        $sidebar_html ='';
        $img_type ='';
        $sidebar_image_link = htmlentities(trim($_POST['sidebar_image_link']));
        
        if($sidebar_type=='visual_editor'){
            $sidebar_editor = addslashes(trim($_POST['sidebar_editor']));
        }else if($sidebar_type=='html_code'){
             $sidebar_html = addslashes(trim($_POST['sidebar_html']));
        }else{
            if($_FILES['sidebar_image']['name']!=""){
            
                if($row['sidebar_image']!=""){
                    unlink('images/'.$row['sidebar_image']);
                }
            
                $ext = pathinfo($_FILES['sidebar_image']['name'], PATHINFO_EXTENSION);
                $sidebar_image=rand(0,99999)."_sideba.".$ext;
                $tpath1='images/'.$sidebar_image;
                
                if($ext!='png')  {
                    $pic1=compress_image($_FILES["sidebar_image"]["tmp_name"], $tpath1, 80);
                }else{
                    $tmp = $_FILES['sidebar_image']['tmp_name'];
                    move_uploaded_file($tmp, $tpath1);
                }
                
            }else{
                $sidebar_image='';
            }
        }
        
        $data = array( 
            'sidebar_title'  =>  cleanInput($_POST['sidebar_title']),
            'sidebar_type'  =>  $sidebar_type,
            'sidebar_icon'  =>  $_POST['sidebar_icon'],
            'sidebar_editor'  =>  $sidebar_editor,
            'sidebar_html'  =>  $sidebar_html,
            'sidebar_image'  =>  $sidebar_image,
            'sidebar_image_link'  =>  $sidebar_image_link
        );
        
        $category_edit=Update('tbl_sidebar', $data, "WHERE id = '".$_POST['sidebar_id']."'");
        
        $_SESSION['msg']="11";
        $_SESSION['class']='success';
        header( "Location:create_sidebar.php?sidebar_id=".$_POST['sidebar_id']);
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
                             <input  type="hidden" name="sidebar_id" value="<?=(isset($_GET['sidebar_id'])) ? $_GET['sidebar_id'] : ''?>" />
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Title</label>
                                <div class="col-sm-10">
                                    <input type="text" name="sidebar_title" class="form-control" value="<?php if(isset($_GET['sidebar_id'])){echo $row['sidebar_title'];}?>" required>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Icon</label>
                                <div class="col-sm-10">
                                    <select name="sidebar_icon" id="sidebar_icon" class="nsofts-select" required>
                                        <option value="">--Select icon--</option>
                                    <?php if(isset($_GET['sidebar_id'])){ ?>
                                        <option value="ri-line-chart-fill" <?php if($row['sidebar_icon']=='ri-line-chart-fill'){?>selected<?php }?>>Chart</option>
                                        <option value="ri-headphone-fill" <?php if($row['sidebar_icon']=='ri-headphone-fill'){?>selected<?php }?>>Headphone</option>
                                        <option value="ri-user-6-fill" <?php if($row['sidebar_icon']=='ri-user-6-fill'){?>selected<?php }?>>User</option>
                                        <option value="ri-global-fill" <?php if($row['sidebar_icon']=='ri-global-fill'){?>selected<?php }?>>Global</option>
                                        <option value="ri-question-answer-fill" <?php if($row['sidebar_icon']=='ri-question-answer-fill'){?>selected<?php }?>>Question Answer</option>
                                        <option value="ri-star-fill" <?php if($row['sidebar_icon']=='ri-star-fill'){?>selected<?php }?>>Star</option>
                                        <option value="ri-share-fill" <?php if($row['sidebar_icon']=='ri-share-fill'){?>selected<?php }?>>Share</option>
                                        <option value="ri-home-4-fill" <?php if($row['sidebar_icon']=='ri-home-4-fill'){?>selected<?php }?>>Home</option>
                                    	<option value="ri-home-smile-line" <?php if($row['sidebar_icon']=='ri-home-smile-line'){?>selected<?php }?>>Home smile</option>
                                    	<option value="ri-mail-send-fill" <?php if($row['sidebar_icon']=='ri-mail-send-fill'){?>selected<?php }?>>Mailsend</option>
                                    	<option value="ri-links-line" <?php if($row['sidebar_icon']=='ri-links-line'){?>selected<?php }?>>Links</option>
                                    	<option value="ri-advertisement-fill" <?php if($row['sidebar_icon']=='ri-advertisement-fill'){?>selected<?php }?>>Advertisement</option>
                                    	<option value="ri-question-answer-line" <?php if($row['sidebar_icon']=='ri-question-answer-line'){?>selected<?php }?>>Question</option>
                                    	<option value="ri-quill-pen-fill" <?php if($row['sidebar_icon']=='ri-quill-pen-fill'){?>selected<?php }?>>Quill Pen</option>
                                    	<option value="ri-bug-line" <?php if($row['sidebar_icon']=='ri-bug-line'){?>selected<?php }?>>Bug</option>
                                    	<option value="ri-draft-line" <?php if($row['sidebar_icon']=='ri-draft-line'){?>selected<?php }?>>Draft</option>
                                    	<option value="ri-folder-music-line" <?php if($row['sidebar_icon']=='ri-folder-music-line'){?>selected<?php }?>>Folder music</option>
                                    	<option value="ri-map-pin-line" <?php if($row['sidebar_icon']=='ri-map-pin-line'){?>selected<?php }?>>Map</option>
                                    	<option value="ri-image-fill" <?php if($row['sidebar_icon']=='ri-image-fill'){?>selected<?php }?>>Image</option>
                                    	<option value="ri-music-fill" <?php if($row['sidebar_icon']=='ri-music-fill'){?>selected<?php }?>>Music</option>
                                    <?php }else{ ?>
                                    	<option value="ri-line-chart-fill">Chart</option>
                                    	<option value="ri-headphone-fill">Headphone</option>
                                    	<option value="ri-user-6-fill">User</option>
                                    	<option value="ri-global-fill">Global</option>
                                    	<option value="ri-question-answer-fill">Question Answer</option>
                                    	<option value="ri-star-fill">Star</option>
                                    	<option value="ri-share-fill">Share</option>
                                    	<option value="ri-home-4-fill">Home</option>
                                    	<option value="ri-home-smile-line">Home smile</option>
                                    	<option value="ri-mail-send-fill">Mailsend</option>
                                    	<option value="ri-links-line">Links</option>
                                    	<option value="ri-advertisement-fill">Advertisement</option>
                                    	<option value="ri-question-answer-line">Question</option>
                                    	<option value="ri-quill-pen-fill">Quill Pen</option>
                                    	<option value="ri-bug-line">Bug</option>
                                    	<option value="ri-draft-line">Draft</option>
                                    	<option value="ri-folder-music-line">Folder music</option>
                                    	<option value="ri-map-pin-line">Map</option>
                                    	<option value="ri-image-fill">Image</option>
                                    	<option value="ri-music-fill">Music</option>
                                    <?php } ?> 
                                    </select>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Type</label>
                                <div class="col-sm-10">
                                    <select name="sidebar_type" id="sidebar_type" class="nsofts-select" required>
                                        <?php if(isset($_GET['sidebar_id'])){ ?>
                                            <option value="visual_editor" <?php if($row['sidebar_type']=='visual_editor'){?>selected<?php }?>>VisualEditor</option>
                                            <option value="html_code" <?php if($row['sidebar_type']=='html_code'){?>selected<?php }?>>HTML Code</option>
                                            <option value="img_type" <?php if($row['sidebar_type']=='img_type'){?>selected<?php }?>>Image</option>
                                        <?php }else{ ?>
                                        	<option value="visual_editor">VisualEditor</option>
                                        	<option value="html_code">HTML Code</option>
                                        	<option value="img_type">Image</option>
                                        <?php } ?> 
                                    </select>
                                </div>
                            </div>
                            
                            <div id="visual_editor_display" class="mb-3 row" <?php if(isset($_GET['sidebar_id'])){ if($row['sidebar_type']!='visual_editor'){?>style="display:none;"<?php }}?>>
                                <label class="col-sm-2 col-form-label">Visual Editor</label>
                                <div class="col-sm-10">
                                     <textarea name="sidebar_editor" id="sidebar_editor" rows="5" class="nsofts-editor"><?php if(isset($_GET['sidebar_id'])){echo $row['sidebar_editor'];}?></textarea>
                                </div>
                            </div>
                            
                            <div id="html_code_display" class="mb-3 row" <?php if(isset($_GET['sidebar_id'])){ if($row['sidebar_type']!='html_code'){?>style="display:none;"<?php }} else {?>style="display:none;"<?php }?>>
                                <label class="col-sm-2 col-form-label">HTML Code</label>
                                <div class="col-sm-10">
                                    <textarea class="form-control"  name="sidebar_html" cols="30" rows="15"><?php if(isset($_GET['sidebar_id'])){echo $row['sidebar_html'];}?></textarea>
                                </div>
                            </div>
                            
                            <div id="img_url_display" class="mb-3 row" <?php if(isset($_GET['sidebar_id'])){ if($row['sidebar_type']!='img_type'){?>style="display:none;"<?php }}else{?>style="display:none;"<?php }?>>
                                <label class="col-sm-2 col-form-label">Click URL</label>
                                <div class="col-sm-10">
                                    <input type="text" name="sidebar_image_link" id="sidebar_image_link" value="<?php if(isset($_GET['sidebar_id'])){echo $row['sidebar_image_link'];}?>" class="form-control">
                                </div>
                            </div>
                            
                            <div id="img_type_display" class="mb-3 row" <?php if(isset($_GET['sidebar_id'])){ if($row['sidebar_type']!='img_type'){?>style="display:none;"<?php }}else{?>style="display:none;"<?php }?>>
                                <label class="col-sm-2 col-form-label">Select Image</label>
                                <div class="col-sm-10">
                                    <input type="file" class="form-control-file" name="sidebar_image"   accept=".png, .jpg, .JPG .PNG" onchange="fileValidation()" id="fileupload">
                                </div>
                            </div>
                            
                            <div id="img_view_display" class="mb-3 row" <?php if(isset($_GET['sidebar_id'])){ if($row['sidebar_type']!='img_type'){?>style="display:none;"<?php }}else{?>style="display:none;"<?php }?>>
                                <label class="col-sm-2 col-form-label">&nbsp;</label>
                                <div class="col-sm-10">
                                    <div class="fileupload_img" id="imagePreview">
                                        <?php if(isset($_GET['sidebar_id']) AND file_exists('images/'.$row['sidebar_image'])) {?>
                                          <img class="col-sm-3 img-thumbnail" type="image" src="images/<?php echo $row['sidebar_image'];?>" alt="image" />
                                        <?php }else{?>
                                          <img class="col-sm-3 img-thumbnail" type="image" src="assets/images/300x300.jpg" alt="image" />
                                        <?php } ?>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="row">
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
$(document).ready(function(e) {
    $("#sidebar_type").change(function(){
        var type=$("#sidebar_type").val();
        if(type=="visual_editor"){
            $("#visual_editor_display").show();
            $("#html_code_display").hide(); 
            $("#img_type_display").hide(); 
            $("#img_view_display").hide(); 
            $("#img_url_display").hide();
        }
        else if(type=="html_code"){
            $("#visual_editor_display").hide();
            $("#html_code_display").show(); 
            $("#img_type_display").hide(); 
            $("#img_view_display").hide();
            $("#img_url_display").hide();
        }else {
           $("#visual_editor_display").hide();
            $("#html_code_display").hide(); 
            $("#img_type_display").show(); 
            $("#img_view_display").show();
             $("#img_url_display").show();
        }
    });
});
</script>