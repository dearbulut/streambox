<?php 
    $page_title = 'View Report';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    if(isset($_GET['report_id'])){
        $qry="SELECT * FROM tbl_reports where id='".$_GET['report_id']."'";
        $result=mysqli_query($mysqli,$qry);
        $row=mysqli_fetch_assoc($result);
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
        
        <form action="" name="addeditaudio" method="POST" enctype="multipart/form-data">
            <div class="row g-4">
                <div class="card h-100">
                    <div class="card-body p-4">
                        <h5 class="mb-5"><?=$page_title ?></h5>
                        <div class="mb-3">
                            <h5 class="mb-4">Title - <?php if(isset($_GET['report_id'])){echo $row['report_title'];}?></h5>
                            <?=stripslashes($row['report_msg'])?>
                        </div>
                        
                    </div>
                </div>
            </div>
        </form>
    </div>
</main>
<!-- End: main -->
<?php include("includes/footer.php");?> 