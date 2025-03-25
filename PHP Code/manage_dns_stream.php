<?php $page_title="Manage DNS 1-Stream";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    $tableName="tbl_stream_dns";   
   
    if(!isset($_GET['keyword'])){
        $sql_query="SELECT * FROM tbl_stream_dns ORDER BY tbl_stream_dns.`id` DESC"; 
    } else{
        $keyword=addslashes(trim($_GET['keyword']));
        $sql_query="SELECT * FROM tbl_stream_dns WHERE (`dns_title` LIKE '%$keyword%' OR `dns_base` LIKE '%$keyword%') ORDER BY tbl_stream_dns.`id` DESC"; 
    }
    $result=mysqli_query($mysqli,$sql_query) or die(mysqli_error($mysqli));
?>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        <div class="card h-100">
            <div class="card-top d-md-inline-flex align-items-center justify-content-between py-3 px-4">
                <div class="d-inline-flex align-items-center text-decoration-none fw-semibold">
                    <span class="ps-2 lh-1"><?=$page_title ?></span>
                </div>
                <div class="d-flex mt-2 mt-md-0">
                    <form method="get" id="searchForm" action="" class="me-2">
                        <div class="input-group">
                            <input type="text" id="search_input" class="form-control" placeholder="Search title or dns" name="keyword" value="<?php if(isset($_GET['keyword'])){ echo $_GET['keyword'];} ?>" required="required">
                            <button class="btn btn-outline-default d-inline-flex align-items-center" type="search">
                                <i class="ri-search-2-line"></i>
                            </button>
                        </div>
                    </form>
                    <a href="create_dns_stream.php?add=yes" class="btn btn-primary d-inline-flex align-items-center justify-content-center">
                        <i class="ri-add-line"></i>
                        <span class="ps-1 text-nowrap d-none d-sm-block">Create DNS</span>
                    </a>
                </div>
            </div>
            
            <div class="card-body p-4">
                <?php if(mysqli_num_rows($result) > 0){ ?>
                    <div class="row g-4 mb-3" id="load-more-container">
                        <?php $i=0; while($row=mysqli_fetch_array($result)) { ?>
                            <div class="col-lg-3 col-sm-6 card-item">
                                <div class="nsofts-card-light p-3">
                                    <h5 class="mb-2 text-truncate"><?php echo $row['dns_title'];?></h5>
                                    <p><?php echo $row['dns_base'];?></p>
                                    <div class="d-flex justify-content-between">
                                        <div class="d-flex">
                                           <a href="create_dns_stream.php?dns_id=<?php echo $row['id'];?>" class="btn btn-outline-primary rounded-pill me-2 btn-icon" data-bs-toggle="tooltip" data-bs-placement="top" title="Edit">
                                                <i class="ri-pencil-line"></i>
                                            </a>
                                            <a href="javascript:void(0)" class="btn btn-outline-danger rounded-pill me-2 btn-icon btn_delete" data-id="<?php echo $row['id'];?>" data-table="<?=$tableName ?>" data-bs-toggle="tooltip" data-bs-placement="top" title="Delete">
                                                <i class="ri-delete-bin-line"></i>
                                            </a>
                                        </div>
                                        <div class="nsofts-switch d-flex align-items-center enable_disable" data-bs-toggle="tooltip" data-bs-placement="top" title="Enable / Disable">
                                            <input type="checkbox" id="enable_disable_check_<?= $i ?>" data-id="<?= $row['id'] ?>" data-table="<?=$tableName ?>" data-column="status" class="cbx hidden btn_enable_disable" <?php if ($row['status'] == 1) { echo 'checked'; } ?>>
                                            <label for="enable_disable_check_<?= $i ?>" class="nsofts-switch__label"></label>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        <?php $i++; } ?>
                    </div>
                    <button class="nsofts-load-btn mt-4 mb-2 d-flex align-items-center justify-content-center"
                        id="load-more-btn">
                        <span>Load More</span>
                        <i class="ri-sort-desc"></i>
                    </button>
                <?php } else { ?>
                    <ul class="p-5">
                        <h1 class="text-center">No data found</h1>
                    </ul>
                <?php } ?>
                </nav>
            </div>
        </div>
        
    </div>
</main>
<!-- End: main -->
<?php include("includes/footer.php");?> 