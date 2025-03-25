<?php $page_title="Data Deletion Policy";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    $tableName="tbl_policy_deletion";   
    
    if(!isset($_GET['keyword'])){
        $sql_query="SELECT * FROM tbl_policy_deletion ORDER BY tbl_policy_deletion.`id` DESC"; 
    } else {
        $keyword=addslashes(trim($_GET['keyword']));
        $sql_query="SELECT * FROM tbl_policy_deletion WHERE (`user_email` LIKE '%$keyword%') ORDER BY tbl_policy_deletion.`id` DESC"; 
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
                            <input type="text" id="search_input" class="form-control" placeholder="Search email" name="keyword" value="<?php if(isset($_GET['keyword'])){ echo $_GET['keyword'];} ?>" required="required">
                            <button class="btn btn-outline-default d-inline-flex align-items-center" type="search">
                                <i class="ri-search-2-line"></i>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
            <div class="card-body p-4">
                <?php if(mysqli_num_rows($result) > 0){ ?>
                    <div class="row g-4">
                        <table class="table ">
                            <thead>
                                <tr>
                                    <th class="text-truncate">User Email</th>
                                    <th class="text-center display-desktop">Date</th>
                                    <th class="text-center">Type</th>
                                    <th style="width: 100px;" class="text-center">Actions</th>
                                </tr>
                            </thead>
                            <tbody id="load-more-container">
                                <?php $i=0; while($row=mysqli_fetch_array($result)) { ?>
                                    <tr class="card-item">
                                        <td class="text-truncate"><?php echo $row['user_email'];?></td>
                                        <td class="text-center display-desktop"><?php echo calculate_time_span($row['deletion_on']);?></td>
                                        <td class="text-center">
                                            <span class="nsofts-badge nsofts-badge-primary"><?php echo $row['policy_type'];?></span>
                                        </td>
                                        <td class="text-center">
                                            <a href="javascript:void(0)" class="btn btn-danger btn-icon btn_delete" data-id="<?php echo $row['id'];?>" data-table="<?=$tableName ?>" style="padding: 10px 10px !important;" data-bs-toggle="tooltip" data-bs-placement="top" title="Delete">
                                                <i class="ri-delete-bin-line"></i>
                                            </a>
                                        </td>
                                    </tr>
                                 <?php $i++; } ?>
                            </tbody>
                        </table>
                    </div>
                    <button class="nsofts-load-btn mt-4 mb-2 d-flex align-items-center justify-content-center" id="load-more-btn">
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