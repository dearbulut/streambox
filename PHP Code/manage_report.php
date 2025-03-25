<?php $page_title="Manage Reports";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    $tableName="tbl_reports";   

    $sql_query="SELECT * FROM tbl_reports ORDER BY tbl_reports.`id` DESC"; 
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
            </div>
            
            <div class="card-body p-4">
                <?php if(mysqli_num_rows($result) > 0){ ?>
                    <div class="row g-4">
                        <table class="table ">
                            <thead>
                                <tr>
                                    <th>User</th>
                                    <th>Title</th>
                                    <th class="display-desktop">Report</th>
                                    <th class="text-center display-desktop">Date</th>
                                    <th style="width: 200px;" class="text-center display-desktop">Actions</th>
                                </tr>
                            </thead>
                            <tbody id="load-more-container">
                                <?php $i=0; while($row=mysqli_fetch_array($result)) { ?>
                                    <tr class="card-item">
                                        <td><?php echo $row['user_name'];?></td>
                                        <td><?php echo $row['report_title'];?></td>
                                        <td class="display-desktop"><?php echo $row['report_msg'];?></td>
                                        <td class="text-center display-desktop"><?php echo date('d-m-Y',$row['report_on']);?></td>
                                        
                                        <td class="text-center display-desktop">
                                            <a href="create_report.php?report_id=<?php echo $row['id']; ?>" class="btn btn-primary btn-icon" style="padding: 10px 10px !important;  margin-right: 10px !important;" data-bs-toggle="tooltip" data-bs-placement="top" title="View Report">
                                                <i class="ri-honour-line"></i>
                                            </a>
                                            <a href="javascript:void(0)" class="btn btn-danger btn-icon btn_delete display-desktop" data-id="<?php echo $row['id'];?>" data-table="<?=$tableName ?>" style="padding: 10px 10px !important;" data-bs-toggle="tooltip" data-bs-placement="top" title="Delete">
                                                <i class="ri-delete-bin-line"></i>
                                            </a>
                                        </td>
                                    </tr>
                                 <?php $i++; } ?>
                            </tbody>
                        </table>
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