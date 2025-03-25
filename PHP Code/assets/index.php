<?php 
require("../includes/curl_helper.php");
$installFile="../includes/.lic";
$errors = false;
?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">

    <!-- Website Title -->
    <title>Script - Deactivator</title>
    
    <!-- Favicon -->
    <link href="../assets/images/open-box.svg" rel="icon">

    <!-- IOS Touch Icons -->
    <link rel="apple-touch-icon" href="../assets/images/open-box.svg">
    <link rel="apple-touch-icon" sizes="152x152" href="../assets/images/open-box.svg">
    <link rel="apple-touch-icon" sizes="180x180" href="../assets/images/open-box.svg">
    <link rel="apple-touch-icon" sizes="167x167" href="../assets/images/open-box.svg">

    <!-- Google fonts -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css2?family=Ubuntu:wght@300;400;500;700&display=swap" rel="stylesheet">

   <!-- Vendor styles -->
   <link rel="stylesheet" href="../assets/vendors/bootstrap/bootstrap.min.css" type="text/css">
   <link rel="stylesheet" href="../assets/vendors/perfect-scrollbar/perfect-scrollbar.min.css" type="text/css">
   <link rel="stylesheet" href="../assets/vendors/remixicon/remixicon.min.css" type="text/css">
   <link rel="stylesheet" href="../assets/vendors/quill/quill.min.css" type="text/css">
   <link rel="stylesheet" href="../assets/vendors/select2/select2.min.css" type="text/css">
   <link rel="stylesheet" href="../assets/vendors/sweetalerts2/sweetalert2.min.css" type="text/css">

   <!-- Main style -->
   <link rel="stylesheet" href="../assets/css/styles.css?v=1.0.0" type="text/css">

</head>
<body>
    
     <!--Loader -->
    <div id="nsofts_loader">
        <div class="text-center">
            <i class="ri-3x ri-donut-chart-line nsofts-loader-icon"></i>
            <span class="d-block">Loading</span>
        </div>
    </div>

    <!-- Start: 404 -->
    <main class="d-flex justify-content-center align-items-center py-5 min-vh-100">
        <div class="container">
            <div class="col-xl-4 col-lg-5 col-md-7 col-sm-9 mx-auto">
                <div class="nsofts-auth position-relative">
                    <img src="../assets/images/pattern-1.svg" class="nsofts-auth__pattern-1 position-absolute" alt="">
                    <img src="../assets/images/pattern-2.svg" class="nsofts-auth__pattern-2 position-absolute" alt="">
                    <div class="card position-relative">
                        <div class="card-body px-4 py-5">
                            <?php
                            if(!empty($_POST)){
                                $deactivate_password = strip_tags(trim($_POST["pass"]));
                                $deactivate_response = deactivateLicense($deactivate_password);
                                if(empty($deactivate_response)){
                                    $msg='Server is unavailable.';
                                } else {
                                    $msg=$deactivate_response['message'];
                                }
                                if($deactivate_response['status'] != true){ ?>
                                    <form action="index.php" method="POST">
                                        <div class="alert alert-danger" role="alert">
                                            <?php echo ucfirst($msg); ?>
                                        </div>
                                        <input type="hidden" name="something">
                                    </form>
                                <?php } else{ ?>
                                    <div class="alert alert-primary" role="alert">
                                        <?php echo ucfirst($msg); ?>
                                    </div>
                                <?php 
                                }
                            } else { ?>
                            
                                <?php if(is_writeable($installFile)){ ?>
                                    <div class="mb-4">
                                        <h5>Script Deactivator</h5>
                                        <?php if(is_writeable($installFile)){ ?>
                                            <p>Click on deactivate license to deactivate and remove the currently installed license from this installation, So that you can activate the same license on some other domain.</p>
                                        <?php } ?>
                                    </div>
                                    <?php
                                    echo "<div class='notify notify--success mb-2' style'margin-bottom: 10px !important;'>
                                        <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                        <path fill='none' d='M0 0h24v24H0z'/>
                                        <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                        </svg> 
                                        <span class='notify__text'>Ready to Deactivate process</span>
                                        </div>";
                                    ?>
                                    </br>
                                    <form action="index.php" method="POST">
                                        <div class="mb-3">
                                            <div class="nsofts-input-icon nsofts-input-icon--both">
                                                <label for="pass" class="nsofts-input-icon__left">
                                                    <i class="ri-door-lock-line"></i>
                                                </label>
                                                <input class="form-control mb-8" type="text" id="pass" placeholder="Enter your deactivate password" name="pass" autocomplete="off" required>
                                                <button type="button" id="nsofts_password_toggler" class="nsofts-input-icon__right btn p-0 border-0 lh-1">
                                                    <i class="ri-eye-line nsofts-eye-open"></i>
                                                    <i class="ri-eye-off-line nsofts-eye-close d-none"></i>
                                                </button>
                                            </div>
                                        </div>
                                        <div class="mb-3">
                                            <button type="submit" class="btn btn-danger btn-lg w-100" style="min-width: 124px;">Deactivate License</button>
                                        </div>
                                    </form>
                                <?php } else { ?>
                                    <?php
                                    echo "<div class='notify notify--error mb-2' style'margin-bottom: 10px !important;'>
                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                    <path fill='none' d='M0 0h24v24H0z'/>
                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                    </svg>
                                    <span class='notify__text'>The Deactivator process is already complete !</span>
                                    </div>";
                                    ?>
                                <?php } ?>
                                
                              <?php 
                            } ?>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </main>
    <!-- End: 404 -->

    <!-- Vendor scripts -->
    <script src="../assets/js/jquery.min.js"></script>
    <script src="../assets/vendors/bootstrap/bootstrap.min.js"></script>
    <script src="../assets/vendors/notify/notify.min.js"></script>
    <script src="../assets/vendors/perfect-scrollbar/perfect-scrollbar.min.js"></script>
    <script src="../assets/vendors/quill/quill.min.js"></script>
    <script src="../assets/vendors/select2/select2.min.js"></script>
    <script src="../assets/vendors/sweetalerts2/sweetalert2.min.css"></script>
    <script src="../assets/vendors/chartjs/chart.min.js"></script>

    <!-- Main script -->
    <script src="../assets/js/main.js?v=1.0.0"></script>
</body>
</html>