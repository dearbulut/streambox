<?php
    $page_title="Data Deletion Policy";
    include("includes/db_helper.php");
    require("includes/lb_helper.php");
    
    $client_lang = array();
    $client_lang['1'] = "Email format is invalid !";
    $client_lang['2']="Your registration is completed. Wait until admin approve it";
    $client_lang['3']="Submitted successfully.";
    
    if(isset($_POST['submit_clear'])){
        
        $email=addslashes(trim($_POST['user_email']));
        
        $data = array(
            'policy_type'  =>  'Clear Data',
            'user_email'  =>  $_POST['user_email'],
            'report_msg'  =>  '',
            'deletion_on'  =>  strtotime(date('d-m-Y h:i:s A')),
        );
        $qry = Insert('tbl_policy_deletion',$data);
        $_SESSION['class']="success";
        $_SESSION['msg']="3";
        header( "Location:account_delete_request.php");
        exit;
        
    } else if(isset($_POST['submit_delete'])){
        
        $email=addslashes(trim($_POST['user_email']));
        
        $data = array(
            'policy_type'  =>  'Delete Account',
            'user_email'  =>  $_POST['user_email'],
            'report_msg'  =>  '',
            'deletion_on'  =>  strtotime(date('d-m-Y h:i:s A')),
        );
        $qry = Insert('tbl_policy_deletion',$data);
        $_SESSION['class']="success";
        $_SESSION['msg']="3";
        header( "Location:account_delete_request.php");
        exit;
    }
?>

 
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">

    <!-- Website Title -->
    <title>Data Deletion Policy | <?php echo APP_NAME;?></title>
    
    <!-- Favicon --> 
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="32x32">
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="192x192">

    <!-- IOS Touch Icons -->
    <link rel="apple-touch-icon" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="152x152" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="180x180" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="167x167" href="images/<?php echo APP_LOGO;?>">

    <!-- Google fonts -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css2?family=Ubuntu:wght@300;400;500;700&display=swap" rel="stylesheet">

    <!-- Vendor styles -->
    <link rel="stylesheet" href="assets/vendors/bootstrap/bootstrap.min.css" type="text/css">
    <link rel="stylesheet" href="assets/vendors/remixicon/remixicon.min.css" type="text/css">
    
    <!-- Main style -->
    <link rel="stylesheet" href="assets/css/styles.css?v=1.0.1" type="text/css">
    
    <!--[if lt IE 9]>
	    <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
        <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->

</head>

<style>
    p {
        color: #5e5e5e;
    }
    h1, h2, h3, h4, h5, h6 {
    	color: #343434;
    }
</style>

<body>
    
    <!-- Start: 404 -->
    <main class="d-flex justify-content-center align-items-center py-5 min-vh-100">
        <div class="container">
            <div class="col-xl-4 col-lg-5 col-md-7 col-sm-9 mx-auto">
                <div class="nsofts-auth position-relative">
                    <img src="assets/images/pattern-1.svg" class="nsofts-auth__pattern-1 position-absolute" alt="">
                    <img src="assets/images/pattern-2.svg" class="nsofts-auth__pattern-2 position-absolute" alt="">
                    <div class="card position-relative">
                        <form action="" name="datadeletion" method="POST" enctype="multipart/form-data">
                            <div class="card-body px-4 py-4">
                            <div class="container mb-3">
                                <div class="row justify-content-start">
                                    <div class="col-3 p-0 mb-2">
                                     <img src="images/<?php echo APP_LOGO;?>"  class="img-thumbnail" alt="image">
                                    </div>
                                    <div class="col-9">
                                        <h5>Welcome to <?php echo APP_NAME;?>!</h5>
                                        <p class="m-0 p-0"> By submitting this you can completely delete you account and collected data.</p>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3">
                                <label for="email" class="form-label fw-semibold">Email or Username</label>
                                <div class="nsofts-input-icon nsofts-input-icon--left">
                                    <label for="email" class="nsofts-input-icon__left">
                                        <i class="ri-user-line"></i>
                                    </label>
                                    <input type="text" name="user_email" id="user_email"  class="form-control" autocomplete="off" placeholder="Enter your email or username" required>
                                </div>
                            </div>
                            <div class="mb-3 border-bottom"></div>
                            <div class="mb-2">
                                <label class="form-check-label">
                                    Confirming to Delete your collected data
                                </label>
                            </div>
                            <div class="mb-4">
                                <button  type="submit" name="submit_clear" class="btn btn-warning btn-lg w-100">Clear Data</button>
                            </div>
                            
                            <div class="mb-3 border-bottom"></div>
                            <div class="mb-2">
                                <label class="form-check-label">
                                    Confirming to Delete your data and Account
                                </label>
                            </div>
                            <div class="mb-3">
                                <button  type="submit" name="submit_delete" class="btn btn-danger btn-lg w-100">Delete Account & Clear Data</button>
                            </div>
                            <label class="form-check-label">
                               After submitting this you request will be reviewed and your data will be removed with in 7 days, After that you cant restore it.
                            </label>
                        </div>
                        </form>
                    </div>
                </div>
            </div>
        </div>
        
    </main>
    <!-- End: 404 -->
    
    <!-- Vendor scripts -->
    <script src="assets/js/jquery.min.js"></script>
    <script src="assets/vendors/bootstrap/bootstrap.min.js"></script>
    <script src="assets/vendors/notify/notify.min.js"></script>
    
    <?php if (isset($_SESSION['msg'])) { ?>
        <script type="text/javascript">
            $('.notifyjs-corner').empty();
            $.notify(
            '<?php echo $client_lang[$_SESSION["msg"]]; ?>', {
                position: "top right",
                className: '<?= $_SESSION["class"] ?>'
            }
            );
        </script>
        <?php
        unset($_SESSION['msg']);
        unset($_SESSION['class']);
    }?>
</body>
</html>