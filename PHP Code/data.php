<?php include("includes/db_helper.php");
 
 $page_title="No data";
 if(isset($_GET['privacy_policy'])){
    $page_title="Privacy Policy";
 } 
 else if(isset($_GET['terms'])){
    $page_title="Terms & Conditions";
 }
?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">

	<!-- Seo Meta -->
    <meta name="description" content="<?php echo $page_title;?>">
 
    <!-- Website Title -->
    <title><?php echo $page_title;?> | <?php echo APP_NAME;?></title>
    
    <!-- Favicon --> 
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="32x32">
    <link href="images/<?php echo APP_LOGO;?>" rel="icon" sizes="192x192">

    <!-- IOS Touch Icons -->
    <link rel="apple-touch-icon" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="152x152" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="180x180" href="images/<?php echo APP_LOGO;?>">
    <link rel="apple-touch-icon" sizes="167x167" href="images/<?php echo APP_LOGO;?>">

    <!-- Google font -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap" rel="stylesheet">

    <!-- Vendor styles -->
    <link rel="stylesheet" href="assets/vendors/bootstrap/bootstrap.min.css" type="text/css">
    
</head>

<style>
    p {
        color: #5e5e5e;
        margin-top: 0;
        margin-bottom: 5px;
    }
    .title {
        color: #dc3535;
    }
    
    h1, h2, h3, h4, h5, h6 {
    	color: #343434;
    }
    .nsofts-container {
        width: 100%;
        max-width: 1200px;
        margin-left: auto;
        margin-right: auto;
    }
    body {
        --ns-body-font-family: 'Inter', sans-serif;
        --ns-body-font-size: 14px;
        font-family: var(--ns-body-font-family);
        font-size: var(--ns-body-font-size);
    }
</style>

<body>
    
    <main class="d-flex justify-content-center py-1 min-vh-100">
        <div class="nsofts-container">
            <div class="card-body p-4">
                <div class="privacyHeader mb-2">
                    <h2 class="title"><?php echo $page_title;?> for <?php echo APP_NAME;?></h2>
                    <p class="mb-5">Updated <?php echo date('D m, Y ');?></p>
                </div>
                <?php if(isset($_GET['privacy_policy'])){ ?>
                    <?=stripslashes($settings_details['app_privacy_policy'])?>
                <?php } else if(isset($_GET['terms'])){ ?>
                    <?=stripslashes($settings_details['app_terms'])?>
                <?php } ?>
            </div>
        </div>
    </main>
    
    <!-- Vendor scripts -->
    <script src="assets/js/jquery.min.js"></script>
    <script src="assets/vendors/bootstrap/bootstrap.min.js"></script>

</body>
</html>