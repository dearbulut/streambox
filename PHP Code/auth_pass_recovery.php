<?php
    include("includes/db_helper.php");
    include("includes/lb_helper.php");
    include("language/language.php");
    include("language/api_language.php");
    include("language/app_language.php");
    include("smtp_email.php");
    date_default_timezone_set("Asia/Colombo");
    $file_path = getBaseUrl();
    
    function generateRandomPassword($length = 10) {
        $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
        $charactersLength = strlen($characters);
        $randomString = '';
        for ($i = 0; $i < $length; $i++) {
            $randomString .= $characters[rand(0, $charactersLength - 1)];
        }
        return $randomString;
    }
    
    if(isset($_POST['submit'])){
        
        $email=addslashes(trim($_POST['forgot_email']));
        
    	$qry = "SELECT * FROM tbl_admin WHERE email = '$email' AND `id` <> 0"; 
    	$result = mysqli_query($mysqli,$qry);
    	$row = mysqli_fetch_assoc($result);
	
    	if($row['email']!=""){
    		$password=generateRandomPassword(7);
    		
    		$new_password = $password;
    
    		$to = $row['email'];
    		$recipient_name=$row['username'];
    		// subject
    		$subject = str_replace('###', APP_NAME, $app_lang['forgot_password_sub_lbl']);
     		
    		$message='
            <div style="background-color: #f9f9f9;" align="center"><br />
            <table style="font-family: OpenSans,sans-serif; color: #666666;" border="0" width="600" cellspacing="0" cellpadding="0" align="center" bgcolor="#FFFFFF">
            <tbody>
            <tr>
            <td colspan="2" bgcolor="#FFFFFF" align="center"><img src="'.$file_path.'images/'.APP_LOGO.'" alt="header" style="width:100px;height:auto"/></td>
            </tr>
            <tr>
            <td width="600" valign="top" bgcolor="#FFFFFF"><br>
            <table style="font-family:OpenSans,sans-serif; color: #666666; font-size: 10px; padding: 15px;" border="0" width="100%" cellspacing="0" cellpadding="0" align="left">
            <tbody>
            <tr>
            <td valign="top"><table border="0" align="left" cellpadding="0" cellspacing="0" style="font-family:OpenSans,sans-serif; color: #666666; font-size: 10px; width:100%;">
            <tbody>
            <tr>
            <td>
            <p style="color: #262626; font-size: 24px; margin-top:0px;"><strong>'.$app_lang['dear_lbl'].' '.$row['username'].'</strong></p>
            <p style="color:#262626; font-size:20px; line-height:32px;font-weight:500;margin-top:5px;"><br>'.$app_lang['your_password_lbl'].': <span style="font-weight:400;">'.$new_password.'</span></p>
            <p style="color:#262626; font-size:17px; line-height:32px;font-weight:500;margin-bottom:30px;">'.$app_lang['thank_you_lbl'].' '.APP_NAME.'</p>
            </td>
            </tr>
            </tbody>
            </table></td>
            </tr>
            </tbody>
            </table></td>
            </tr>
            <tr>
            <td style="color: #262626; padding: 20px 0; font-size: 18px; border-top:5px solid #52bfd3;" colspan="2" align="center" bgcolor="#ffffff">'.$app_lang['email_copyright'].' '.APP_NAME.'.</td>
            </tr>
            </tbody>
            </table>
            </div>';
    
    		send_email($to,$recipient_name,$subject,$message);
    
            $data = array(
              'password'  =>  md5(trim($new_password))
            );
            
            $update_edit=Update('tbl_admin', $data, "WHERE id = '".$row['id']."'");
            
            $_SESSION['msg']="20";
            $_SESSION['class']='success'; 
            header("Location:auth_pass_recovery.php");	 
            exit;
    	} else { 	 
    		$_SESSION['msg']="21";
            $_SESSION['class']='error'; 
            header("location:auth_pass_recovery.php");	 
            exit;
    	}
    }
	
?>

 
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">

	<!-- Seo Meta -->
    <meta name="description" content="Admin panel | Dashboard">
    <meta name="keywords" content="css3, html5">
    
    <!-- Website Title -->
    <title>Forgot Password | <?php echo APP_NAME;?></title>
    
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
   <link rel="stylesheet" href="assets/vendors/perfect-scrollbar/perfect-scrollbar.min.css" type="text/css">
   <link rel="stylesheet" href="assets/vendors/remixicon/remixicon.min.css" type="text/css">
   <link rel="stylesheet" href="assets/vendors/quill/quill.min.css" type="text/css">
   <link rel="stylesheet" href="assets/vendors/select2/select2.min.css" type="text/css">
   <link rel="stylesheet" href="assets/vendors/sweetalerts2/sweetalert2.min.css" type="text/css">

   <!-- Main style -->
   <link rel="stylesheet" href="assets/css/styles.css?v=1.0.0" type="text/css">
    
</head>
<body>
    
    <!-- Loader -->
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
                    <img src="assets/images/pattern-1.svg" class="nsofts-auth__pattern-1 position-absolute" alt="">
                    <img src="assets/images/pattern-2.svg" class="nsofts-auth__pattern-2 position-absolute" alt="">
                    <div class="card position-relative">
                        <div class="card-body px-4 py-4">
                            
                            <form action="" method="POST" enctype="multipart/form-data">
                                <div class="mb-4">
                                    <h5>Forgot password!</h5>
                                    <p>Enter your register email to get password on your mail id.</p>
                                </div>

                                <div class="mb-3">
                                    <div class="nsofts-input-icon nsofts-input-icon--left">
                                        <label for="email" class="nsofts-input-icon__left">
                                            <i class="ri-at-line"></i>
                                        </label>
                                        <input type="text" name="forgot_email" id="forgot_email"  class="form-control" autocomplete="off" placeholder="Enter your email " required>
                                    </div>
                                </div>
                                
                                <div class="mb-3">
                                    <button  type="submit" name="submit" class="btn btn-primary btn-lg w-100">Send mail</button>
                                </div>
                                <p class="text-center">Go to login page? <a href="index.php" class="text-decoration-none">Login</a></p>
                                
                            </form>
                        </div>
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
    <script src="assets/vendors/perfect-scrollbar/perfect-scrollbar.min.js"></script>
    <script src="assets/vendors/quill/quill.min.js"></script>
    <script src="assets/vendors/select2/select2.min.js"></script>

    <!-- Main script -->
    <script src="assets/js/main.js?v=1.0.0"></script>
    
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