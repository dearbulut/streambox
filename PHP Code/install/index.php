<?php
session_start();
require("../includes/lb_helper.php");
$installFile="../includes/.lic";
$database_dump_file = 'database.sql';

$item_id = '52621164';
$item_url = 'https://codecanyon.net/item/streambox-mobile-pro-iptv-player-android-mobile-and-tablets/52621164';

$product_info = get_latest_version($item_id);
$errors = false;
$step = isset($_GET['step']) ? $_GET['step'] : '';
if(is_writeable($installFile)){
  $errors = true; 
}

?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    
    <!-- Website Title -->

    <title><?php echo (isset($product_info['product_name'])) ? $product_info['product_name'].' - Installer'  : 'PHP extension missing!'; ?></title>

    <!-- Favicon --> 
    <link href="../assets/install/logo.jpg" rel="icon" sizes="32x32">
    <link href="../assets/install/logo.jpg" rel="icon" sizes="192x192">

    <!-- IOS Touch Icons -->
    <link rel="apple-touch-icon" href="../assets/install/logo.jpg">
    <link rel="apple-touch-icon" sizes="152x152" href="../assets/install/logo.jpg">
    <link rel="apple-touch-icon" sizes="180x180" href="../assets/install/logo.jpg">
    <link rel="apple-touch-icon" sizes="167x167" href="../assets/install/logo.jpg">
    
	<!-- Google fonts -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
	<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
	<link href="https://fonts.googleapis.com/css2?family=Nunito:wght@200;300;400;500;600;700;800;900;1000&display=swap" rel="stylesheet">
    
    <!-- Stylesheets -->  
    <link rel="stylesheet" href="../assets/install/css/styles.css" media="all">    
    
    <!--[if lt IE 9]>
        <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
        <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
    
</head>

<!-- Scripts -->
<script>
	window.onload = (event) => {
		var card = document.getElementById('card');
		card.classList.add('show');
	};
</script>

<body>
 	<div id="wrapper">
		<div class="container">
			<div class="setup">
				<div id="card" class="card">
					<div class="card__body">
					    <?php switch ($step) { default: ?>
					        <?php  
                                if(phpversion() < "7.4"){
                                    $errors = true;
                                }
                                if(!extension_loaded('mysqli')){
                                    $errors = true; 
                                }
                                if(!extension_loaded('curl')){
                                    $errors = true; 
                                }
                                if(!extension_loaded('pdo')){
                                    $errors = true; 
                                }
                                if(!extension_loaded('json')){
                                    $errors = true; 
                                }
                                if(is_writeable($installFile)){
                                    $errors = true; 
                                }
                                if (empty($product_info)) {
                                    $errors = false; 
                                }
                            ?>
                            
                            <div class="card__image">
                                <?php if($errors==true){?>
                                    <img src="../assets/install/images/server_error.png" alt="">
                                <?php } else { ?>
                                    <img src="../assets/install/images/server_success.png" alt="">
                                <?php } ?>
                            </div>
    				        <div class="card__content">
        						<div class="card__content__head">
        							<h3 class="card__title">
        								<span><a target="_blank" class="text-red" href="<?php echo $item_url; ?>"><?php echo $product_info['product_name']; ?></a></span>
        							</h3>
        							<p class="card__fade">This php extensions Are must needed! If you server don't have this Ask you server provider to enable it. This are commonly used php extension in all Hosting's.</p>
        						</div>
        						<div class="card__fade">
        							<div class="notify-list">
        							     <?php  
                                            // Add or remove your script's requirements below
                                            if(is_writeable($installFile)){
                                                echo "<div class='notify notify--error'>
                                                <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                <path fill='none' d='M0 0h24v24H0z'/>
                                                <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                </svg>
                                                <span class='notify__text'>The installation process is already complete !</span>
                                                </div>";
                                            } else {
                                                
                                                if(phpversion() < "7.4"){
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>Current PHP version is ".phpversion()."! minimum PHP 7.4 or higher required.</span>
                                                    </div>";
                                                } else {
                                                    echo "<div class='notify notify--success'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                                    </svg> 
                                                    <span class='notify__text'>You are running PHP version ".phpversion()."</span>
                                                    </div>";
                                                }
                                                if(!extension_loaded('mysqli')){
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>MySQLi PHP extension missing!</span>
                                                    </div>";
                                                } else {
                                                    echo "<div class='notify notify--success'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                                    </svg>
                                                    <span class='notify__text'>MySQLi PHP extension available</span>
                                                    </div>";
                                                } 
                                                if(!extension_loaded('curl')){
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>Curl PHP extension missing!</span>
                                                    </div>";
                                                } else {
                                                    echo "<div class='notify notify--success'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                                    </svg>
                                                    <span class='notify__text'>Curl PHP extension available</span>
                                                    </div>";
                                                }
                                                if(!extension_loaded('pdo')){
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>PDO PHP extension missing!</span>
                                                    </div>";
                                                } else {
                                                    echo "<div class='notify notify--success'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                                    </svg>
                                                    <span class='notify__text'>PDO PHP extension available</span>
                                                    </div>";
                                                }
                                                if(!extension_loaded('json')){
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>JSON PHP extension missing!</span>
                                                    </div>";
                                                } else {
                                                    echo "<div class='notify notify--success'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z'/>
                                                    </svg>
                                                    <span class='notify__text'>JSON PHP extension available</span>
                                                    </div>";
                                                }
                                                
                                                if (empty($product_info)) {
                                                    echo "<div class='notify notify--error'>
                                                    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='24' height='24'>
                                                    <path fill='none' d='M0 0h24v24H0z'/>
                                                    <path fill='currentColor' d='M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z'/>
                                                    </svg>
                                                    <span class='notify__text'>PHP extension missing!</span>
                                                    </div>";
                                                }
                                                
                                            }
                                        ?>
        							</div>
        							<div class="card__content__foot">
        								<p>Copyright © <?php echo date('Y'); ?> <a target="_blank" class="text-red" href="https://nemosofts.com">nemosofts</a> All rights reserved.</p>
        								<div class="text-right">
        								    <a href="<?php echo $item_url; ?>" target="_blank" class="btn btn--primary btn--slide is-danger" style="min-width: 115px;">BUY</a>
        								    <?php if($errors==true){ ?>
        								        <button type="button" id="next" class="btn btn--primary btn--slide" style="min-width: 115px;" disabled>Next</button>
                                            <?php }else{ ?>
                                                <a href="index.php?step=0" class="btn btn--primary btn--slide" style="min-width: 115px;">Next</a>
                                            <?php } ?>
        								</div>
        							</div>
        						</div>
    					    </div>
					    
					    <?php break; case "0": ?>
					    
                            <?php
                              $license_code = null;
                              $client_name = null;
                              if(!empty($_POST['license'])&&!empty($_POST['client'])){
                                $license_code = strip_tags(trim($_POST["license"]));
                                $client_name = strip_tags(trim($_POST["client"]));
                                
                                $activate_response = activate_license($license_code,$client_name,$item_id);
                                
                                $_SESSION['envato_buyer_name']=$client_name;
                                $_SESSION['envato_purchase_code']=$license_code;
            
                                if(empty($activate_response)){
                                  $msg='Server is unavailable.';
                                } else {
                                  $msg=$activate_response['message'];
                                }
                                if($activate_response['status'] != true){ ?>
                                
                                    <div class="card__image">
            							<img src="../assets/install/images/verified_error.png" alt="">
            						</div>
            						<div class="card__content">
            							<div class="card__content__head">
            								<h3 class="card__title">
            									<span>Verify Envato Purchase Code</span>
            								</h3>
            								<ol class="card__fade">
            									<li>Log into your Envato Market account.</li>
            									<li>Hover the mouse over your username at the top of the screen.</li>
            									<li>Click ‘Downloads’ from the drop-down menu.</li>
            									<li>Click ‘License certificate & purchase code’.</li>
            								</ol>
            							</div>
            							<div class="card__fade">
            							    <form action="index.php?step=0" method="POST">
                								<div class="mb-48">
                								    <div class="notify notify--error" style="margin-bottom: 18px;">
                										<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                											<path fill="none" d="M0 0h24v24H0z"/>
                											<path fill="currentColor" d="M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z"/>
                										</svg>
                										<span class="notify__text"><?php echo ucfirst($msg); ?></span>
                									</div>
            									
                									<label for="name" class="form-label">Envato user name</label>
                									<div class="form-group">
                										<svg class="form-group__icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                											<path fill="none" d="M0 0h24v24H0z"/>
                											<path d="M20 22h-2v-2a3 3 0 0 0-3-3H9a3 3 0 0 0-3 3v2H4v-2a5 5 0 0 1 5-5h6a5 5 0 0 1 5 5v2zm-8-9a6 6 0 1 1 0-12 6 6 0 0 1 0 12zm0-2a4 4 0 1 0 0-8 4 4 0 0 0 0 8z"/>
                										</svg>
                										<input class="form-control" type="text" placeholder="Enter your envato user name" name="client" autocomplete="off" required>
                									</div>
                									<label for="code" class="form-label">Purchase code</label>
                									<div class="form-group">
                										<svg class="form-group__icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                											<path fill="none" d="M0 0h24v24H0z"/>
                											<path d="M12 1l8.217 1.826a1 1 0 0 1 .783.976v9.987a6 6 0 0 1-2.672 4.992L12 23l-6.328-4.219A6 6 0 0 1 3 13.79V3.802a1 1 0 0 1 .783-.976L12 1zm0 2.049L5 4.604v9.185a4 4 0 0 0 1.781 3.328L12 20.597l5.219-3.48A4 4 0 0 0 19 13.79V4.604L12 3.05zM12 7a2 2 0 0 1 1.001 3.732L13 15h-2v-4.268A2 2 0 0 1 12 7z"/>
                										</svg>
                										<input class="form-control mb-8" type="text" placeholder="Enter your item purchase code" name="license" autocomplete="off" required>
                										<a href="https://help.market.envato.com/hc/en-us/articles/202822600-Where-Is-My-Purchase-Code" class="text-red" target="_blank">Where Is My Purchase Code?</a>
                									</div>
                								</div>
                								<div class="card__content__foot">
                									<div class="text-right">
                									     <button type="submit" class="btn btn--primary btn--slide" style="min-width: 124px;">Verify</button>
                									</div>
                								</div>
            								</form>
            							</div>
            						</div>
        						
                                
                                <?php }else{ ?>
                                
                                
                                <div class="card__image">
            							<img src="../assets/install/images/verified_success.png" alt="">
            						</div>
            						<div class="card__content">
            							<div class="card__content__head">
            								<h3 class="card__title">
            									<span>Verify Envato Purchase Code</span>
        									
            								</h3>
            							</div>
            							<div class="card__fade">
            							    <form action="index.php?step=1" method="POST">
                								<div class="mb-48">
                								    <div class="notify notify--success">
                										<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                											<path fill="none" d="M0 0h24v24H0z"/>
                											<path fill="currentColor" d="M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z"/>
                										</svg>
                										<span class="notify__text"><?php echo ucfirst($msg); ?></span>
                									</div>
                									<input type="hidden" name="lcscs" id="lcscs" value="<?php echo ucfirst($activate_response['status']); ?>">
                								</div>
                								<div class="card__content__foot">
                									<div class="text-right">
                									    <button type="submit" class="btn btn--primary btn--slide" style="min-width: 124px;">Next</button>
                									</div>
                								    
                								</div>
            								</form>
            							</div>
            						</div>
                                
                                <?php } ?>
                                
                            <?php }else{ ?>
                            
                                <div class="card__image">
        							<img src="../assets/install/images/verified.png" alt="">
        						</div>
        						<div class="card__content">
        							<div class="card__content__head">
        								<h3 class="card__title">
        									<span>Verify Envato Purchase Code</span>
        								</h3>
        								<ol class="card__fade">
        									<li>Log into your Envato Market account.</li>
        									<li>Hover the mouse over your username at the top of the screen.</li>
        									<li>Click ‘Downloads’ from the drop-down menu.</li>
        									<li>Click ‘License certificate & purchase code’.</li>
        								</ol>
        							</div>
        							<div class="card__fade">
        							    <form action="index.php?step=0" method="POST">
            								<div class="mb-48">
            									<label for="name" class="form-label">Envato user name</label>
            									<div class="form-group">
            										<svg class="form-group__icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
            											<path fill="none" d="M0 0h24v24H0z"/>
            											<path d="M20 22h-2v-2a3 3 0 0 0-3-3H9a3 3 0 0 0-3 3v2H4v-2a5 5 0 0 1 5-5h6a5 5 0 0 1 5 5v2zm-8-9a6 6 0 1 1 0-12 6 6 0 0 1 0 12zm0-2a4 4 0 1 0 0-8 4 4 0 0 0 0 8z"/>
            										</svg>
            										<input class="form-control" type="text" placeholder="Enter your envato user name" name="client" autocomplete="off" required>
            									</div>
            									<label for="code" class="form-label">Purchase code</label>
            									<div class="form-group">
            										<svg class="form-group__icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
            											<path fill="none" d="M0 0h24v24H0z"/>
            											<path d="M12 1l8.217 1.826a1 1 0 0 1 .783.976v9.987a6 6 0 0 1-2.672 4.992L12 23l-6.328-4.219A6 6 0 0 1 3 13.79V3.802a1 1 0 0 1 .783-.976L12 1zm0 2.049L5 4.604v9.185a4 4 0 0 0 1.781 3.328L12 20.597l5.219-3.48A4 4 0 0 0 19 13.79V4.604L12 3.05zM12 7a2 2 0 0 1 1.001 3.732L13 15h-2v-4.268A2 2 0 0 1 12 7z"/>
            										</svg>
            										<input class="form-control mb-8" type="text" placeholder="Enter your item purchase code" name="license" autocomplete="off" required>
            										<a href="https://help.market.envato.com/hc/en-us/articles/202822600-Where-Is-My-Purchase-Code" class="text-red" target="_blank">Where Is My Purchase Code?</a>
            									</div>
            								</div>
            								<div class="card__content__foot">
            									<div class="text-right">
            									    <button type="submit" class="btn btn--primary btn--slide" style="min-width: 124px;">Verify</button>
            									</div>
            								</div>
        								</form>
        							</div>
        						</div>
                            
                            <?php } ?>
					    
					    <?php break; case "1": ?>
					    
					        <?php if($_POST && isset($_POST["lcscs"])){ 
                                
                                $valid = strip_tags(trim($_POST["lcscs"]));
                                $db_host = strip_tags(trim($_POST["host"]));
                                $db_user = strip_tags(trim($_POST["user"]));
                                $db_pass = strip_tags(trim($_POST["pass"]));
                                $db_name = strip_tags(trim($_POST["name"]));
                                // Let's import the sql file into the given database
                                
                                    if(!empty($db_host)){
                                        
                                      $con = @mysqli_connect($db_host, $db_user, $db_pass, $db_name);
                                      if(mysqli_connect_errno()){ ?>
                                      
                                          <div class="card__image">
                    							<img src="../assets/install/images/database.png" alt="">
                    						</div>
                    						<div class="card__content">
                    							<div class="card__content__head">
                    								<h3 class="card__title">
                    									<span>Database</span>
                    								</h3>
                    							</div>
                    							<div class="card__fade">
                    							    <form action="index.php?step=1" method="POST">
                    							        <div class="notify notify--error" style="margin-bottom: 18px;">
                    										<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                    											<path fill="none" d="M0 0h24v24H0z"/>
                    											<path fill="currentColor" d="M12 10.586l4.95-4.95 1.414 1.414-4.95 4.95 4.95 4.95-1.414 1.414-4.95-4.95-4.95 4.95-1.414-1.414 4.95-4.95-4.95-4.95L7.05 5.636z"/>
                    										</svg>
                    										<span class="notify__text">Failed to connect to MySQL: <?php echo mysqli_connect_error(); ?></span>
                    									</div>
                									
                        								<input type="hidden" name="lcscs" id="lcscs" value="<?php echo $valid; ?>">
                                                
                                                        <div class="mb-16">
            												<label for="host" class="form-label">Database Host</label>
            												<input class="form-control" type="text" id="host" placeholder="enter your database host" name="host" value="localhost" required>
            											</div>
            											<div class="mb-16">
            												<label for="username" class="form-label">Database Username</label>
            												<input class="form-control" type="text" id="user" placeholder="enter your database username" name="user" required>
            											</div>
            											<div class="mb-16">
            												<label for="password" class="form-label">Database Password</label>
            												<input class="form-control" type="text" id="pass" placeholder="enter your database password" name="pass">
            											</div>
            											<div class="mb-16">
            												<label for="name" class="form-label">Database Name</label>
            												<input class="form-control" type="text" id="name" placeholder="enter your database name" name="name" required>
            											</div>
                        								<div class="card__content__foot">
                        									<div class="text-right">
                        										<button type="submit" id="next" class="btn btn--primary btn--slide" style="min-width: 124px;">Import</button>
                        									</div>
                        								</div>
                        								<p style="margin-top: 10px;">Copyright © <?php echo date('Y'); ?> <a target="_blank" class="text-red"  href="https://nemosofts.com">nemosofts</a> All rights reserved.</p>
                    								</form>
                    							</div>
                    						</div>
                                    <?php
                                        exit;
                                      }
                                      $templine = '';
                                      $lines = file($database_dump_file);
                                      foreach($lines as $line){
                                        if(substr($line, 0, 2) == '--' || $line == '')
                                          continue;
                                        $templine .= $line;
                                        $query = false;
                                        if(substr(trim($line), -1, 1) == ';'){
                                          $query = mysqli_query($con, $templine);
                                          $templine = '';
                                        }
                                      }
                                      
                                      $dataFile = "../includes/db_helper.php";
                                      $fhandle = fopen($dataFile,"r");
                                      $content = fread($fhandle,filesize($dataFile));
                                      $content = str_replace('db_name', $db_name, $content);
                                      $content = str_replace('db_uname', $db_user, $content);
                                      $content = str_replace('db_password', $db_pass, $content);
                                      $content = str_replace('db_hname', $db_host, $content);
                                      $fhandle = fopen($dataFile,"w");
                                      fwrite($fhandle,$content);
                                      fclose($fhandle);

                                      mysqli_autocommit($con,FALSE);
                
                                      // Update envato client details
                                      $sqlUpdate="UPDATE tbl_settings SET 
                                        `envato_buyer_name` = '".$_SESSION['envato_buyer_name']."',
                                        `envato_purchase_code` = '".$_SESSION['envato_purchase_code']."',
                                        `envato_package_name` = '' WHERE `id` = 1";
                
                                      $result=mysqli_query($con, $sqlUpdate) or die(mysqli_error($con));
                                      
                                      // Commit transaction
                                      if (!mysqli_commit($con)) {
                                        echo "Commit transaction failed";
                                        exit();
                                      }
                                      
                                      // Close connection
                                      mysqli_close($con);
                                      
                                    ?>
                                    
                                        <div class="card__image">
                							<img src="../assets/install/images/database.png" alt="">
                						</div>
                						<div class="card__content">
                							<div class="card__content__head">
                								<h3 class="card__title">
                									<span>Database</span>
                								</h3>
                							</div>
                							<div class="card__fade">
                							    <form action="index.php?step=2" method="POST">
                    								<div class="mb-48">
                    								    <div class="notify notify--success">
                    										<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                    											<path fill="none" d="M0 0h24v24H0z"/>
                    											<path fill="currentColor" d="M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z"/>
                    										</svg>
                    										<span class="notify__text">Database was successfully imported.</span>
                    									</div>
                    									<input type="hidden" name="dbscs" id="dbscs" value="true">
                    								</div>
                    								<div class="card__content__foot">
                    									<div class="text-right">
                    									    <button type="submit" class="btn btn--primary btn--slide" style="min-width: 124px;">Next</button>
                    									</div>
                    								</div>
                								</form>
                							</div>
                						</div>

                                    <?php }else{ ?>
                                    
                                        <div class="card__image">
                							<img src="../assets/install/images/database.png" alt="">
                						</div>
                						<div class="card__content">
                							<div class="card__content__head">
                								<h3 class="card__title">
                									<span>Database</span>
                								</h3>
                							</div>
                							<div class="card__fade">
                							    <form action="index.php?step=1" method="POST">
                    								<input type="hidden" name="lcscs" id="lcscs" value="<?php echo $valid; ?>">
                                            
                                                    <div class="mb-16">
        												<label for="host" class="form-label">Database Host</label>
        												<input class="form-control" type="text" id="host" placeholder="enter your database host" name="host" value="localhost" required>
        											</div>
        											<div class="mb-16">
        												<label for="username" class="form-label">Database Username</label>
        												<input class="form-control" type="text" id="user" placeholder="enter your database username" name="user" required>
        											</div>
        											<div class="mb-16">
        												<label for="password" class="form-label">Database Password</label>
        												<input class="form-control" type="text" id="pass" placeholder="enter your database password" name="pass">
        											</div>
        											<div class="mb-16">
        												<label for="name" class="form-label">Database Name</label>
        												<input class="form-control" type="text" id="name" placeholder="enter your database name" name="name" required>
        											</div>
                    								<div class="card__content__foot">
                    									<div class="text-right">
                    										<button type="submit" id="next" class="btn btn--primary btn--slide" style="min-width: 124px;">Import</button>
                    									</div>
                    								</div>
                    								<p style="margin-top: 10px;">Copyright © <?php echo date('Y'); ?> <a target="_blank" class="text-red" href="https://nemosofts.com">nemosofts</a> All rights reserved.</p>
                								</form>
                							</div>
                						</div>
                                    
                                    <?php } ?>

                                <?php }else{ ?>
                                    
                                    <h2 style="color: #f44336c7;">Sorry, something went wrong.</h2>

                                <?php } ?>
					    
					    <?php break; case "2": ?>
					    
    					    <?php if($_POST && isset($_POST["dbscs"])){
                                session_destroy();
                            ?>
                                <div class="card__image">
        							<img src="../assets/install/images/finish.png" alt="">
        						</div>
        						<div class="card__content">
        							<div class="card__content__head">
        								<h3 class="card__title">
        									<span>Finish</span>
        								</h3>
        							</div>
        							<div class="card__fade">
        								<div class="notify notify--success mb-40">
                                            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="24" height="24">
                                                <path fill="none" d="M0 0h24v24H0z"/>
                                                <path fill="currentColor" d="M10 15.172l9.192-9.193 1.415 1.414L10 18l-6.364-6.364 1.414-1.414z"/>
                                            </svg>
                                            <span class="notify__text"><?php echo $product_info['product_name']; ?> is successfully installed.</span>
                                        </div>
                                        <p>You can now login using your username: <b style="color: #f44336c7;">admin</b> and default password: <b style="color: #f44336c7;">admin</b></p>
                                        <p>The first thing you should do is change your account details.</p>
                                        <div class="text-center mb-48">
                                            <a href="../index.php" class="btn btn--primary">Let's go</a>
                                        </div>
                                        <h3 class="card__title">Support</h3>
                                        <div class="card__foot">
                                            <p>We provide support through Email or Telegram. <br>
                                            <b>Email:</b> <a href="mailto:info.nemosofts@gmail.com" style="color: #2196f3;">info.nemosofts@gmail.com</a> <br>
                                            <b>Telegram:</b> <a href="https://t.me/nemosofts" style="color: #2196f3;">@nemosofts</a> </p>
                                        </div>
        								<div class="card__content__foot text-center">
        									<p>Thank you for purchasing our products</p>
        								</div>
        							</div>
        						</div>
        
                            <?php } else { ?>
                            
                                <h2 style="color: #f44336c7;">Sorry, something went wrong.</h2>
                                
                            <?php } break; } ?>
					</div>
				</div>
			</div>
		</div>
	</div>
</body>
</html>