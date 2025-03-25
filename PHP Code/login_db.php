<?php
session_start();
include("includes/db_helper.php");

// Sanitize and validate the inputs (Use FILTER_SANITIZE_FULL_SPECIAL_CHARS instead of FILTER_SANITIZE_STRING)
$username = filter_input(INPUT_POST, 'user_login', FILTER_SANITIZE_FULL_SPECIAL_CHARS);
$password = filter_input(INPUT_POST, 'nsofts_password_input', FILTER_SANITIZE_FULL_SPECIAL_CHARS);

// Check if username or password is empty
if (empty($username)) {
    $_SESSION['class'] = "error";
    $_SESSION['msg'] = "1"; // Username field is empty
    header("Location: index.php");
    exit;
} elseif (empty($password)) {
    $_SESSION['class'] = "error";
    $_SESSION['msg'] = "2"; // Password field is empty
    header("Location: index.php");
    exit;
} else {
    
    $qry="select * from tbl_admin where username='".$username."'";
	$result=mysqli_query($mysqli,$qry);		
	if(mysqli_num_rows($result) > 0){
        $row=mysqli_fetch_assoc($result);
        
        if($row['status']==0){
            
            $_SESSION['class']="error";
            $_SESSION['msg']="approve_admin"; 
            header( "Location:index.php");
            exit; 
			
		} else {
            
            if($row['password']==md5($password)) {
                
                $_SESSION['id']=$row['id'];
                $_SESSION['admin_name']=$row['username'];
                $_SESSION['admin_type']=$row['admin_type'];
                
                $_SESSION['class']="success"; 
                $_SESSION['msg']="17"; 
                
                header( "Location:dashboard.php");
                exit;
                
            } else {
                
                $_SESSION['class']="error";
                $_SESSION['msg']="4"; 
                header( "Location:index.php");
                exit;
                
            }
        }
        
	} else {
        
		$_SESSION['class']="error";
		$_SESSION['msg']="4"; 
		header( "Location:index.php");
		exit; 
	}
}
?>
