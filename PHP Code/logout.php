<?php
session_start();

// Clear specific session variables
unset($_SESSION["admin_name"]);
unset($_SESSION["admin_type"]);

// Destroy the entire session
session_destroy();

// Redirect to index page
header("Location: index.php");
exit;
?>