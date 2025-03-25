<?php
// Check if both admin_name and admin_type are set in the session
if (!isset($_SESSION['admin_name']) || !isset($_SESSION['admin_type'])) {
    session_unset();  // Free all session variables
    session_destroy();  // Destroy the session

    // Redirect to login page (index.php) and stop script execution
    header("Location: index.php");
    exit;
}
?>