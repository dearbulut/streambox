<?php 
    $page_title='Notification OneSignal';
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    require_once("thumbnail_images.class.php");
    
    if(isset($_POST['submit'])){
        
        if($_FILES['big_picture']['name']!=""){   
            
            $big_picture=rand(0,99999)."_".$_FILES['big_picture']['name'];
            $tpath2='images/'.$big_picture;
            move_uploaded_file($_FILES["big_picture"]["tmp_name"], $tpath2);
            
            if( isset($_SERVER['HTTPS'] ) ) {  
              $file_path = 'https://'.$_SERVER['SERVER_NAME'] . dirname($_SERVER['REQUEST_URI']).'/images/'.$big_picture;
            }else{
              $file_path = 'http://'.$_SERVER['SERVER_NAME'] . dirname($_SERVER['REQUEST_URI']).'/images/'.$big_picture;
            }
              
            $content = array(
                "en" => $_POST['notification_msg']                                                 
            );
            
            $fields = array(
                'app_id' => ONESIGNAL_APP_ID,
                'included_segments' => array('All'),                                            
                'data' => array("foo" => "bar"),
                'headings'=> array("en" => $_POST['notification_title']),
                'contents' => $content,
                'big_picture' =>$file_path                    
            );
            
            $fields = json_encode($fields);
            print("\nJSON sent:\n");
            print($fields);
            
            $ch = curl_init();
            curl_setopt($ch, CURLOPT_URL, "https://onesignal.com/api/v1/notifications");
            curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json; charset=utf-8', 'Authorization: Basic '.ONESIGNAL_REST_KEY));
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
            curl_setopt($ch, CURLOPT_HEADER, FALSE);
            curl_setopt($ch, CURLOPT_POST, TRUE);
            curl_setopt($ch, CURLOPT_POSTFIELDS, $fields);
            curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, FALSE);
            
            $response = curl_exec($ch);
            curl_close($ch);
            
        }else{
            
            $content = array(
                "en" => $_POST['notification_msg']
            );
            
            $fields = array(
                'app_id' => ONESIGNAL_APP_ID,
                'included_segments' => array('All'),                                      
                'data' => array("foo" => "bar"),
                'headings'=> array("en" => $_POST['notification_title']),
                'contents' => $content
            );
            
            $fields = json_encode($fields);
            print("\nJSON sent:\n");
            print($fields);
            
            $ch = curl_init();
            curl_setopt($ch, CURLOPT_URL, "https://onesignal.com/api/v1/notifications");
            curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json; charset=utf-8', 'Authorization: Basic '.ONESIGNAL_REST_KEY));
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
            curl_setopt($ch, CURLOPT_HEADER, FALSE);
            curl_setopt($ch, CURLOPT_POST, TRUE);
            curl_setopt($ch, CURLOPT_POSTFIELDS, $fields);
            curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, FALSE);
            
            $response = curl_exec($ch);
            
            curl_close($ch);
        }
        
        $data = array(
            'notification_title' => cleanInput($_POST['notification_title']),
            'notification_msg' => cleanInput($_POST['notification_msg']),
            'notification_description' => addslashes($_POST['notification_des']),
            'notification_on' =>  strtotime(date('d-m-Y h:i:s A')) 
        );
        $qry = Insert('tbl_notification',$data);
        
        $_SESSION['class'] = "success";
        $_SESSION['msg']="16";
        header( "Location:notification_onesignal.php");
        exit; 
    }

?>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb align-items-center">
                <li class="breadcrumb-item d-inline-flex"><a href="dashboard.php"><i class="ri-home-4-fill"></i></a></li>
                <li class="breadcrumb-item d-inline-flex active" aria-current="page"><?php echo (isset($page_title)) ? $page_title : "" ?></li>
            </ol>
        </nav>
        
        <div class="row g-4">
            <div class="col-12">
                <div class="card h-100">
                    <div class="card-body p-4">
                        <h5 class="mb-4"><?=$page_title ?></h5>
                        <form action="" name="addeditone" method="POST" enctype="multipart/form-data">
                        
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Title</label>
                                <div class="col-sm-10">
                                    <input type="text" name="notification_title" id="notification_title" value="" class="form-control" required>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Sub Message</label>
                                <div class="col-sm-10">
                                    <input type="text" name="notification_des" id="notification_des" value="" class="form-control" required>
                                </div>
                            </div>

                             <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Description</label>
                                <div class="col-sm-10">
                                    <textarea name="notification_des" id="notification_des" rows="5" class="nsofts-editor"></textarea>
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">Select Image</label>
                                <div class="col-sm-10">
                                    <input type="file" class="form-control" name="big_picture" accept="image/*">
                                </div>
                            </div>
                            
                            <div class="mb-3 row">
                                <label class="col-sm-2 col-form-label">&nbsp;</label>
                                <div class="col-sm-10">
                                    <button type="submit" name="submit" class="btn btn-primary" style="min-width: 120px;">Send</button>
                                </div>
                            </div>
                            
                        </form>
                    </div>
                </div>
            </div>
        </div>
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?> 