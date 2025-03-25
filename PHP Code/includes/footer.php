    <!-- Start: footer -->
    <footer id="nsofts_footer">
        <div class="nsofts-container d-sm-flex justify-content-between text-center">
            <span>Copyright © <?php echo date('Y'); ?> <a target="_blank" href="https://nemosofts.com"><span class="fw-semibold">nemosofts</span></a> All rights reserved.</span>
            <div class="d-flex justify-content-center mt-2 mt-sm-0">
                <a href="https://1.envato.market/Py1XZX" target="_blank" class="me-3">Codecanyon</a>
                <a href="https://1.envato.market/R5JMNv" target="_blank" >More products</a>
            </div>
        </div>
    </footer>
    <!-- End: footer -->
    

    <!-- Vendor scripts -->
    <script src="assets/js/jquery.min.js"></script>
    <script src="assets/vendors/bootstrap/bootstrap.min.js"></script>
    <script src="assets/vendors/notify/notify.min.js"></script>
    <script src="assets/vendors/perfect-scrollbar/perfect-scrollbar.min.js"></script>
    <script src="assets/vendors/quill/quill.min.js"></script>
    <script src="assets/vendors/select2/select2.min.js"></script>
    <script src="assets/vendors/sweetalerts2/sweetalert2.min.js"></script>
    <script src="assets/vendors/chartjs/chart.min.js"></script>

    <!-- Main script -->
    <script src="assets/js/main.js?v=1.0.0"></script>
    
    <script type="text/javascript">
    
        $(document).ready(function(event) {
            $(document).on("click", ".btn_enable_disable", function(e) {
                var _action;
                
                var _currentElement = $(this);
                var _id = $(this).data("id");
                var _table = $(this).data("table");
                var _column = $(this).data("column");
                
                var _for = $(this).prop("checked");
                if (_for == false) {
                    _action = "disable";
                } else {
                    _action = "enable";
                }
                
                $.ajax({
                    type: 'post',
                    url: 'processData.php',
                    dataType: 'json',
                    data: {id: _id, for_action: _action, table: _table, column: _column,'action':'toggle_status'},
                    success: function(res) {
                        $.notify(res.msg, { position:"top right",className: res.class} ); 
                    }
                });
            });
        });
      
        $(document).on("click", ".btn_delete", function(e){
            e.preventDefault();
            
            var _ids=$(this).data("id");
            var _table=$(this).data("table");
            
            swal({
                title: "Are you sure to delete this?",
                type: "warning",
                confirmButtonClass: 'btn btn-primary m-2',
                cancelButtonClass: 'btn btn-danger m-2',
                buttonsStyling: false,
                showCancelButton: true,
                confirmButtonText: "Yes",
                cancelButtonText: "No",
                closeOnConfirm: false,
                closeOnCancel: false,
                showLoaderOnConfirm: true
                
            }).then(function(result) {
                if (result.value) {
                    $.ajax({
                        type:'post',
                        url:'processData.php',
                        dataType:'json',
                        data:{'id':_ids,'table':_table,'for_action':'delete','action':'multi_action'},
                        success:function(res){
                            location.reload();
                        }
                    });
                } else {
                    swal.close();
                }
            });
        });
        
        function copyToClipboard(el) {
            var text = el.innerText;
            
            if (window.clipboardData && window.clipboardData.setData) {
                return window.clipboardData.setData("Text", text);
                
            } else if (document.queryCommandSupported && document.queryCommandSupported("copy")) {
                var textarea = document.createElement("textarea");
                textarea.value = text;
                textarea.style.position = "fixed";  // Prevent scrolling to bottom of page in Microsoft Edge.
                el.appendChild(textarea);
                textarea.select();
                try {
                    return document.execCommand('copy');
                } catch (ex) {
                    console.warn("Copy to clipboard failed.", ex);
                    return prompt("Copy to clipboard: Ctrl+C, Enter", text);
                } finally {
                    el.removeChild(textarea);
                }
            }
        }
    </script>  
    
    <?php if (isset($_SESSION['msg'])) { ?>
        <script type="text/javascript">
            var _class='<?php echo $_class = ($_SESSION["class"]) ? $_SESSION["class"] : "success" ?>';
            var _msg='<?php echo $client_lang[$_SESSION["msg"]]; ?>';
            _msg=_msg.replace(/(<([^>]+)>)/ig,"");
            $('.notifyjs-corner').empty();
            $.notify(_msg,{position: "top right",className: _class});
        </script>
        <?php unset($_SESSION['msg']); ?>
        <?php unset($_SESSION['class']);?>
    <?php } ?>
    
</body>
</html>