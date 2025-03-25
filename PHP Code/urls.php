<?php 
    $page_title="URLs";
    include("includes/header.php");
    require("includes/lb_helper.php");

    $file_path = getBaseUrl();
    
    $privacy_policy_file_path = getBaseUrl().'data.php?privacy_policy';
    $terms_file_path = getBaseUrl().'data.php?terms';
    $delete_request_file_path = getBaseUrl().'account_delete_request.php';
    
?>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        
        <div class="row g-4 mb-3">
            <div class="col-12">
                <div class="card h-100">
                    <div class="card-body p-3">
                        <h5 class="mb-3"><?php echo (isset($page_title)) ? $page_title : "" ?></h5>
                        <div class="pb-clipboard mb-2">
                            <span class="pb-clipboard__url"><a class="fw-bold text-decoration-none" style="color: var(--ns-primary);">base_url :  </a><span id="clipboard_base_url"><?=$file_path ?></span></span>
                            <a class="pb-clipboard__link btn_base_url" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                </svg>
                            </a>
                        </div>
                        
                        
                        <div class="pb-clipboard mb-2">
                            <span class="pb-clipboard__url"><a class="fw-bold text-decoration-none" style="color: var(--ns-primary);">Privacy Policy :  </a><span id="clipboard_policy"><?=$privacy_policy_file_path ?></span></span>
                            <a class="pb-clipboard__link btn_policy" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                </svg>
                            </a>
                        </div>

                        <div class="pb-clipboard mb-2">
                            <span class="pb-clipboard__url"><a class="fw-bold text-decoration-none" style="color: var(--ns-primary);">Terms & Conditions :  </a><span id="clipboard_terms"><?=$terms_file_path ?></span></span>
                            <a class="pb-clipboard__link btn_terms" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                </svg>
                            </a>
                        </div>
                    </div>
                </div>
            </div>
        </div>
        
        <div class="row g-4 mb-3">
            <div class="col-12">
                <div class="card h-100">
                    <div class="card-body p-3">
                        <h5 class="mb-3">Play Store Policy</h5>
                        <div class="pb-clipboard mb-2">
                            <span class="pb-clipboard__url"><a class="fw-bold text-decoration-none" style="color: var(--ns-primary);">Account Delete Request :  </a><span id="clipboard_delete"><?=$delete_request_file_path ?></span></span>
                            <a class="pb-clipboard__link btn_delete_ac" href="javascript:void(0);" data-clipboard-action="copy" data-clipboard-target="#clipboard_base_url" data-bs-toggle="tooltip" data-bs-custom-class="custom-tooltip" data-bs-placement="top" title="Copy url">
                                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
                                </svg>
                            </a>
                        </div>
                    </div>
                </div>
            </div>
        </div>

    </div>
</main>
<!-- End: main -->

<?php include("includes/footer.php");?> 

<script type="text/javascript">
        $(document).ready(function(event) {
            $(document).on("click", ".btn_base_url", function(e) {
                var el = document.getElementById('clipboard_base_url');
                var successful = copyToClipboard(el);
                if (successful) {
                    $.notify('Copied!', { position:"top right",className: 'success'} );
                } else {
                    $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
                }
            });
            $(document).on("click", ".btn_policy", function(e) {
                var el = document.getElementById('clipboard_policy');
                var successful = copyToClipboard(el);
                if (successful) {
                    $.notify('Copied!', { position:"top right",className: 'success'} );
                } else {
                    $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
                }
            });
            $(document).on("click", ".btn_terms", function(e) {
                var el = document.getElementById('clipboard_terms');
                var successful = copyToClipboard(el);
                if (successful) {
                    $.notify('Copied!', { position:"top right",className: 'success'} );
                } else {
                    $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
                }
            });
            $(document).on("click", ".btn_delete_ac", function(e) {
                var el = document.getElementById('clipboard_delete');
                var successful = copyToClipboard(el);
                if (successful) {
                    $.notify('Copied!', { position:"top right",className: 'success'} );
                } else {
                    $.notify('Whoops, not copied!', { position:"top right",className: 'error'} );
                }
            });
        });
    
    </script>