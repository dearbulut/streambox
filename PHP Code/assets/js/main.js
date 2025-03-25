"use strict";

const Bundle = function() {

    const show = 'show';
    const open = 'open';
    const active = 'active';

    const $body = $('body');
    
    let analyticsChart;

    /**
     * Check for dark mode
     * @returns {boolean}
     */
    const isDarkMode = function() {
        return window.localStorage.getItem('dark_mode') === 'true';
    }


    /**
     * Get CSS variable value
     * @param {string} name 
     * @returns {string}
     */
    const getCSSVarValue = function(name) {
        var hex = getComputedStyle(document.documentElement).getPropertyValue('--ns-' + name);
            if (hex && hex.length > 0) {
                hex = hex.trim();
            }
            return hex;
    }

    /**
     * Page loader
     */
    const loader = function() {
        const loading = $('#nsofts_loader');
        loading.fadeOut(500);
    }

    /**
     * Initialize perfect scrollbar
     */
    const initScrollbar = function() {
        $('[data-scroll="true"]').each(function() {
            // Bind perfect scrollbar with element.
            new PerfectScrollbar(this, {
                wheelSpeed: 2,
                swipeEasing: true,
                wheelPropagation: false,
                minScrollbarLength: 40
            });
        });
    }

    /**
     * Initialize select2
     */
    const initSelect = function() {
        $('.nsofts-select').select2();
    }

    /**
     * Initialize quill editor
     */
    const initEditor = function() {
        const toolbarOptions = [
            ['bold', 'italic', 'underline', 'strike'], 
            ['blockquote', 'code-block'],
            
            [{ 'list': 'ordered'}, { 'list': 'bullet' }],
            [{ 'script': 'sub'}, { 'script': 'super' }],
            [{ 'indent': '-1'}, { 'indent': '+1' }],
            [{ 'direction': 'rtl' }],
          
            [{ 'size': ['small', false, 'large', 'huge'] }],
            [{ 'header': [1, 2, 3, 4, 5, 6, false] }],
          
            [{ 'color': [] }, { 'background': [] }],
            [{ 'font': [] }],
            [{ 'align': [] }],
          
            ['clean']
        ];

        $('.nsofts-editor').each(function(i) {
            const editor = document.createElement('div');
            editor.className = this.className;
            editor.innerHTML = this.value;
            
            this.classList.add('d-none');
            $(this).parent().append(editor);
            
            // console.log(editor);
            const quill = new Quill(editor, {
                theme: 'snow',
                placeholder: 'Write here...',
                modules: {
                    toolbar: toolbarOptions
                },
            });
            
            quill.on('text-change', () => {
                $(this).html(quill.root.innerHTML);
            });
        });
    }



    /**
     * Initialize charts
     */
    const initChart = function() {
        globalChartSettings();
        analytics();
    }

    /**
     * Theme sidebar
     */
    const sidebar = function() {
        const hamburger = $('#nsofts_hamburger');
        const hamburger_top = $('#nsofts_hamburger_top');
        const compact = 'nsofts-compact-sidebar';
        const compactSidebar = 'compact_sidebar';

        if (window.localStorage.getItem(compactSidebar) === 'true') {
            $body.addClass(compact);
            hamburger.addClass(active);
            hamburger_top.addClass(active);
        }

        hamburger.on('click', function() {
            hamburger.toggleClass(active);
            hamburger_top.toggleClass(active);
            $body.toggleClass(compact);
            $body.hasClass(compact) ? window.localStorage.setItem(compactSidebar, true) : window.localStorage.removeItem(compactSidebar);
        });

        hamburger_top.on('click', function() {
            hamburger.toggleClass(active);
            hamburger_top.toggleClass(active);
            $body.toggleClass(compact);
            $body.hasClass(compact) ? window.localStorage.setItem(compactSidebar, true) : window.localStorage.removeItem(compactSidebar);
        });

        // Submenu
        const link = $('.nsofts-has-menu > .nsofts-sidebar-nav__link');
        const menu = $('.nsofts-submenu');

        Array.from(menu).forEach(item => {
            if ($(item).hasClass(show)) {
                $(item).slideDown();
            }
        });

        link.on('click', function() {
            const _this = $(this);
            const next = _this.next();

            if (_this.hasClass(open)) {
                _this.removeClass(open);
                next.slideUp().removeClass(show);

            } else {
                Array.from(link).forEach(item => {
                    if (!$(item).hasClass(active)) {
                        $(item).removeClass(open);
                        $(item).next().slideUp().removeClass(show);
                    }
                });
                _this.addClass(open);
                next.slideDown().addClass(show);
            }
        });
    }


    /**
     * Theme dark 
     */
    const themeOptions = function() {
        const toggler = $('#nsofts_theme_toggler');
        const dark = 'nsofts-theme-dark';
        const mode = 'dark_mode'
        
        // Apply dark mode by default if no preference is set in localStorage
        if (window.localStorage.getItem(mode) === null || window.localStorage.getItem(mode) === 'true') {
            $body.addClass(dark);
            toggler.addClass(active);
        } else {
            $body.removeClass(dark);
            toggler.removeClass(active);
        }
        toggler.on('click', function() {
            const _this = $(this);
            if (_this.hasClass(active)) {
                _this.removeClass(active);
                $body.removeClass(dark);
                window.localStorage.setItem(mode, 'false'); // Set light mode
            } else {
                _this.addClass(active);
                $body.addClass(dark);
                window.localStorage.setItem(mode, 'true'); // Set dark mode
            }
        });
    }


    /**
     * Password toggle
     */
    const password = function() {
        const passwordInput = $('#nsofts_password_input');
        const passwordToggler = $('#nsofts_password_toggler');
        const passwordOpen = $('.nsofts-eye-open');
        const passwordClose = $('.nsofts-eye-close');
        const none = 'd-none';
        
        passwordToggler.on('click', function() {
            const _this = $(this);
            if (_this.hasClass(active)) {
                _this.removeClass(active);
                passwordOpen.removeClass(none);
                passwordClose.addClass(none);
                passwordInput.attr('type', 'password');
                
            } else {
                _this.addClass(active);
                passwordOpen.addClass(none);
                passwordClose.removeClass(none);
                passwordInput.attr('type', 'text');
            }
        });
    }
    
    /**
     * Tooltip
     */
    const initTooltip = function() {
        var tooltips = document.querySelectorAll('[data-bs-toggle="tooltip"]');
        var tooltipList = [...tooltips].map(el => new bootstrap.Tooltip(el));
    }
    
    /**
     * 
     * Initialize notify
     * @param {string} text 
     * @param {string} className 
     */
    const message = function(text, className = 'success') {
        $.notify(text, { className: className });
    }

    /**
     * handle the "Load More" functionality
     *----------------------------------------------------------------------------*/
    const LoadMore = function() {
        var container = document.getElementById('load-more-container');
        var loadMoreBtn = document.getElementById('load-more-btn');
        
        if (container && loadMoreBtn) {
            var cards = container.querySelectorAll('.card-item');
            let visibleCards = 12;

            // Hide extra cards initially
            for (let i = visibleCards; i < cards.length; i++) {
                cards[i].style.display = 'none';
            }

            // Toggle Load More button visibility
            if (cards.length > visibleCards) {
                loadMoreBtn.style.display = '';
            } else {
                loadMoreBtn.style.cssText = 'display: none !important';  // Hide with !important
            }

            // Click event for Load More button
            loadMoreBtn.addEventListener('click', () => {
                for (let i = visibleCards; i < visibleCards + 12 && i < cards.length; i++) {
                    cards[i].style.display = '';
                }
                
                visibleCards += 12;

                // Hide Load More button if all cards are displayed
                if (visibleCards >= cards.length) {
                    loadMoreBtn.style.cssText = 'display: none !important';  // Hide with !important
                }
            });
        }
    }
    
    /**
     * File validation for image upload
     */
    const fileValidation = function() {
        var fileInput = document.getElementById('fileupload');
        if (fileInput) {
            var filePath = fileInput.value;
            var allowedExtensions = /(\.png|\.jpg|\.jpeg|\.PNG|\.JPG|\.JPEG)$/i;
            
            if (!allowedExtensions.exec(filePath)) {
                if (filePath !== '') {
                    fileInput.value = '';
                }
                message("Please upload a file with extension .png, .jpg, .jpeg .PNG, .JPG, .JPEG only!", 'error');
                return false;
            } else {
                if (fileInput.files && fileInput.files[0]) {
                    var reader = new FileReader();
                    reader.onload = function(e) {
                        $("#imagePreview").find("img").attr("src", e.target.result);
                    };
                    reader.readAsDataURL(fileInput.files[0]);
                }
            }
        }
    }

    return {
        init() { 
            loader();
            initScrollbar();
            initSelect();
            initEditor();
            sidebar();
            themeOptions();
            password();
            initTooltip();
            LoadMore();
            
            // Attach fileValidation to the file input
            $('#fileupload').on('change', fileValidation);
        }
    }
    
}();

jQuery(window).on('load', Bundle.init());