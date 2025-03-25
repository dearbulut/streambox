<?php $page_title="Settings Advertisement";
    include("includes/header.php");
    require("includes/lb_helper.php");
    require("language/language.php");
    
    if(!isset($_SESSION['admin_type'])){
        if($_SESSION['admin_type'] == 0){
            session_destroy();
            header( "Location:index.php");
            exit;
        }
    }
    
    $qry="SELECT * FROM tbl_advertisement where id='1'";
    $result=mysqli_query($mysqli,$qry);
    $settings_data=mysqli_fetch_assoc($result);

    if(isset($_POST['submit'])){
        
        $ad_network  =  cleanInput($_POST['ad_network']);
        
        if($ad_network == 'admob'){
            
            $data = array(
                'ad_status'  =>  ($_POST['ad_status']) ? 'true' : 'false',
                'ad_network'  =>  cleanInput($_POST['ad_network']),
                
                'admob_publisher_id'  =>  $_POST['admob_publisher_id'],
                'admob_banner_unit_id'  =>  $_POST['admob_banner_unit_id'],
                'admob_interstitial_unit_id'  =>  $_POST['admob_interstitial_unit_id'],
                'admob_reward_ad_unit_id'  =>  $_POST['admob_reward_ad_unit_id'],
                
                'interstital_ad_click'  =>  $_POST['interstital_ad_click'],
                'reward_minutes'  =>  $_POST['reward_minutes'],
                
                'banner_movie'  =>  ($_POST['banner_movie']) ? 'true' : 'false',
                'banner_series'  =>  ($_POST['banner_series']) ? 'true' : 'false',
                'banner_epg'  =>  ($_POST['banner_epg']) ? 'true' : 'false',
                
                'interstitial_post_list'  =>  ($_POST['interstitial_post_list']) ? 'true' : 'false',
                
                'reward_ad_on_movie'  =>  ($_POST['reward_ad_on_movie']) ? 'true' : 'false',
                'reward_ad_on_episodes'  =>  ($_POST['reward_ad_on_episodes']) ? 'true' : 'false',
                'reward_ad_on_live'  =>  ($_POST['reward_ad_on_live']) ? 'true' : 'false',
                'reward_ad_on_single'  =>  ($_POST['reward_ad_on_single']) ? 'true' : 'false',
                'reward_ad_on_local'  =>  ($_POST['reward_ad_on_local']) ? 'true' : 'false'
            );
        }
        
        $settings_edit=Update('tbl_advertisement', $data, "WHERE id = '1'");
        $_SESSION['msg']="11";
        $_SESSION['class'] = "success";
        header( "Location:settings_ads.php");
        exit;
    }
?>
<style>
    .col-form-label {
        color: var(--ns-gray-300);
    }
</style>

<!-- Start: main -->
<main id="nsofts_main">
    <div class="nsofts-container">
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb align-items-center">
                <li class="breadcrumb-item d-inline-flex"><a href="dashboard.php"><i class="ri-home-4-fill"></i></a></li>
                <li class="breadcrumb-item d-inline-flex active" aria-current="page"><?php echo (isset($page_title)) ? $page_title : "" ?></li>
            </ol>
        </nav>
        
        <form action="" name="advertisement" method="POST" enctype="multipart/form-data">
        <div class="row clearfix">
            <div class="col-lg-8 col-md-8 col-sm-12 col-xs-12">
                <div class="card">
                    <div class="card-top d-md-inline-flex align-items-center justify-content-between py-3 px-4">
                        <h5  class="d-inline-flex align-items-center fw-semibold m-0">MANAGE ADS</h5>
                        <div class="d-flex mt-2 mt-md-0">
                            <button type="submit" name="submit" class="btn btn-primary d-inline-flex align-items-center justify-content-center" style="min-width: 120px;">
                                <i class="ri-refresh-line"></i>
                                <span class="ps-1 text-nowrap d-none d-sm-block">UPDATE</span>
                            </button>
                        </div>
                    </div>
                    <div class="card-body p-0">
                        <div class="p-4">
                            <div class="mb-3 row">
                                <label for="" class="col-sm-2 col-form-label">Ad Status</label>
                                <div class="col-sm-10">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="ad_status" name="ad_status" value="true" class="nsofts-switch__label" <?php if($settings_data['ad_status']=='true'){ echo 'checked'; }?>/>
                                        <label for="ad_status" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                            </div>
                            <div class="mb-3">
                                <label for="" class="col-form-label">Primary Ad Network</label>
                                <select name="ad_network" id="ad_network" class="nsofts-select " required>
                                    <option value="admob" <?php if ($settings_data['ad_network'] == 'admob') { ?>selected<?php } ?>>AdMob</option>
                                </select>
                            </div>
                            <div class="admob_ads" style="display: none">
                                <div class="mb-2">
                                    <label for="" class="col-form-label">AdMob Publisher ID</label>
                                    <input type="text" name="admob_publisher_id" id="admob_publisher_id" value="<?php echo $settings_data['admob_publisher_id']; ?>" class="form-control">
                                </div>
                                <div class="mb-2">
                                    <label for="" class="col-form-label">AdMob Banner Ad Unit ID</label>
                                    <input type="text" name="admob_banner_unit_id" id="admob_banner_unit_id" value="<?php echo $settings_data['admob_banner_unit_id']; ?>" class="form-control">
                                </div>
                                <div class="mb-2">
                                    <label for="" class="col-form-label">AdMob Interstitial Ad Unit ID</label>
                                    <input type="text" name="admob_interstitial_unit_id" id="admob_interstitial_unit_id" value="<?php echo $settings_data['admob_interstitial_unit_id']; ?>" class="form-control">
                                </div>
                                <div class="mb-2">
                                    <label for="" class="col-form-label">AdMob Reward Ad Unit ID</label>
                                    <input type="text" name="admob_reward_ad_unit_id" id="admob_reward_ad_unit_id" value="<?php echo $settings_data['admob_reward_ad_unit_id']; ?>" class="form-control">
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="card mt-4">
                    <div class="card-body p-0">
                        <div class="card-top p-3">
                            <h5 class="fw-semibold ps-2 lh-1 m-0">GLOBAL CONFIGURATION</h5>
                        </div>
                        
                        <div class="p-4 pt-1">
                            <div class="mb-1">
                                <label for="" class="col-form-label">Interstitial Ad Interval</label>
                                <input type="text" name="interstital_ad_click" id="interstital_ad_click" value="<?php echo $settings_data['interstital_ad_click']; ?>" class="form-control ads_click">
                            </div>
                            <div class="mb-2">
                                <label for="" class="col-form-label">The reward ad will show the ad * minutes after playing</label>
                                <select name="reward_minutes" id="reward_minutes" class="nsofts-select " required>
                                    <option value="2" <?php if ($settings_data['reward_minutes'] == '2') { ?>selected<?php } ?>>2 Minutes</option>
                                    <option value="3" <?php if ($settings_data['reward_minutes'] == '3') { ?>selected<?php } ?>>3 Minutes</option>
                                    <option value="5" <?php if ($settings_data['reward_minutes'] == '5') { ?>selected<?php } ?>>5 Minutes</option>
                                    <option value="8" <?php if ($settings_data['reward_minutes'] == '8') { ?>selected<?php } ?>>8 Minutes</option>
                                    <option value="10" <?php if ($settings_data['reward_minutes'] == '10') { ?>selected<?php } ?>>10 Minutes</option>
                                    <option value="15" <?php if ($settings_data['reward_minutes'] == '15') { ?>selected<?php } ?>>15 Minutes</option>
                                    <option value="25" <?php if ($settings_data['reward_minutes'] == '25') { ?>selected<?php } ?>>25 Minutes</option>
                                    <option value="30" <?php if ($settings_data['reward_minutes'] == '30') { ?>selected<?php } ?>>30 Minutes</option>
                                </select>
                            </div>
                        </div>
                    </div>
                </div>

            </div>
            <div class="col-lg-4 col-md-4 col-sm-12 col-xs-12">
                <div class="card">
                    <div class="card-body p-0">
                        
                        <div class="card-top p-3">
                            <h5 class="fw-semibold ps-2 lh-1 m-0">ADS PLACEMENT</h5>
                        </div>
                        
                        <div class="p-4">
                            <label class="col-form-label"> Enable or Disable Certain Ads Format Separately</label>
                            
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="banner_movie" name="banner_movie" value="true" class="cbx hidden" <?php if($settings_data['banner_movie']=='true'){ echo 'checked'; }?>/>
                                        <label for="banner_movie" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Banner Ad on Movie Details</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="banner_series" name="banner_series" value="true" class="cbx hidden" <?php if($settings_data['banner_series']=='true'){ echo 'checked'; }?>/>
                                        <label for="banner_series" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Banner Ad on Series Details</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="banner_epg" name="banner_epg" value="true" class="cbx hidden" <?php if($settings_data['banner_epg']=='true'){ echo 'checked'; }?>/>
                                        <label for="banner_epg" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Banner Ad on EPG</label>
                            </div>

                            <div class="mb-4"></div>
                            
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="interstitial_post_list" name="interstitial_post_list" value="true" class="cbx hidden" <?php if($settings_data['interstitial_post_list']=='true'){ echo 'checked'; }?>/>
                                        <label for="interstitial_post_list" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Interstitial Ad</label>
                            </div>
                            
                            <div class="mb-4"></div>
                            
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="reward_ad_on_movie" name="reward_ad_on_movie" value="true" class="cbx hidden" <?php if($settings_data['reward_ad_on_movie']=='true'){ echo 'checked'; }?>/>
                                        <label for="reward_ad_on_movie" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Reward Ad on Movie Player</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="reward_ad_on_episodes" name="reward_ad_on_episodes" value="true" class="cbx hidden" <?php if($settings_data['reward_ad_on_episodes']=='true'){ echo 'checked'; }?>/>
                                        <label for="reward_ad_on_episodes" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Reward Ad on Episodes Player</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="reward_ad_on_live" name="reward_ad_on_live" value="true" class="cbx hidden" <?php if($settings_data['reward_ad_on_live']=='true'){ echo 'checked'; }?>/>
                                        <label for="reward_ad_on_live" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Reward Ad on Live TV Player</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="reward_ad_on_single" name="reward_ad_on_single" value="true" class="cbx hidden" <?php if($settings_data['reward_ad_on_single']=='true'){ echo 'checked'; }?>/>
                                        <label for="reward_ad_on_single" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Reward Ad on Single URL Player</label>
                            </div>
                            <div class="row">
                                <div class="col-sm-3 mt-2">
                                    <div class="nsofts-switch d-flex align-items-center">
                                        <input type="checkbox" id="reward_ad_on_local" name="reward_ad_on_local" value="true" class="cbx hidden" <?php if($settings_data['reward_ad_on_local']=='true'){ echo 'checked'; }?>/>
                                        <label for="reward_ad_on_local" class="nsofts-switch__label"></label>
                                    </div>
                                </div>
                                <label for="" class="col-sm col-form-label">Reward Ad on Local Player</label>
                            </div>
                            
                        </div>
                    </div>
                </div>
            </div>
        </div>
        </form>
    </div>
</main>
<!-- End: main -->
    
<?php include("includes/footer.php");?>
<script type="text/javascript">
$(document).ready(function(e) {
    var adType = $("select[name='ad_network']").val();
    if (adType === 'admob') {
        $(".admob_ads").show();
    } else {
        $(".admob_ads").hide();
    }
    
    if (adType === 'startapp') {
        $(".startapp_ads").show();
    } else {
        $(".startapp_ads").hide();
    }
    
    if (adType === 'unity') {
        $(".unity_ads").show();
    } else {
        $(".unity_ads").hide();
    }
    
    if (adType === 'applovin') {
        $(".applovin_ads").show();
    } else {
        $(".applovin_ads").hide();
    }
    
    if (adType === 'ironsource') {
        $(".ironsource_ads").show();
    } else {
        $(".ironsource_ads").hide();
    }
    
    if (adType === 'meta') {
        $(".meta_ads").show();
    } else {
        $(".meta_ads").hide();
    }
    
    if (adType === 'yandex') {
        $(".yandex_ads").show();
    } else {
        $(".yandex_ads").hide();
    }
    
    if (adType === 'wortise') {
        $(".wortise_ads").show();
    } else {
        $(".wortise_ads").hide();
    }
    
});

$("select[name='ad_network']").change(function(e) {
    if ($(this).val() === 'admob') {
        $(".admob_ads").show();
    } else {
        $(".admob_ads").hide();
    }
    
    if ($(this).val() === 'startapp') {
        $(".startapp_ads").show();
    } else {
        $(".startapp_ads").hide();
    }
    
    if ($(this).val() === 'unity') {
        $(".unity_ads").show();
    } else {
        $(".unity_ads").hide();
    }
    
    if ($(this).val() === 'applovin') {
        $(".applovin_ads").show();
    } else {
        $(".applovin_ads").hide();
    }
    
    if ($(this).val() === 'ironsource') {
        $(".ironsource_ads").show();
    } else {
        $(".ironsource_ads").hide();
    }
    
    if ($(this).val() === 'meta') {
        $(".meta_ads").show();
    } else {
        $(".meta_ads").hide();
    }
    
    if ($(this).val() === 'yandex') {
        $(".yandex_ads").show();
    } else {
        $(".yandex_ads").hide();
    }
    
    if ($(this).val() === 'wortise') {
        $(".wortise_ads").show();
    } else {
        $(".wortise_ads").hide();
    }
});

$("input[name='native_position']").blur(function(e) {
    if ($(this).val() == '' || parseInt($(this).val()) <= 0) {
      $(this).val('1');
    }
});

$("#interstital_ad_click").blur(function(e) {
if ($(this).val() == '')
  $(this).val("1");
});

$("input[name='reward_credit']").blur(function(e) {
    if ($(this).val() == '' || parseInt($(this).val()) <= 0) {
      $(this).val('1');
    }
});

</script>
