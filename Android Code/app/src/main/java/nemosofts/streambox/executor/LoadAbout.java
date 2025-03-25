package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import androidx.nemosofts.Envato;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.callback.Method;
import nemosofts.streambox.interfaces.AboutListener;
import nemosofts.streambox.item.ItemDns;
import nemosofts.streambox.item.ItemNotification;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadAbout extends AsyncTaskExecutor<String, String, String> {

    private final DBHelper dbHelper;
    private final Envato envato;
    private final Helper helper;
    private final SPHelper spHelper;
    private final AboutListener aboutListener;
    private String verifyStatus = "0";
    private String message = "";

    private static final String TAG_DNS_TITLE = "dns_title";
    private static final String TAG_DNS_URL = "dns_base";

    public LoadAbout(Context context, AboutListener aboutListener) {
        this.aboutListener = aboutListener;
        helper = new Helper(context);
        spHelper = new SPHelper(context);
        envato = new Envato(context);
        dbHelper = new DBHelper(context);
    }

    @Override
    protected void onPreExecute() {
        aboutListener.onStart();
        if (!Callback.getArrayListNotify().isEmpty()){
            Callback.getArrayListNotify().clear();
        }
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            String json = ApplicationUtil.responsePost(Callback.API_URL,
                    helper.getAPIRequestNSofts(Method.METHOD_APP_DETAILS, "",
                            "", "","")
            );
            JSONObject mainJson = new JSONObject(json);

            try {
                JSONObject jsonObject = mainJson.getJSONObject(Callback.TAG_ROOT);

                if (jsonObject.has("details")) {
                    JSONArray jsonArrayDetails = jsonObject.getJSONArray("details");

                    for (int i = 0; i < jsonArrayDetails.length(); i++) {
                        JSONObject c = jsonArrayDetails.getJSONObject(i);

                        // App Details
                        String email = c.getString("app_email");
                        String author = c.getString("app_author");
                        String contact = c.getString("app_contact");
                        String website = c.getString("app_website");
                        String description = c.getString("app_description");
                        String developed = c.getString("app_developed_by");
                        spHelper.setAboutDetails(email, author, contact, website, description, developed);

                        // Envato ------------------------------------------------------------------
                        String apikey = c.getString("envato_api_key");
                        if (!apikey.isEmpty()){
                            envato.setEnvatoKEY(apikey);
                        } else {
                            spHelper.setAboutDetails(false);
                        }

                        // isSupported -------------------------------------------------------------
                        Boolean isRtl = Boolean.parseBoolean(c.getString("is_rtl"));
                        Boolean isMaintenance = Boolean.parseBoolean(c.getString("is_maintenance"));
                        Boolean isScreenshot = Boolean.parseBoolean(c.getString("is_screenshot"));
                        Boolean isApk = Boolean.parseBoolean(c.getString("is_apk"));
                        Boolean isVpn = Boolean.parseBoolean(c.getString("is_vpn"));
                        Boolean isXuiDns = Boolean.parseBoolean(c.getString("is_xui_dns"));
                        Boolean isRadio = Boolean.parseBoolean(c.getString("is_xui_radio"));
                        Boolean isStreamDns = Boolean.parseBoolean(c.getString("is_stream_dns"));
                        Boolean isStreamRadio = Boolean.parseBoolean(c.getString("is_stream_radio"));
                        Boolean isLocalStorage = Boolean.parseBoolean(c.getString("is_local_storage"));
                        spHelper.setIsSupportedApp(isRtl, isMaintenance, isScreenshot, isApk, isVpn);
                        spHelper.setIsSupported(isXuiDns, isRadio, isStreamDns, isStreamRadio, isLocalStorage);

                        // isSelect ----------------------------------------------------------------
                        Boolean isXui = Boolean.parseBoolean(c.getString("is_select_xui"));
                        Boolean isStream = Boolean.parseBoolean(c.getString("is_select_stream"));
                        Boolean isPlaylist = Boolean.parseBoolean(c.getString("is_select_playlist"));
                        Boolean isDeviceID = Boolean.parseBoolean(c.getString("is_select_device_id"));
                        Boolean isSingle = Boolean.parseBoolean(c.getString("is_select_single"));
                        Boolean isActivation = Boolean.parseBoolean(c.getString("is_select_activation_code"));
                        spHelper.setIsSelect(isXui, isStream, isPlaylist, isDeviceID, isSingle, isActivation);

                        // AppUpdate ---------------------------------------------------------------
                        Boolean isAppUpdate = Boolean.parseBoolean(c.getString("app_update_status"));
                        Callback.setIsAppUpdate(isAppUpdate);
                        if(!c.getString("app_new_version").isEmpty()) {
                            int appNew = Integer.parseInt(c.getString("app_new_version"));
                            Callback.setAppNewVersion(appNew);
                        }
                        Callback.setAppUpdateDesc(c.getString("app_update_desc"));
                        Callback.setAppRedirectUrl(c.getString("app_redirect_url"));

                        spHelper.setIsTheme(Integer.parseInt(c.getString("is_theme")));
                        spHelper.setIsThemeEPG(Integer.parseInt(c.getString("is_epg")));
                        spHelper.setIsDownload(Boolean.parseBoolean(c.getString("is_download")));
                        spHelper.setTmdbKEY(c.getString("tmdb_key"));
                    }
                }

                // Ads Network ---------------------------------------------------------------------
                if (jsonObject.has("ads_details")) {
                    JSONArray jsonArrayDetails = jsonObject.getJSONArray("ads_details");
                    for (int i = 0; i < jsonArrayDetails.length(); i++) {
                        JSONObject c = jsonArrayDetails.getJSONObject(i);

                        Callback.setIsAdsStatus(Boolean.parseBoolean(c.getString("ad_status")));

                        // PRIMARY ADS -------------------------------------------------------------
                        Callback.setAdNetwork(c.getString("ad_network"));
                        Callback.setAdmobBannerAdID(c.getString("banner_ad_id"));
                        Callback.setAdmobInterstitialAdID(c.getString("interstital_ad_id"));
                        Callback.setAdmobRewardAdID(c.getString("reward_ad_id"));

                        // ADS PLACEMENT -----------------------------------------------------------
                        if (c.has("banner_movie")){
                            Callback.setBannerMovie(Boolean.parseBoolean(c.getString("banner_movie")));
                            Callback.setBannerSeries(Boolean.parseBoolean(c.getString("banner_series")));
                            Callback.setBannerEpg(Boolean.parseBoolean(c.getString("banner_epg")));
                            Callback.setIsInterAd(Boolean.parseBoolean(c.getString("interstital_ad")));

                            Callback.setRewardAdMovie(Boolean.parseBoolean(c.getString("reward_ad_on_movie")));
                            Callback.setRewardAdEpisodes(Boolean.parseBoolean(c.getString("reward_ad_on_episodes")));
                            Callback.setRewardAdLive(Boolean.parseBoolean(c.getString("reward_ad_on_live")));
                            Callback.setRewardAdSingle(Boolean.parseBoolean(c.getString("reward_ad_on_single")));
                            Callback.setRewardAdLocal(Boolean.parseBoolean(c.getString("reward_ad_on_local")));
                        }

                        // GLOBAL CONFIGURATION ----------------------------------------------------
                        if(!c.getString("interstital_ad_click").isEmpty()) {
                            Callback.setInterstitialAdShow(Integer.parseInt(c.getString("interstital_ad_click")));
                        }
                        if(!c.getString("reward_minutes").isEmpty()) {
                            Callback.setRewardMinutes(Integer.parseInt(c.getString("reward_minutes")));
                        }
                    }
                }

                if (jsonObject.has("xui_dns")) {
                    dbHelper.removeAllDNS(DBHelper.TABLE_DNS_XUI);
                    JSONArray jsonArrayXui = jsonObject.getJSONArray("xui_dns");
                    if (jsonArrayXui.length() > 0) {
                        for (int i = 0; i < jsonArrayXui.length(); i++) {
                            JSONObject jsonobject = jsonArrayXui.getJSONObject(i);

                            String title = jsonobject.getString(TAG_DNS_TITLE);
                            String base = jsonobject.getString(TAG_DNS_URL);

                            ItemDns objItem = new ItemDns(title, base);
                            dbHelper.addToDNS(DBHelper.TABLE_DNS_XUI, objItem);
                        }
                    }
                }

                if (jsonObject.has("stream_dns")) {
                    dbHelper.removeAllDNS(DBHelper.TABLE_DNS_STREAM);
                    JSONArray jsonArrayXui = jsonObject.getJSONArray("stream_dns");
                    if (jsonArrayXui.length() > 0) {
                        for (int i = 0; i < jsonArrayXui.length(); i++) {
                            JSONObject jsonobject = jsonArrayXui.getJSONObject(i);

                            String title = jsonobject.getString(TAG_DNS_TITLE);
                            String base = jsonobject.getString(TAG_DNS_URL);

                            ItemDns objItem = new ItemDns(title, base);
                            dbHelper.addToDNS(DBHelper.TABLE_DNS_STREAM, objItem);
                        }
                    }
                }

                parseBlockedDns(jsonObject);
                parsePopupAds(jsonObject);
                parseNotificationData(jsonObject);

                return "1";
            } catch (Exception e) {
                JSONArray jsonArray = mainJson.getJSONArray(Callback.TAG_ROOT);
                JSONObject jsonObject = jsonArray.getJSONObject(0);
                verifyStatus = jsonObject.getString(Callback.TAG_SUCCESS);
                message = jsonObject.getString(Callback.TAG_MSG);
                Log.e("LoadAbout", "Error fetching about", e);
                return "0";
            }
        } catch (Exception ee) {
            Log.e("LoadAbout", "Error fetching about", ee);
            return "0";
        }
    }

    private void parseBlockedDns(JSONObject jsonObject) throws JSONException {
        if (jsonObject == null){
            return;
        }
        if (jsonObject.has("xui_dns_block")) {
            JSONArray jsonArrayXui = jsonObject.getJSONArray("xui_dns_block");
            if (jsonArrayXui.length() > 0) {
                for (int i = 0; i < jsonArrayXui.length(); i++) {
                    JSONObject jsonobject = jsonArrayXui.getJSONObject(i);

                    String base = jsonobject.getString(TAG_DNS_URL);

                    ItemDns objItem = new ItemDns("", base);
                    Callback.getArrayBlacklist().add(objItem);
                }
            }
        }
    }

    private void parseNotificationData(JSONObject jsonObject) throws JSONException {
        if (jsonObject == null){
            return;
        }
        if (jsonObject.has("notification_data")) {
            JSONArray jsonArrayNotify = jsonObject.getJSONArray("notification_data");
            for (int i = 0; i < jsonArrayNotify.length(); i++) {
                JSONObject notificationObject = jsonArrayNotify.getJSONObject(i);
                ItemNotification notification = new ItemNotification(
                        notificationObject.getString("id"),
                        notificationObject.getString("notification_title"),
                        notificationObject.getString("notification_msg"),
                        notificationObject.getString("notification_description"),
                        notificationObject.getString("notification_on")
                );
                Callback.getArrayListNotify().add(notification);
            }
        }
    }

    private void parsePopupAds(JSONObject jsonObject) throws JSONException {
        if (jsonObject == null){
            return;
        }
        if (jsonObject.has("popup_ads")) {
            JSONArray jsonArrayAds = jsonObject.getJSONArray("popup_ads");
            for (int i = 0; i < jsonArrayAds.length(); i++) {
                JSONObject adObject = jsonArrayAds.getJSONObject(i);
                Callback.setAdsTitle(adObject.getString("ads_title"));
                Callback.setAdsImage(adObject.getString("ads_image"));
                Callback.setAdsRedirectType(adObject.getString("ads_redirect_type"));
                Callback.setAdsRedirectURL(adObject.getString("ads_redirect_url"));
            }
        }
    }
    @Override
    protected void onPostExecute(String s) {
        aboutListener.onEnd(s, verifyStatus, message);
    }
}