package nemosofts.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.nemosofts.BuildConfig;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.util.encrypter.EncryptData;

public class SPHelper {

    private static final String TAG = "SPHelper";
    private final EncryptData encryptData;
    private final SharedPreferences sharedPreferences;
    private final SharedPreferences.Editor editor;

    public static final String TAG_SELECT_XUI = "select_xui";
    public static final String TAG_SELECT_STREAM = "select_stream";
    public static final String TAG_SELECT_PLAYLIST = "select_playlist";
    public static final String TAG_SELECT_DEVICE = "select_device_id";
    public static final String TAG_SELECT_ACTIVATION_CODE = "select_activation_code";
    public static final String TAG_SELECT_SINGLE = "select_single";
    public static final String TAG_IS_LOCAL_STORAGE = "is_local_storage";

    private static final String TAG_ABOUT = "is_about";
    public static final String TAG_ABOUT_EMAIL = "app_email";
    public static final String TAG_ABOUT_AUTHOR = "app_author";
    public static final String TAG_ABOUT_CONTACT = "app_contact";
    public static final String TAG_ABOUT_WEB = "app_website";
    public static final String TAG_ABOUT_DES = "app_description";
    public static final String TAG_ABOUT_DEV = "app_developedBy";

    private static final String TAG_IS_SUPP_RTL = "is_rtl";
    public static final String TAG_IS_SUPP_MAINTENANCE = "is_maintenance";
    public static final String TAG_IS_SUPP_SCREEN = "is_screenshot";
    public static final String TAG_IS_SUPP_APK = "is_apk";
    public static final String TAG_IS_SUPP_VPN = "is_vpn";
    public static final String TAG_IS_SUPP_XUI_DNS = "is_xui_dns";
    public static final String TAG_IS_SUPP_XUI_RADIO = "is_xui_radio";
    public static final String TAG_IS_SUPP_STREAM_DNS = "is_stream_dns";
    public static final String TAG_IS_SUPP_STREAM_RADIO = "is_stream_radio";

    private static final String TAG_FIRST_OPEN = "first_open";
    private static final String SHARED_PREF_AUTOLOGIN = "autologin";
    private static final String TAG_IS_LOGGED = "islogged";
    private static final String TAG_LOGIN_TYPE = "login_type";

    // user info
    private static final String TAG_USER_ID = "user_id";
    private static final String TAG_ANY_NAME = "any_name";
    private static final String TAG_USERNAME = "username";
    private static final String TAG_PASSWORD = "password";
    private static final String TAG_MESSAGE = "message";
    private static final String TAG_STATUS = "status";
    private static final String TAG_EXP_DATE = "exp_date";
    private static final String TAG_ACTIVE_CONNECTIONS = "active_cons";
    private static final String TAG_MAX_CONNECTIONS = "max_connections";

    // server info
    private static final String TAG_IS_XUI = "is_xui";
    private static final String TAG_URL_DATA = "url_data";
    private static final String TAG_PORT = "port";
    private static final String TAG_PORT_HTTPS = "https_port";
    private static final String TAG_SERVER_PROTOCOL = "server_protocol";

    private static final String TAG_ADULT_PASSWORD = "adult_password";

    public SPHelper(@NonNull Context ctx) {
        encryptData = new EncryptData(ctx);
        sharedPreferences = ctx.getSharedPreferences(BuildConfig.APPLICATION_ID + "_" + "app_settings", Context.MODE_PRIVATE);
        editor = sharedPreferences.edit();
    }

    public Boolean getIsFirst() {
        return sharedPreferences.getBoolean(TAG_FIRST_OPEN, true);
    }
    public void setIsFirst(Boolean flag) {
        editor.putBoolean(TAG_FIRST_OPEN, flag);
        editor.apply();
    }

    public boolean isLogged() {
        return sharedPreferences.getBoolean(TAG_IS_LOGGED, false);
    }
    public void setIsLogged(Boolean isLogged) {
        editor.putBoolean(TAG_IS_LOGGED, isLogged);
        editor.apply();
    }

    public Boolean getIsAutoLogin() { return sharedPreferences.getBoolean(SHARED_PREF_AUTOLOGIN, false); }
    public void setIsAutoLogin(Boolean isAutoLogin) {
        editor.putBoolean(SHARED_PREF_AUTOLOGIN, isAutoLogin);
        editor.apply();
    }

    // User Info
    public String getAnyName() {
        return sharedPreferences.getString(TAG_ANY_NAME, "");
    }
    public void setAnyName(String anyName){
        editor.putString(TAG_ANY_NAME, anyName);
        editor.apply();
    }

    public String getLoginType() {
        return sharedPreferences.getString(TAG_LOGIN_TYPE, Callback.TAG_LOGIN);
    }
    public void setLoginType(String type){
        editor.putString(TAG_LOGIN_TYPE, type);
        editor.apply();
    }

    public void setLoginDetails(ItemLoginUser itemLoginUser, ItemLoginServer itemLoginServer) {
        try {
            // user info
            editor.putString(TAG_USERNAME, encryptData.encrypt(itemLoginUser.getUsername()));
            editor.putString(TAG_PASSWORD, encryptData.encrypt(itemLoginUser.getPassword()));
            editor.putString(TAG_MESSAGE, itemLoginUser.getMessage());
            editor.putString(TAG_STATUS, itemLoginUser.getStatus());
            editor.putString(TAG_EXP_DATE, itemLoginUser.getExpDate());
            editor.putString(TAG_ACTIVE_CONNECTIONS, itemLoginUser.getActiveCons());
            editor.putString(TAG_MAX_CONNECTIONS, itemLoginUser.getMaxConnections());

            // server info
            editor.putBoolean(TAG_IS_XUI, itemLoginServer.getXui());
            editor.putString(TAG_URL_DATA, itemLoginServer.getUrl());
            editor.putString(TAG_PORT, itemLoginServer.getPort());
            editor.putString(TAG_PORT_HTTPS, itemLoginServer.getHttpsPort());
            editor.putString(TAG_SERVER_PROTOCOL, itemLoginServer.getServerProtocol());

            editor.apply();
        } catch (Exception e) {
            Log.e(TAG, "Failed to save login details", e);
        }
    }

    // user info
    public String getUserName() {
        return encryptData.decrypt(sharedPreferences.getString(TAG_USERNAME, ""));
    }
    public String getPassword() {
        return encryptData.decrypt(sharedPreferences.getString(TAG_PASSWORD,""));
    }
    public String getCardMessage() {
        return sharedPreferences.getString(TAG_MESSAGE, "");
    }
    public String getIsStatus() {
        return sharedPreferences.getString(TAG_STATUS, "");
    }
    public String getExpDate() {
        return sharedPreferences.getString(TAG_EXP_DATE,"0");
    }
    public String getActiveConnections() {
        return sharedPreferences.getString(TAG_ACTIVE_CONNECTIONS, "");
    }
    public String getMaxConnections() {
        return sharedPreferences.getString(TAG_MAX_CONNECTIONS, "");
    }

    // server info
    public Boolean getIsXuiUser() {
        return sharedPreferences.getBoolean(TAG_IS_XUI, true);
    }

    @NonNull
    private String buildServerUrl(String endpoint) {
        String protocol = sharedPreferences.getString(TAG_SERVER_PROTOCOL, "http");
        String url = sharedPreferences.getString(TAG_URL_DATA, "");
        String port = protocol.equals("http") ?
                sharedPreferences.getString(TAG_PORT, "") :
                sharedPreferences.getString(TAG_PORT_HTTPS, "");
        return protocol + "://" + url + ":" + port + "/" + endpoint;
    }

    public String getServerURL() {
        return buildServerUrl("");
    }

    public String getAPI() {
        return buildServerUrl("player_api.php");
    }

    public String getServerURLSub() {
        String protocol = sharedPreferences.getString(TAG_SERVER_PROTOCOL, "http");
        String url = sharedPreferences.getString(TAG_URL_DATA, "");
        String port = protocol.equals("http") ?
                sharedPreferences.getString(TAG_PORT, "") :
                sharedPreferences.getString(TAG_PORT_HTTPS, "");
        return protocol+"://"+url+ ":"+port;
    }

    public void setCurrentDate(String type){
        Calendar calendar = Calendar.getInstance();
        @SuppressLint("SimpleDateFormat") SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String currentDateTime = simpleDateFormat.format(calendar.getTime());
        editor.putString(type, currentDateTime);
        editor.apply();
    }

    public void setCurrentDateEmpty(String type){
        editor.putString(type, "");
        editor.apply();
    }

    @NonNull
    public String getCurrent(String type) {
        return sharedPreferences.getString(type, "");
    }

    // AboutDetails --------------------------------------------------------------------------------
    public void setAboutDetails(String email, String author, String contact, String website,
                                String description, String developed) {
        editor.putString(TAG_ABOUT_EMAIL, email);
        editor.putString(TAG_ABOUT_AUTHOR, author);
        editor.putString(TAG_ABOUT_CONTACT, contact);
        editor.putString(TAG_ABOUT_WEB, website);
        editor.putString(TAG_ABOUT_DES, description);
        editor.putString(TAG_ABOUT_DEV, developed);
        editor.apply();
    }
    public String getAppEmail() {
        return sharedPreferences.getString(TAG_ABOUT_EMAIL, "");
    }
    public String getAppAuthor() {
        return sharedPreferences.getString(TAG_ABOUT_AUTHOR, "");
    }
    public String getAppContact() {
        return sharedPreferences.getString(TAG_ABOUT_CONTACT, "");
    }
    public String getAppWebsite() {
        return sharedPreferences.getString(TAG_ABOUT_WEB, "");
    }
    public String getAppDescription() {
        return sharedPreferences.getString(TAG_ABOUT_DES, "");
    }
    public String getAppDevelopedBy() {
        return sharedPreferences.getString(TAG_ABOUT_DEV, "");
    }
    public Boolean getIsAboutDetails() {
        return sharedPreferences.getBoolean(TAG_ABOUT, false);
    }
    public void setAboutDetails(Boolean flag){
        editor.putBoolean(TAG_ABOUT, flag);
        editor.apply();
    }

    // isSupported ---------------------------------------------------------------------------------
    public void setIsSupportedApp(Boolean isRtl, Boolean isMaintenance, Boolean isScreenshot, Boolean isApk, Boolean isVpn) {
        editor.putBoolean(TAG_IS_SUPP_RTL, isRtl);
        editor.putBoolean(TAG_IS_SUPP_MAINTENANCE, isMaintenance);
        editor.putBoolean(TAG_IS_SUPP_SCREEN, isScreenshot);
        editor.putBoolean(TAG_IS_SUPP_APK, isApk);
        editor.putBoolean(TAG_IS_SUPP_VPN, isVpn);
        editor.apply();
    }

    public void setIsSupported(Boolean isXuiDns, Boolean isRadio, Boolean isStreamDns,
                               Boolean isStreamRadio, Boolean isLocalStorage) {
        editor.putBoolean(TAG_IS_SUPP_XUI_DNS, isXuiDns);
        editor.putBoolean(TAG_IS_SUPP_XUI_RADIO, isRadio);
        editor.putBoolean(TAG_IS_SUPP_STREAM_DNS, isStreamDns);
        editor.putBoolean(TAG_IS_SUPP_STREAM_RADIO, isStreamRadio);
        editor.putBoolean(TAG_IS_LOCAL_STORAGE, isLocalStorage);
        editor.apply();
    }
    public Boolean getIsRTL() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_RTL, false);
    }
    public Boolean getIsMaintenance() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_MAINTENANCE, false);
    }
    public Boolean getIsScreenshot() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_SCREEN, false);
    }
    public Boolean getIsAPK() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_APK, false);
    }
    public Boolean getIsVPN() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_VPN, false);
    }
    public Boolean getIsXuiDNS(){
        return sharedPreferences.getBoolean(TAG_IS_SUPP_XUI_DNS, false);
    }
    public Boolean getIssStreamDNS(){
        return sharedPreferences.getBoolean(TAG_IS_SUPP_STREAM_DNS, false);
    }
    public Boolean getIsRadio(){
        if (getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            return sharedPreferences.getBoolean(TAG_IS_SUPP_XUI_RADIO, false);
        } else {
            return sharedPreferences.getBoolean(TAG_IS_SUPP_STREAM_RADIO, false);
        }
    }

    public int getIsTheme() {
        return sharedPreferences.getInt("is_theme", 0);
    }
    public void setIsTheme(int flag){
        editor.putInt("is_theme", flag);
        editor.apply();
    }

    public boolean getIsDownload() {
        return sharedPreferences.getBoolean("is_download", false);
    }
    public void setIsDownload(Boolean flag){
        editor.putBoolean("is_download", flag);
        editor.apply();
    }

    public String getAdultPassword() {
        if (sharedPreferences.getString(TAG_ADULT_PASSWORD,"").isEmpty()){
            return "";
        } else {
            return encryptData.decrypt(sharedPreferences.getString(TAG_ADULT_PASSWORD,""));
        }
    }
    public void setAdultPassword(String password){
        editor.putString(TAG_ADULT_PASSWORD, encryptData.encrypt(password));
        editor.apply();
    }

    public int getScreen() {
        return sharedPreferences.getInt("screen_data", 5);
    }
    public void setScreen(int screen) {
        editor.putInt("screen_data", screen);
        editor.apply();
    }

    public Boolean getIsScreen() {
        return sharedPreferences.getBoolean("is_screen", true);
    }
    public void setIsScreen(Boolean isScreen) {
        editor.putBoolean("is_screen", isScreen);
        editor.apply();
    }

    public int getLiveLimit() {
        return sharedPreferences.getInt("live_limit", 20);
    }
    public int getMovieLimit() {
        return sharedPreferences.getInt("movie_limit", 20);
    }
    public void setRecentlyLimit(int stateLive, int stateMovie) {
        editor.putInt("live_limit", stateLive);
        editor.putInt("movie_limit", stateMovie);
        editor.apply();
    }

    public boolean getIsAutoplayEpisode() {
        return sharedPreferences.getBoolean("is_autoplay_epg", false);
    }
    public void setIsAutoplayEpisode(Boolean flag){
        editor.putBoolean("is_autoplay_epg", flag);
        editor.apply();
    }

    public String getAgentName() {
        return sharedPreferences.getString("agent_name","");
    }
    public void setAgentName(String agent){
        editor.putString("agent_name", agent);
        editor.apply();
    }

    public void removeSignOut() {
        editor.putString(TAG_LOGIN_TYPE, Callback.TAG_LOGIN);
        editor.putBoolean(SHARED_PREF_AUTOLOGIN, false);
        editor.putBoolean(TAG_IS_LOGGED, false);

        editor.putString(Callback.TAG_TV, "");
        editor.putString(Callback.TAG_MOVIE, "");
        editor.putString(Callback.TAG_SERIES, "");

        // user info
        editor.putString(TAG_USERNAME, encryptData.encrypt(""));
        editor.putString(TAG_PASSWORD, encryptData.encrypt(""));
        editor.putString(TAG_MESSAGE, "");
        editor.putString(TAG_STATUS, "");
        editor.putString(TAG_EXP_DATE, "");
        editor.putString(TAG_ACTIVE_CONNECTIONS, "");
        editor.putString(TAG_MAX_CONNECTIONS, "");

        // server info
        editor.putBoolean(TAG_IS_XUI, true);
        editor.putString(TAG_URL_DATA, "");
        editor.putString(TAG_PORT, "");
        editor.putString(TAG_PORT_HTTPS, "");
        editor.putString(TAG_SERVER_PROTOCOL, "");

        editor.putString(TAG_USER_ID, "0");

        editor.apply();

        if (!Callback.getArrayListNotify().isEmpty()){
            Callback.getArrayListNotify().clear();
        }
        if (!Callback.getArrayListRadio().isEmpty()){
            Callback.getArrayListRadio().clear();
        }
        if (!Callback.getArrayListEpisodes().isEmpty()){
            Callback.getArrayListEpisodes().clear();
        }
        if (!Callback.getArrayListLive().isEmpty()){
            Callback.getArrayListLive().clear();
        }
    }

    public Boolean getIsSelect(String type) {
        return sharedPreferences.getBoolean(type, true);
    }
    public void setIsSelect(Boolean xui, Boolean stream, Boolean playlist, Boolean deviceID,
                            Boolean single, Boolean activation) {
        editor.putBoolean(TAG_SELECT_XUI, xui);
        editor.putBoolean(TAG_SELECT_STREAM, stream);
        editor.putBoolean(TAG_SELECT_PLAYLIST, playlist);
        editor.putBoolean(TAG_SELECT_DEVICE, deviceID);
        editor.putBoolean(TAG_SELECT_SINGLE, single);
        editor.putBoolean(TAG_SELECT_ACTIVATION_CODE, activation);
        editor.apply();
    }

    public int getLiveFormat() {
        return sharedPreferences.getInt("live_format", 0);
    }
    public void setLiveFormat(int state) {
        editor.putInt("live_format", state);
        editor.apply();
    }

    public int getAutoUpdate() {
        return sharedPreferences.getInt("add_data", 5);
    }
    public void setAutoUpdate(int state) {
        editor.putInt("add_data", state);
        editor.apply();
    }

    public String getTmdbKEY() {
        return sharedPreferences.getString("tmdb_key","");
    }
    public void setTmdbKEY(String apiKey){
        editor.putString("tmdb_key", apiKey);
        editor.apply();
    }

    public Boolean getIsSplashAudio() {
        return sharedPreferences.getBoolean("splash_audio", true);
    }
    public void setIsAudio(boolean isAudio) {
        editor.putBoolean("splash_audio", isAudio);
        editor.apply();
    }

    // Shimmer -------------------------------------------------------------------------------------
    public void setIsShimmering(boolean isHome, boolean isDetails) {
        editor.putBoolean("shimmer_home", isHome);
        editor.putBoolean("shimmer_details", isDetails);
        editor.apply();
    }

    public Boolean getIsShimmeringHome() {
        return sharedPreferences.getBoolean("shimmer_home", true);
    }
    public Boolean getIsShimmeringDetails() {
        return sharedPreferences.getBoolean("shimmer_details", true);
    }

    // UI ------------------------------------------------------------------------------------------
    public void setIsUI(boolean isName, boolean isDownload, boolean isCast) {
        editor.putBoolean("ui_card_title", isName);
        editor.putBoolean("ui_download", isDownload);
        editor.putBoolean("ui_cast", isCast);
        editor.apply();
    }

    public boolean getUICardTitle() {
        return sharedPreferences.getBoolean("ui_card_title", true);
    }
    public boolean getIsCast() {
        return sharedPreferences.getBoolean("ui_cast", true);
    }
    public boolean getIsDownloadUser() {
        return sharedPreferences.getBoolean("ui_download", true);
    }

    public void setIsPlayerUI(boolean isSubtitle, boolean isVR) {
        editor.putBoolean("ui_player_subtitle", isSubtitle);
        editor.putBoolean("ui_player_vr", isVR);
        editor.apply();
    }
    public boolean getIsSubtitle() {
        return sharedPreferences.getBoolean("ui_player_subtitle", true);
    }
    public boolean getIsVR() {
        return sharedPreferences.getBoolean("ui_player_vr", true);
    }


    public int getIsThemeEPG() {
        return sharedPreferences.getInt("is_theme_epg", 2);
    }
    public void setIsThemeEPG(int flag) {
        editor.putInt("is_theme_epg", flag);
        editor.apply();
    }

    public Boolean isSnowFall() { return sharedPreferences.getBoolean("switch_snow_fall", true); }
    public void setSnowFall(Boolean state) {
        editor.putBoolean("switch_snow_fall", state);
        editor.apply();
    }

    public boolean getIsUpdateLive() {
        return sharedPreferences.getBoolean("auto_update_live", true);
    }

    public boolean getIsUpdateMovies() {
        return sharedPreferences.getBoolean("auto_update_movies", true);
    }

    public boolean getIsUpdateSeries() {
        return sharedPreferences.getBoolean("auto_update_series", true);
    }

    public void setIsUpdateLive(boolean state) {
        editor.putBoolean("auto_update_live", state);
        editor.apply();
    }

    public void setIsUpdateMovies(boolean state) {
        editor.putBoolean("auto_update_movies", state);
        editor.apply();
    }

    public void setIsUpdateSeries(boolean state) {
        editor.putBoolean("auto_update_series", state);
        editor.apply();
    }

    public boolean getIs12Format() {
        return sharedPreferences.getBoolean("time_format", true);
    }
    public void setIs12Format(boolean state) {
        editor.putBoolean("time_format", state);
        editor.apply();
    }

    public Boolean isHardwareDecoding() { return sharedPreferences.getBoolean("hardware_decoding", true); }
    public void setHardwareDecoding(Boolean isHardwareDecoding) {
        editor.putBoolean("hardware_decoding", isHardwareDecoding);
        editor.apply();
    }

    public String getUserId() {
        return sharedPreferences.getString(TAG_USER_ID, "0");
    }
    public void setUserId(String newUserId) {
        editor.putString(TAG_USER_ID, newUserId);
        editor.apply();
    }

    public int getBlurRadius() {
        try {
            return sharedPreferences.getInt("blur_radius", 80);
        } catch (Exception e) {
           return 80;
        }
    }
    public void setBlurRadius(int blurRadius) {
        editor.putInt("blur_radius", blurRadius);
        editor.apply();
    }

    public Boolean isOVEN() { return sharedPreferences.getBoolean("is_vpn", false); }
    public void setOVEN(Boolean isVpn) {
        editor.putBoolean("is_vpn", isVpn);
        editor.apply();
    }
}