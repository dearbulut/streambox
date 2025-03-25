package nemosofts.streambox.callback;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.item.ItemDns;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.item.ItemNotification;
import nemosofts.streambox.item.ItemPoster;

public class Callback implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    // API URL -------------------------------------------------------------------------------------
    public static final String API_URL = BuildConfig.BASE_URL+"api.php";

    // TAG API -------------------------------------------------------------------------------------
    public static final String TAG_ROOT = BuildConfig.API_NAME;
    public static final String TAG_SUCCESS = "success";
    public static final String TAG_MSG = "MSG";

    public static final String TAG_TV = "date_tv";
    public static final String TAG_MOVIE = "date_movies";
    public static final String TAG_SERIES = "date_series";

    public static final String TAG_LOGIN = "none";
    public static final String TAG_LOGIN_ONE_UI = "one_ui";
    public static final String TAG_LOGIN_SINGLE_STREAM = "single_stream";
    public static final String TAG_LOGIN_PLAYLIST = "playlist";
    public static final String TAG_LOGIN_STREAM = "stream";
    public static final String TAG_LOGIN_VIDEOS = "videos";

    // Update and dialog ---------------------------------------------------------------------------
    public static final String DIALOG_TYPE_UPDATE = "upgrade";
    public static final String DIALOG_TYPE_MAINTENANCE = "maintenance";
    public static final String DIALOG_TYPE_DEVELOPER = "developer";
    public static final String DIALOG_TYPE_VPN = "vpn";

    private static Boolean isAppUpdate = false;
    public static Boolean getIsAppUpdate() {
        return isAppUpdate;
    }
    public static void setIsAppUpdate(Boolean appUpdate) {
        isAppUpdate = appUpdate;
    }

    private static int appNewVersion = 1;
    public static int getAppNewVersion() {
        return appNewVersion;
    }
    public static void setAppNewVersion(int newVersion) {
        appNewVersion = newVersion;
    }

    private static String appUpdateDesc = "";
    public static String getAppUpdateDesc() {
        return appUpdateDesc;
    }
    public static void setAppUpdateDesc(String updateDesc) {
        appUpdateDesc = updateDesc;
    }

    private static String appRedirectUrl = "";
    public static String getAppRedirectUrl() {
        return appRedirectUrl;
    }
    public static void setAppRedirectUrl(String redirectUrl) {
        appRedirectUrl = redirectUrl;
    }

    // Success -------------------------------------------------------------------------------------
    private static String successLive = "0";
    public static String getSuccessLive() {
        return successLive;
    }
    public static void setSuccessLive(String value) {
        successLive = value;
    }

    private static String successSeries = "0";
    public static String getSuccessSeries() {
        return successSeries;
    }
    public static void setSuccessSeries(String value) {
        successSeries = value;
    }

    private static String successMovies = "0";
    public static String getSuccessMovies() {
        return successMovies;
    }
    public static void setSuccessMovies(String value) {
        successMovies = value;
    }

    // Update Recreate Recreate --------------------------------------------------------------------
    private static Boolean isDataUpdate = false;
    public static Boolean getIsDataUpdate() {
        return isDataUpdate;
    }
    public static void setIsDataUpdate(Boolean dataUpdate) {
        isDataUpdate = dataUpdate;
    }

    private static Boolean isRecreate = false;
    public static Boolean getIsRecreate() {
        return isRecreate;
    }
    public static void setIsRecreate(Boolean recreate) {
        isRecreate = recreate;
    }

    private static Boolean isRecreateUi = false;
    public static Boolean getIsRecreateUi() {
        return isRecreateUi;
    }
    public static void setIsRecreateUi(Boolean recreateUi) {
        isRecreateUi = recreateUi;
    }

    // Custom Advertising --------------------------------------------------------------------------
    private static String adsTitle = "";
    private static String adsImage = "";
    private static String adsRedirectType = "";
    private static String adsRedirectURL = "";
    public static String getAdsTitle() {
        return adsTitle;
    }
    public static void setAdsTitle(String title) {
        adsTitle = title;
    }
    public static String getAdsImage() {
        return adsImage;
    }
    public static void setAdsImage(String image) {
        adsImage = image;
    }
    public static String getAdsRedirectType() {
        return adsRedirectType;
    }
    public static void setAdsRedirectType(String redirectType) {
        adsRedirectType = redirectType;
    }
    public static String getAdsRedirectURL() {
        return adsRedirectURL;
    }
    public static void setAdsRedirectURL(String redirectURL) {
        adsRedirectURL = redirectURL;
    }

    // Episodes ------------------------------------------------------------------------------------
    private static int playPosEpisodes = 0;
    public static int getPlayPosEpisodes() {
        return playPosEpisodes;
    }
    public static void setPlayPosEpisodes(int position) {
        playPosEpisodes = position;
    }

    private static final List<ItemEpisodes> arrayListEpisodes = new ArrayList<>();
    public static List<ItemEpisodes> getArrayListEpisodes() {
        return arrayListEpisodes;
    }
    public static void setArrayListEpisodes(List<ItemEpisodes> episodes) {
        arrayListEpisodes.addAll(episodes);
    }

    // LIVE ----------------------------------------------------------------------------------------
    private static int playPosLive = 0;
    public static int getPlayPosLive() {
        return playPosLive;
    }
    public static void setPlayPosLive(int position) {
        playPosLive = position;
    }

    private static final List<ItemChannel> arrayListLive = new ArrayList<>();
    public static List<ItemChannel> getArrayListLive() {
        return arrayListLive;
    }
    public static void setArrayListLive(List<ItemChannel> itemChannels) {
        arrayListLive.addAll(itemChannels);
    }

    // Notify --------------------------------------------------------------------------------------
    private static int posNotify = 0;
    public static int getPosNotify() {
        return posNotify;
    }
    public static void setPosNotify(int position) {
        posNotify = position;
    }

    private static final List<ItemNotification> arrayListNotify = new ArrayList<>();
    public static List<ItemNotification> getArrayListNotify() {
        return arrayListNotify;
    }

    // Blacklist -----------------------------------------------------------------------------------
    private static final List<ItemDns> arrayBlacklist = new ArrayList<>();
    public static List<ItemDns> getArrayBlacklist() {
        return arrayBlacklist;
    }

    // Poster list ---------------------------------------------------------------------------------
    private static int posterPos = 0;
    public static int getPosterPos() {
        return posterPos;
    }
    public static void setPosterPos(int position) {
        posterPos = position;
    }

    private static final List<ItemPoster> arrayListPoster = new ArrayList<>();
    public static List<ItemPoster> getArrayListPoster() {
        return arrayListPoster;
    }
    public static void setArrayListPoster(List<ItemPoster> itemPosters) {
        arrayListPoster.addAll(itemPosters);
    }

    // Radio ---------------------------------------------------------------------------------------
    private static Boolean isPlayed = false;
    public static Boolean getIsPlayed() {
        return isPlayed;
    }
    public static void setIsPlayed(Boolean played) {
        isPlayed = played;
    }

    private static int playPos = 0;
    public static int getPlayPos() {
        return playPos;
    }
    public static void setPlayPos(int position) {
        playPos = position;
    }

    private static final List<ItemChannel> arrayListPlay = new ArrayList<>();
    public static List<ItemChannel> getArrayListRadio() {
        return arrayListPlay;
    }
    public static void setArrayListRadio(List<ItemChannel> listRadio) {
        arrayListPlay.addAll(listRadio);
    }

    // Advertising ---------------------------------------------------------------------------------
    public static final String AD_TYPE_ADMOB = "admob";

    private static String adNetwork = AD_TYPE_ADMOB;
    public static String getAdNetwork() {
        return adNetwork;
    }
    public static void setAdNetwork(String network) {
        adNetwork = network;
    }

    private static Boolean isAdsStatus = false;
    public static Boolean getIsAdsStatus() {
        return isAdsStatus;
    }
    public static void setIsAdsStatus(Boolean adsStatus) {
        isAdsStatus = adsStatus;
    }

    private static int adCount = 0;
    public static int getAdCount() {
        return adCount;
    }
    public static void setAdCount(int count) {
        adCount = count;
    }

    private static int interstitialAdShow = 5;
    public static int getInterstitialAdShow() {
        return interstitialAdShow;
    }
    public static void setInterstitialAdShow(int count) {
        interstitialAdShow = count;
    }

    private static long rewardMinutes = 20;
    public static long getRewardMinutes() {
        return rewardMinutes;
    }
    public static void setRewardMinutes(long minutes) {
        rewardMinutes = minutes;
    }

    private static String admobBannerAdID = "";
    public static String getAdmobBannerAdID() {
        return admobBannerAdID;
    }
    public static void setAdmobBannerAdID(String bannerAdID) {
        admobBannerAdID = bannerAdID;
    }

    private static String admobInterstitialAdID = "";
    public static String getAdmobInterstitialAdID() {
        return admobInterstitialAdID;
    }
    public static void setAdmobInterstitialAdID(String interstitialAdID) {
        admobInterstitialAdID = interstitialAdID;
    }

    private static String admobRewardAdID = "";
    public static String getAdmobRewardAdID() {
        return admobRewardAdID;
    }
    public static void setAdmobRewardAdID(String rewardAdID) {
        admobRewardAdID = rewardAdID;
    }

    private static Boolean bannerMovie = false;
    public static Boolean getBannerMovie() {
        return bannerMovie;
    }
    public static void setBannerMovie(Boolean movieBanner) {
        bannerMovie = movieBanner;
    }

    private static Boolean bannerSeries = false;
    public static Boolean getBannerSeries() {
        return bannerSeries;
    }
    public static void setBannerSeries(Boolean seriesBanner) {
        bannerSeries = seriesBanner;
    }

    private static Boolean bannerEpg = false;
    public static Boolean getBannerEpg() {
        return bannerEpg;
    }
    public static void setBannerEpg(Boolean epgBanner) {
        bannerEpg = epgBanner;
    }

    private static Boolean isInterAd = false;
    public static Boolean getIsInterAd() {
        return isInterAd;
    }
    public static void setIsInterAd(Boolean interAd) {
        isInterAd = interAd;
    }

    private static Boolean rewardAdMovie = false;
    public static Boolean getRewardAdMovie() {
        return rewardAdMovie;
    }
    public static void setRewardAdMovie(Boolean movieRewardAd) {
        rewardAdMovie = movieRewardAd;
    }

    private static Boolean rewardAdEpisodes = false;
    public static Boolean getRewardAdEpisodes() {
        return rewardAdEpisodes;
    }
    public static void setRewardAdEpisodes(Boolean episodesRewardAd) {
        rewardAdEpisodes = episodesRewardAd;
    }

    private static Boolean rewardAdLive = false;
    public static Boolean getRewardAdLive() {
        return rewardAdLive;
    }
    public static void setRewardAdLive(Boolean liveRewardAd) {
        rewardAdLive = liveRewardAd;
    }

    private static Boolean rewardAdSingle = false;
    public static Boolean getRewardAdSingle() {
        return rewardAdSingle;
    }
    public static void setRewardAdSingle(Boolean singleRewardAd) {
        rewardAdSingle = singleRewardAd;
    }

    private static Boolean rewardAdLocal = false;
    public static Boolean getRewardAdLocal() {
        return rewardAdLocal;
    }
    public static void setRewardAdLocal(Boolean localRewardAd) {
        rewardAdLocal = localRewardAd;
    }
}