package nemosofts.streambox.util;

import static android.content.Context.UI_MODE_SERVICE;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.UiModeManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.database.Cursor;
import android.graphics.Color;
import android.media.AudioManager;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.provider.MediaStore;
import android.provider.Settings;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.TypedValue;
import android.view.WindowManager;
import android.view.WindowMetrics;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.media3.common.MimeTypes;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.extractor.ts.DefaultTsPayloadReaderFactory;
import androidx.media3.extractor.ts.TsExtractor;
import androidx.media3.ui.PlayerControlView;
import androidx.media3.ui.SubtitleView;
import androidx.nemosofts.theme.ThemeEngine;

import org.jetbrains.annotations.Contract;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.SelectPlayerActivity;
import nemosofts.streambox.activity.UiBlackPantherActivity;
import nemosofts.streambox.activity.UiChristmasActivity;
import nemosofts.streambox.activity.UiGlossyActivity;
import nemosofts.streambox.activity.UiHalloweenActivity;
import nemosofts.streambox.activity.UiMovieActivity;
import nemosofts.streambox.activity.UiOneActivity;
import nemosofts.streambox.activity.PlaylistActivity;
import nemosofts.streambox.activity.SingleStreamActivity;
import nemosofts.streambox.activity.UiVUIActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.CustomDefaultTrackNameProvider;
import nemosofts.streambox.util.player.CustomPlayerView;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.logging.HttpLoggingInterceptor;

public class ApplicationUtil {

    private static final String TAG = "ApplicationUtil";
    public static final String FEATURE_FIRE_TV = "amazon.hardware.fire_tv";

    private static final String USERNAME_KEY = "username";
    private static final String PASSWORD_KEY = "password";
    private static final String ACTION_KEY = "action";

    private ApplicationUtil() {
        throw new IllegalStateException("Utility class");
    }

    private static final Random RANDOM = new Random();
    public static int getRandomValue(int bound) {
        return RANDOM.nextInt(bound);
    }

    @NonNull
    public static String responsePost(String url, RequestBody requestBody) {
        // Set up logging for HTTP requests and responses
        HttpLoggingInterceptor logging = new HttpLoggingInterceptor();
        logging.setLevel(HttpLoggingInterceptor.Level.BASIC);

        OkHttpClient client = new OkHttpClient.Builder()
                .connectTimeout(15, TimeUnit.SECONDS)
                .writeTimeout(20, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .addInterceptor(logging)
                .cache(null)
                .build();

        // Build the POST request
        Request request = new Request.Builder()
                .url(url)
                .post(requestBody)
                .build();

        try {
            Response response = client.newCall(request).execute();
            return response.body() != null ? response.body().string() : "";
        } catch (Exception e) {
            return "";
        }
    }

    @NonNull
    public static String getMovieCredits(@NonNull String movieID, @NonNull String token) {
        String url = "https://api.themoviedb.org/3/movie/" + movieID + "/credits?language=en-US";
        return executeRequest(url, token);
    }

    @NonNull
    public static String getMovieImages(@NonNull String movieID, @NonNull String token) {
        String url = "https://api.themoviedb.org/3/movie/" + movieID + "/images";
        return executeRequest(url, token);
    }

    private static String executeRequest(String url, String token) {
        OkHttpClient client = new OkHttpClient();
        Request request = new Request.Builder()
                .url(url)
                .get()
                .addHeader("accept", "application/json")
                .addHeader("Authorization", "Bearer "+token)
                .build();
        try {
            Response response = client.newCall(request).execute();
            return response.body() != null ? response.body().string() : "";
        } catch (Exception e) {
            return "";
        }
    }

    @NonNull
    public static RequestBody getAPIRequestLogin(String username, String password) {
        return new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart(USERNAME_KEY, username)
                .addFormDataPart(PASSWORD_KEY, password)
                .build();
    }

    @NonNull
    public static  RequestBody getAPIRequest(String action, String username, String password) {
        return new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart(USERNAME_KEY, username)
                .addFormDataPart(PASSWORD_KEY, password)
                .addFormDataPart(ACTION_KEY, action)
                .build();
    }

    @NonNull
    public static  RequestBody getAPIRequestID(String action, String type, String seriesID,
                                               String username, String password) {
        return new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart(USERNAME_KEY, username)
                .addFormDataPart(PASSWORD_KEY, password)
                .addFormDataPart(ACTION_KEY, action)
                .addFormDataPart(type, seriesID)
                .build();
    }

    @NonNull
    public static String toBase64(@NonNull String input) {
        byte[] encodeValue = Base64.encode(input.getBytes(), Base64.DEFAULT);
        return new String(encodeValue);
    }

    @NonNull
    public static String decodeBase64(String encoded) {
        byte[] decodedBytes = Base64.decode(encoded, Base64.DEFAULT);
        return new String(decodedBytes);
    }

    @NonNull
    public static String encodeBase64(@NonNull String encoded) {
        byte[] decodedBytes = Base64.encode(encoded.getBytes(), Base64.DEFAULT);
        return new String(decodedBytes);
    }

    public static int getColumnWidth(@NonNull Context ctx, int column, int gridPadding) {
        Resources r = ctx.getResources();
        float padding = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, gridPadding, r.getDisplayMetrics());
        return (int) ((getScreenWidth(ctx) - ((column + 1) * padding)) / column);
    }

    private static int getScreenWidth(@NonNull Context ctx) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            WindowManager wm = ctx.getSystemService(WindowManager.class);
            WindowMetrics windowMetrics = wm.getCurrentWindowMetrics();
            return windowMetrics.getBounds().width();
        } else {
            DisplayMetrics displayMetrics = ctx.getResources().getDisplayMetrics();
            return displayMetrics.widthPixels;
        }
    }

    @NonNull
    public static String convertIntToDate(String convertDate, String pattern) {
        if (convertDate == null || convertDate.isEmpty()) {
            return " none";
        }

        if (convertDate.equals("null")) {
            return " Unlimited";
        }

        try {
            long timestamp = Long.parseLong(convertDate);
            Date date = new Date(timestamp * 1000);
            SimpleDateFormat dateFormat = new SimpleDateFormat(pattern, Locale.getDefault());
            return " " + dateFormat.format(date);
        } catch (NumberFormatException e) {
            return " none";
        }
    }

    @NonNull
    public static String readableFileSize(long size) {
        if (size <= 0) return "0 Bytes";
        final String[] units = new String[]{"Bytes", "kB", "MB", "GB", "TB"};
        int digitGroups = (int) (Math.log10(size) / Math.log10(1024));
        return new DecimalFormat("#,##0.#").format(size / Math.pow(1024, digitGroups)) + " " + units[digitGroups];
    }

    @NonNull
    public static String timeFormat(String time) {
        if (time == null || time.isEmpty()) {
            return "0";
        }

        try {
            int totalMinutes = Integer.parseInt(time);
            int hours = totalMinutes / 60;
            int minutes = totalMinutes % 60;
            return formatTime(hours, minutes);
        } catch (NumberFormatException e) {
            return "0";
        }
    }

    @NonNull
    private static String formatTime(int hours, int minutes) {
        if (hours > 0) {
            return hours + "h " + minutes + "m";
        } else if (minutes > 0) {
            return minutes + "m";
        } else {
            return "0";
        }
    }

    @NonNull
    public static String formatTimeToTime(String timeString) {
        if (timeString == null || timeString.trim().isEmpty()) {
            return "0";
        }

        try {
            String[] timeParts = timeString.split(":");
            int hours = Integer.parseInt(timeParts[0]);
            int minutes = Integer.parseInt(timeParts[1]);
            int seconds = Integer.parseInt(timeParts[2]);
            return formatTime(hours, minutes) + " " + seconds + "s";
        } catch (Exception e) {
            return "0";
        }
    }

    public static Boolean calculateUpdateHours(@NonNull String inputDateStr, int updateHours){
        boolean isUpdate = false;
        try {
            if (!inputDateStr.isEmpty()){
                @SuppressLint("SimpleDateFormat") SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
                Date inputDate = dateFormat.parse(inputDateStr);
                Date currentDate = new Date();
                assert inputDate != null;
                long timeDifferenceInMillis = currentDate.getTime() - inputDate.getTime();
                long seconds = timeDifferenceInMillis / 1000;
                int hours = (int) (seconds / 3600);
                isUpdate = hours > updateHours;
            }
        } catch (Exception e) {
            return isUpdate;
        }
        return isUpdate;
    }

    @NonNull
    public static String calculateTimeSpan(String inputDateStr) {
        final String NOT_AVAILABLE = " not available";
        if (inputDateStr == null || inputDateStr.trim().isEmpty()) {
            return NOT_AVAILABLE;
        }

        try {
            @SuppressLint("SimpleDateFormat")
            SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
            Date inputDate = dateFormat.parse(inputDateStr);
            if (inputDate == null) {
                return NOT_AVAILABLE;
            }

            long timeDifferenceInMillis = new Date().getTime() - inputDate.getTime();
            long seconds = timeDifferenceInMillis / 1000;
            return TimeFormatter.formatTimeSpan(seconds);
        } catch (Exception e) {
            return NOT_AVAILABLE;
        }
    }

    @NonNull
    public static String averageRating(String rating) {
        if (rating == null || rating.isEmpty()) {
            return "0";
        }

        try {
            float floatRating = Float.parseFloat(rating);
            int roundedRating = Math.max(0, Math.min(5, (int) Math.floor(floatRating)));
            return String.valueOf(roundedRating);
        } catch (NumberFormatException e) {
            return "0";  // Return "0" for invalid numeric formats
        }
    }

    // TvBox
    public static boolean isTvBox(@NonNull Context context) {
        final PackageManager pm = context.getPackageManager();

        // TV for sure
        UiModeManager uiModeManager = (UiModeManager) context.getSystemService(UI_MODE_SERVICE);
        if (uiModeManager.getCurrentModeType() == Configuration.UI_MODE_TYPE_TELEVISION) {
            return true;
        }

        if (pm.hasSystemFeature(FEATURE_FIRE_TV)) {
            return true;
        }

        // Legacy storage no longer works on Android 11 (level 30)
        if (Build.VERSION.SDK_INT < 30) {
            // (Some boxes still report touchscreen feature)
            if (!pm.hasSystemFeature(PackageManager.FEATURE_TOUCHSCREEN) && !pm.hasSystemFeature(PackageManager.FEATURE_TELEPHONY)) {
                return true;
            }

            if (pm.hasSystemFeature("android.hardware.hdmi.cec")) {
                return true;
            }

            return Build.MANUFACTURER.equalsIgnoreCase("zidoo");
        }

        // Default: No TV - use SAF
        return false;
    }

    public static boolean isLandscape(@NonNull Context context) {
        int orientation = context.getResources().getConfiguration().orientation;
        return orientation == Configuration.ORIENTATION_LANDSCAPE;
    }

    public static boolean isLandscapeWallpaper(int width, int height) {
        return width > height;
    }

    public static boolean isTablet(@NonNull Context context) {
        return context.getResources().getConfiguration().smallestScreenWidthDp >= 720;
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void showText(@NonNull final CustomPlayerView playerView, final String text, final long timeout) {
        playerView.removeCallbacks(playerView.textClearRunnable);
        playerView.clearIcon();
        playerView.setCustomErrorMessage(text);
        playerView.postDelayed(playerView.textClearRunnable, timeout);
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void showText(final CustomPlayerView playerView, final String text) {
        showText(playerView, text, 1200);
    }

    public static void openThemeActivity(Activity activity) {
        if (activity.isFinishing()){
            return;
        }
        int theme = new SPHelper(activity).getIsTheme();
        Intent intent;
        if (theme == 2){
            intent = new Intent(activity, UiGlossyActivity.class);
        } else  if (theme == 3){
            intent = new Intent(activity, UiBlackPantherActivity.class);
        } else  if (theme == 4){
            intent = new Intent(activity, UiMovieActivity.class);
        } else if (theme == 5){
            intent = new Intent(activity, UiVUIActivity.class);
        } else if (theme == 6){
            intent = new Intent(activity, UiChristmasActivity.class);
        } else if (theme == 7){
            intent = new Intent(activity, UiHalloweenActivity.class);
        } else {
            intent = new Intent(activity, UiOneActivity.class);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        activity.startActivity(intent);
        activity.finish();
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void openHomeActivity(Activity activity) {
        SPHelper spHelper = new SPHelper(activity);
        Intent intent;
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            intent = new Intent(activity, SingleStreamActivity.class);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            intent = new Intent(activity, PlaylistActivity.class);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            int theme = spHelper.getIsTheme();
            if (theme == 2){
                intent = new Intent(activity, UiGlossyActivity.class);
            }  else if (theme == 3){
                intent = new Intent(activity, UiBlackPantherActivity.class);
            } else if (theme == 4){
                intent = new Intent(activity, UiMovieActivity.class);
            } else if (theme == 5){
                intent = new Intent(activity, UiVUIActivity.class);
            } else if (theme == 6){
                intent = new Intent(activity, UiChristmasActivity.class);
            } else if (theme == 7){
                intent = new Intent(activity, UiHalloweenActivity.class);
            } else {
                intent = new Intent(activity, UiOneActivity.class);
            }
        } else {
            intent = new Intent(activity, SelectPlayerActivity.class);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        activity.startActivity(intent);
        activity.finish();
    }

    public static int openThemeBg(Activity activity) {
        int theme = new SPHelper(activity).getIsTheme();
        int themePage = new ThemeEngine(activity).getThemePage();
        if (theme == 2){
            return R.drawable.bg_ui_glossy;
        } else if (theme == 3){
            return R.drawable.bg_dark_panther;
        } else {
            if (themePage == 0){
                return R.drawable.bg_dark;
            } else if (themePage == 1 || themePage == 2 || themePage == 3){
                return R.drawable.bg_classic;
            } else {
                return R.drawable.bg_dark;
            }
        }
    }

    @NonNull
    @Contract(pure = true)
    public static String containerExtension(String container) {
        if (container != null){
            if (container.contains(".")){
                return container;
            } else {
                return "."+container;
            }
        } else {
            return ".mp4";
        }
    }

    @NonNull
    @Contract(pure = true)
    public static Boolean isAdultsCount(@NonNull String count) {
        String normalizedCount = count.toLowerCase();
        // List of keywords related to adult content
        String[] adultKeywords = {"18+", "+18", "[18+]", "adults", "adult", "xxx", "pron", "sex"};
        // Check if any of the keywords are present in the normalized string
        for (String keyword : adultKeywords) {
            if (normalizedCount.contains(keyword)) {
                return true;
            }
        }
        return false;
    }

    @SuppressLint("HardwareIds")
    public static String getDeviceID(Context context) {
        try {
            return Settings.Secure.getString(context.getContentResolver(), Settings.Secure.ANDROID_ID);
        } catch (Exception e) {
            return "N/A";
        }
    }

    public static int getBatteryDrawable(int status, int level, int scale) {
        float batteryLevel = (level / (float) scale) * 100;
        boolean isCharging = (status == BatteryManager.BATTERY_STATUS_CHARGING);
        if (isCharging){
            return R.drawable.ic_battery_charging;
        } else if (batteryLevel < 10){
            return R.drawable.ic_battery_disable;
        } else if (batteryLevel < 20){
            return R.drawable.ic_battery_empty;
        } else if (batteryLevel < 30){
            return R.drawable.ic_battery_one;
        } else if (batteryLevel < 50){
            return R.drawable.ic_battery_two;
        } else {
            return R.drawable.ic_battery_full;
        }
    }

    @NonNull
    public static String getTimestamp(String data, boolean is12h) {
        try {
            long timestamp = Long.parseLong(data);
            // Create a Date object using the timestamp
            Date date = new Date(timestamp * 1000);
            // Create a SimpleDateFormat object to define the desired date and time format
            SimpleDateFormat sdf;
            if (is12h) {
                sdf = new SimpleDateFormat("hh:mm a", Locale.getDefault()); // Changed to 12-hour format
            } else {
                sdf = new SimpleDateFormat("HH:mm", Locale.getDefault()); // 24-hour format
            }
            // Format the date using the SimpleDateFormat
            return sdf.format(date);
        } catch (Exception e) {
            return "";
        }
    }

    @NonNull
    public static String formatFrameRate(float frameRate) {
        DecimalFormat decimalFormat = new DecimalFormat("#.#");
        return decimalFormat.format(frameRate);
    }

    public static String format(Number number) {
        if (number != null){
            char[] suffix = {' ', 'k', 'M', 'B', 'T', 'P', 'E'};
            long numValue = number.longValue();
            int value = (int) Math.floor(Math.log10(numValue));
            int base = value / 3;
            if (value >= 3 && base < suffix.length) {
                return new DecimalFormat("#0.0").format(numValue / Math.pow(10, (double) base * 3)) + suffix[base];
            } else {
                return new DecimalFormat("#,##0").format(numValue);
            }
        } else {
            return String.valueOf(0);
        }
    }

    @NonNull
    @Contract(pure = true)
    public static String getVideoResolution(int height) {
        try {
            if (height >= 4320) {
                return "8k";
            } else if (height >= 2160) {
                return "4k";
            } else if (height >= 1440) {
                return "2k";
            } else if (height >= 1080) {
                return "1080p";
            } else if (height >= 720) {
                return "720p";
            } else if (height >= 480) {
                return "480p";
            } else if (height >= 360) {
                return "360p";
            } else if (height >= 240) {
                return "240p";
            } else if (height >= 140) {
                return "140p";
            } else {
                return "Unknown resolution";
            }
        } catch (Exception e) {
            return "Unknown resolution";
        }
    }

    public static void setRating(String rating, ImageView star1, ImageView star2, ImageView star3,
                                 ImageView star4, ImageView star5) {
        if (rating == null) {
            return;
        }
        try {
            String average = averageRating(rating);
            if (star1 != null && star2 != null && star3 != null && star4 != null && star5 != null) {
                switch (average) {
                    case "1" :
                        star1.setImageResource(R.drawable.ic_star);
                        star2.setImageResource(R.drawable.ic_star_border);
                        star3.setImageResource(R.drawable.ic_star_border);
                        star4.setImageResource(R.drawable.ic_star_border);
                        star5.setImageResource(R.drawable.ic_star_border);
                        break;
                    case "2" :
                        star1.setImageResource(R.drawable.ic_star);
                        star2.setImageResource(R.drawable.ic_star);
                        star3.setImageResource(R.drawable.ic_star_border);
                        star4.setImageResource(R.drawable.ic_star_border);
                        star5.setImageResource(R.drawable.ic_star_border);
                        break;
                    case "3" :
                        star1.setImageResource(R.drawable.ic_star);
                        star2.setImageResource(R.drawable.ic_star);
                        star3.setImageResource(R.drawable.ic_star);
                        star4.setImageResource(R.drawable.ic_star_border);
                        star5.setImageResource(R.drawable.ic_star_border);
                        break;
                    case "4" :
                        star1.setImageResource(R.drawable.ic_star);
                        star2.setImageResource(R.drawable.ic_star);
                        star3.setImageResource(R.drawable.ic_star);
                        star4.setImageResource(R.drawable.ic_star);
                        star5.setImageResource(R.drawable.ic_star_border);
                        break;
                    case "5" :
                        star1.setImageResource(R.drawable.ic_star);
                        star2.setImageResource(R.drawable.ic_star);
                        star3.setImageResource(R.drawable.ic_star);
                        star4.setImageResource(R.drawable.ic_star);
                        star5.setImageResource(R.drawable.ic_star);
                        break;
                    default :
                        star1.setImageResource(R.drawable.ic_star_border);
                        star2.setImageResource(R.drawable.ic_star_border);
                        star3.setImageResource(R.drawable.ic_star_border);
                        star4.setImageResource(R.drawable.ic_star_border);
                        star5.setImageResource(R.drawable.ic_star_border);
                        break;
                }
            }
        } catch (Exception e) {
            Log.e("setRating", "Exception", e);
        }
    }

    @NonNull
    public static CharSequence setErrorMsg(String errorMsg) {
        try {
            SpannableStringBuilder builder = new SpannableStringBuilder(errorMsg);
            builder.setSpan(new ForegroundColorSpan(Color.WHITE), 0, errorMsg.length(), 0);
            return builder;
        } catch (Exception e) {
            return errorMsg;
        }
    }

    @NonNull
    @OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
    public static DefaultExtractorsFactory getDefaultExtractorsFactory() {
        return new DefaultExtractorsFactory()
                .setTsExtractorFlags(DefaultTsPayloadReaderFactory.FLAG_ENABLE_HDMV_DTS_AUDIO_STREAMS
                        | DefaultTsPayloadReaderFactory.FLAG_IGNORE_AAC_STREAM
                        | DefaultTsPayloadReaderFactory.FLAG_IGNORE_H264_STREAM)
                .setTsExtractorTimestampSearchBytes(1500 * TsExtractor.TS_PACKET_SIZE);
    }

    @NonNull
    @OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
    public static DefaultRenderersFactory getDefaultRenderersFactory(Context context, @NonNull Boolean isHardwareDecoding) {
        DefaultRenderersFactory renderersFactory = new DefaultRenderersFactory(context);
        if (Boolean.TRUE.equals(isHardwareDecoding)){
            renderersFactory.setEnableDecoderFallback(true); // Allow fallback to software decoder if necessary
            renderersFactory.setExtensionRendererMode(DefaultRenderersFactory.EXTENSION_RENDERER_MODE_PREFER); // Prefer hardware
        } else {
            renderersFactory.setEnableDecoderFallback(false); // No fallback to hardware decoders
            renderersFactory.setExtensionRendererMode(DefaultRenderersFactory.EXTENSION_RENDERER_MODE_OFF); // Force software decoding
        }
        return renderersFactory;
    }

    @OptIn(markerClass = UnstableApi.class)
    @NonNull
    public static String getInfoAudio( ExoPlayer exoPlayer) {
        String infoAudio = """
        Audio Sample Rate: N/A
    
        Audio Channels: N/A
    
        Audio Type: N/A
    
        Audio MIME Type: N/A
        """;

        if (exoPlayer == null){
            return infoAudio;
        }
        if (exoPlayer.getAudioFormat() != null){
            int audioSampleRate = exoPlayer.getAudioFormat().sampleRate;
            int audioChannels = exoPlayer.getAudioFormat().channelCount;
            String audioMimeType = exoPlayer.getAudioFormat().sampleMimeType;

            infoAudio = "Audio Sample Rate: " + audioSampleRate + "\n\n"
                    + "Audio Channels: " + audioChannels + "\n\n"
                    + "Audio Type: "+ formatAudioFromMime(audioMimeType) +"\n\n"
                    + "Audio MIME Type: " + audioMimeType +"\n";

        }
        return infoAudio;
    }

    @OptIn(markerClass = UnstableApi.class)
    @NonNull
    public static String getInfoVideo(ExoPlayer exoPlayer, boolean isLive) {
        String infoVideo = """
        Video Quality : Unknown resolution
    
        Video Width: N/A
    
        Video Height: N/A
        """;

        if (exoPlayer == null){
            return infoVideo;
        }
        if (exoPlayer.getVideoFormat() != null){
            int videoWidth = exoPlayer.getVideoFormat().width;
            int videoHeight = exoPlayer.getVideoFormat().height;
            int videoBitrate = exoPlayer.getVideoFormat().bitrate;
            float frameRate = exoPlayer.getVideoFormat().frameRate;
            String finalRate = formatFrameRate(frameRate);

            if (isLive){
                infoVideo = "Video Quality: " + ApplicationUtil.getVideoResolution(videoHeight)+ "\n\n"
                        + "Video Width: " + videoWidth + "\n\n"
                        + "Video Height: " + videoHeight + "\n";
            } else {
                infoVideo = "Video Quality: " + ApplicationUtil.getVideoResolution(videoHeight)+ "\n\n"
                        + "Video Width: " + videoWidth + "\n\n"
                        + "Video Height: " + videoHeight + "\n\n"
                        + "Video Bitrate: " + videoBitrate + "\n\n"
                        + "Video Frame Rate: " + finalRate + "\n";
            }
        }
        return infoVideo;
    }

    @OptIn(markerClass = UnstableApi.class)
    public static String formatAudioFromMime(final String mimeType) {
        if (mimeType == null) {
            return "N/A";
        }
        return switch (mimeType) {
            case MimeTypes.AUDIO_DTS -> "DTS";
            case MimeTypes.AUDIO_DTS_HD -> "DTS-HD";
            case MimeTypes.AUDIO_DTS_EXPRESS -> "DTS Express";
            case MimeTypes.AUDIO_TRUEHD -> "TrueHD";
            case MimeTypes.AUDIO_AC3 -> "AC-3";
            case MimeTypes.AUDIO_E_AC3 -> "E-AC-3";
            case MimeTypes.AUDIO_E_AC3_JOC -> "E-AC-3-JOC";
            case MimeTypes.AUDIO_AC4 -> "AC-4";
            case MimeTypes.AUDIO_AAC -> "AAC";
            case MimeTypes.AUDIO_MPEG -> "MP3";
            case MimeTypes.AUDIO_MPEG_L2 -> "MP2";
            case MimeTypes.AUDIO_VORBIS -> "Vorbis";
            case MimeTypes.AUDIO_OPUS -> "Opus";
            case MimeTypes.AUDIO_FLAC -> "FLAC";
            case MimeTypes.AUDIO_ALAC -> "ALAC";
            case MimeTypes.AUDIO_WAV -> "WAV";
            case MimeTypes.AUDIO_AMR -> "AMR";
            case MimeTypes.AUDIO_AMR_NB -> "AMR-NB";
            case MimeTypes.AUDIO_AMR_WB -> "AMR-WB";
            case MimeTypes.AUDIO_IAMF -> "IAMF";
            case MimeTypes.AUDIO_MPEGH_MHA1, MimeTypes.AUDIO_MPEGH_MHM1 -> "MPEG-H";
            case MimeTypes.APPLICATION_PGS -> "PGS";
            case MimeTypes.APPLICATION_SUBRIP -> "SRT";
            case MimeTypes.TEXT_SSA -> "SSA";
            case MimeTypes.TEXT_VTT -> "VTT";
            case MimeTypes.APPLICATION_TTML -> "TTML";
            case MimeTypes.APPLICATION_TX3G -> "TX3G";
            case MimeTypes.APPLICATION_DVBSUBS -> "DVB";
            default -> mimeType;
        };
    }

    @Nullable
    public static String getVideoId(String videoUrl) {
        // Simplified regular expression to capture the video ID
        final String reg = "(youtu\\.be/|youtube\\.com/(watch\\?v=|embed/|v/|.+?&v=))([a-zA-Z0-9_-]{11})";
        if (videoUrl == null || videoUrl.trim().isEmpty())
            return null;

        Pattern pattern = Pattern.compile(reg, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(videoUrl);
        if (matcher.find())
            return matcher.group(3); // Group 3 captures the video ID directly

        return null;
    }

    public static float normalizeFontScale(float fontScale, boolean small) {
        // https://bbc.github.io/subtitle-guidelines/#Presentation-font-size
        if (fontScale >= 1.99f) {
            return small ? 1.15f : 1.2f;
        }
        if (fontScale > 1.01f) {
            return small ? 1.0f : 1.1f;
        }

        // Handle small font scales
        if (fontScale <= 0.26f) {
            return small ? 0.65f : 0.8f;
        }
        if (fontScale < 0.99f) {
            return small ? 0.75f : 0.9f;
        }

        // Default case for font scale ~1.0
        return small ? 0.85f : 1.0f;
    }

    @OptIn(markerClass = UnstableApi.class)
    public static float getaFloat(@NonNull Context context, float subtitlesScale) {
        final float size;
        if (context.getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE) {
            size = SubtitleView.DEFAULT_TEXT_SIZE_FRACTION * subtitlesScale;
        } else {
            DisplayMetrics metrics = context.getResources().getDisplayMetrics();
            float ratio = ((float)metrics.heightPixels / (float)metrics.widthPixels);
            if (ratio < 1)
                ratio = 1 / ratio;
            size = SubtitleView.DEFAULT_TEXT_SIZE_FRACTION * subtitlesScale / ratio;
        }
        return size;
    }


    // Helper method to get the volume -------------------------------------------------------------
    public static int getVolume(final Context context, final boolean max, final AudioManager audioManager) {
        // Handle Samsung-specific volume retrieval for Android SDK 30 and above
        if (isSamsungDeviceWithSDK30OrAbove()) {
            Integer samsungVolume = getSamsungVolume(context, max, audioManager);
            if (samsungVolume != null) {
                return samsungVolume;
            }
        }

        // Fallback to standard volume retrieval
        return max ? audioManager.getStreamMaxVolume(AudioManager.STREAM_MUSIC)
                : audioManager.getStreamVolume(AudioManager.STREAM_MUSIC);
    }

    // Checks if the device is Samsung and running SDK 30 or above
    private static boolean isSamsungDeviceWithSDK30OrAbove() {
        return Build.VERSION.SDK_INT >= 30 && Build.MANUFACTURER.equalsIgnoreCase("samsung");
    }

    // Retrieves the Samsung-specific volume if possible, otherwise returns null
    @Nullable
    private static Integer getSamsungVolume(Context context, boolean max, AudioManager audioManager) {
        try {
            int mediaVolumeInterval = getSamsungMediaVolumeInterval(context);
            if (mediaVolumeInterval < 10) {
                return retrieveFineVolume(max, audioManager, mediaVolumeInterval);
            }
        }  catch (ClassNotFoundException | NoSuchMethodException | InstantiationException |
                  IllegalAccessException | InvocationTargetException e) {
            e.printStackTrace();
        }
        return null;
    }

    // Gets the media volume interval from Samsung's SemSoundAssistantManager
    private static int getSamsungMediaVolumeInterval(Context context)
            throws ClassNotFoundException, NoSuchMethodException, InstantiationException,
            IllegalAccessException, InvocationTargetException {
        Class<?> clazz = Class.forName("com.samsung.android.media.SemSoundAssistantManager");
        Constructor<?> constructor = clazz.getConstructor(Context.class);
        Method getMediaVolumeInterval = clazz.getDeclaredMethod("getMediaVolumeInterval");
        Object result = getMediaVolumeInterval.invoke(constructor.newInstance(context));
        return result instanceof Integer ? (int) result : 10; // Default interval if not found
    }

    // Retrieves the fine volume using Samsung's semGetFineVolume method
    @Nullable
    private static Integer retrieveFineVolume(boolean max, AudioManager audioManager,
                                              int mediaVolumeInterval) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        Method method = AudioManager.class.getDeclaredMethod("semGetFineVolume", int.class);
        Object result = method.invoke(audioManager, AudioManager.STREAM_MUSIC);
        if (result instanceof Integer) {
            int fineVolume = (int) result;
            return max ? 150 / mediaVolumeInterval : fineVolume / mediaVolumeInterval;
        }
        return null;
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void setCustomTrackNameProvider(Context context, CustomPlayerView playerView) {
        if (context == null || playerView == null) {
            return;
        }
        try {
            PlayerControlView controlView = playerView.findViewById(androidx.media3.ui.R.id.exo_controller);
            CustomDefaultTrackNameProvider trackNameProvider = new CustomDefaultTrackNameProvider(context.getResources());
            final Field field = PlayerControlView.class.getDeclaredField("trackNameProvider");
            field.setAccessible(true);
            field.set(controlView, trackNameProvider);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            Log.e(TAG, "Failed to set custom track name provider", e);
        }
    }

    @Nullable
    public static String getRealPathFromURI(Context ctx, Uri contentUri) {
        try {
            @SuppressLint("Recycle") Cursor cursor = ctx.getContentResolver().query(contentUri,
                    null, null, null, null);
            if (cursor == null) {
                return contentUri.getPath();
            }
            cursor.moveToFirst();
            int idx = cursor.getColumnIndex(MediaStore.MediaColumns.DATA);
            return cursor.getString(idx);
        } catch (Exception e) {
            return null;
        }
    }
}