package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadChannel;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.executor.LoadMovies;
import nemosofts.streambox.executor.LoadSeries;
import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.advertising.AdManagerInterAdmob;
import nemosofts.streambox.util.advertising.GDPRChecker;
import nemosofts.streambox.util.advertising.RewardAdAdmob;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class UiBlackPantherActivity extends AppCompatActivity {

    private static final String TAG = "BlackPantherActivity";
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private ProgressDialog progressDialog;
    private TextView tvAutoRenewLive;
    private TextView tvAutoRenewMovie;
    private TextView tvAutoRenewSeries;
    private ImageView ivAutoRenewLive;
    private ImageView ivAutoRenewMovie;
    private ImageView ivAutoRenewSeries;
    private ProgressBar pbLive;
    private ProgressBar pbMovie;
    private ProgressBar pbSerials;
    private final Handler handlerLive = new Handler(Looper.getMainLooper());
    private final Handler handlerMovie = new Handler(Looper.getMainLooper());
    private final Handler handlerSeries = new Handler(Looper.getMainLooper());
    private int progressStatusLive = 0;
    private int progressStatusMovie = 0;
    private int progressStatusSeries = 0;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(UiBlackPantherActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        progressDialog = new ProgressDialog(UiBlackPantherActivity.this,true);

        tvAutoRenewLive = findViewById(R.id.tv_tv_auto_renew);
        tvAutoRenewMovie = findViewById(R.id.tv_movie_auto_renew);
        tvAutoRenewSeries = findViewById(R.id.tv_series_auto_renew);

        ivAutoRenewLive = findViewById(R.id.iv_tv_auto_renew);
        ivAutoRenewMovie = findViewById(R.id.iv_movie_auto_renew);
        ivAutoRenewSeries = findViewById(R.id.iv_series_auto_renew);

        pbLive = findViewById(R.id.pb_live_tv);
        pbMovie = findViewById(R.id.pb_movie);
        pbSerials = findViewById(R.id.pb_serials);

        setListenerHome();

        changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
        changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, true);
        changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, true);

        if (spHelper.isLogged()){
            TextView tvUserName = findViewById(R.id.tv_user_name);
            String userName = getString(R.string.user_list_user_name)+" "+ spHelper.getAnyName();
            tvUserName.setText(userName);

            String expDate = getString(R.string.expiration)+" "+ ApplicationUtil.convertIntToDate(spHelper.getExpDate(), "MMMM dd, yyyy");
            TextView tvDate = findViewById(R.id.tv_exp_date);
            tvDate.setText(expDate);
        }

        setWifiIcon();
        setFormattedDate();

        loadLogin();

        chalkedDataLive();
        chalkedDataMovie();
        chalkedDataSerials();

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.select_live).requestFocus();
        }

        setAds();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isFinishing()){
               return;
            }
            DialogUtil.popupAdsDialog(UiBlackPantherActivity.this);
        }, 600);
    }

    private void setAds() {
        if (ApplicationUtil.isTvBox(this)){
            return;
        }
        new GDPRChecker(UiBlackPantherActivity.this).check();
        if (Boolean.TRUE.equals(Callback.getRewardAdMovie() || Callback.getRewardAdEpisodes()
                || Callback.getRewardAdLive() || Callback.getRewardAdSingle() || Callback.getRewardAdLocal())) {
            RewardAdAdmob rewardAdAdmob = new RewardAdAdmob(getApplicationContext());
            rewardAdAdmob.createAd();
        }
        if (Boolean.TRUE.equals(Callback.getIsInterAd())) {
            AdManagerInterAdmob adManagerInterAdmob = new AdManagerInterAdmob(getApplicationContext());
            adManagerInterAdmob.createAd();
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setListenerHome() {
        if (!spHelper.getIsDownload()){
            findViewById(R.id.iv_file_download).setVisibility(View.GONE);
        }
        if (Boolean.FALSE.equals(spHelper.getIsRadio())){
            findViewById(R.id.iv_radio).setVisibility(View.GONE);
        }
        findViewById(R.id.iv_notifications).setOnClickListener(v -> startActivity(new Intent(UiBlackPantherActivity.this, NotificationsActivity.class)));
        findViewById(R.id.iv_file_download).setOnClickListener(v -> startActivity(new Intent(UiBlackPantherActivity.this, DownloadActivity.class)));
        findViewById(R.id.iv_profile).setOnClickListener(v -> startActivity(new Intent(UiBlackPantherActivity.this, ProfileActivity.class)));
        findViewById(R.id.iv_profile_re).setOnClickListener(v -> signOut());
        findViewById(R.id.iv_settings).setOnClickListener(v -> startActivity(new Intent(UiBlackPantherActivity.this, SettingActivity.class)));
        findViewById(R.id.select_live).setOnClickListener(v -> selectLive());
        findViewById(R.id.select_movie).setOnClickListener(v -> selectMovie());
        findViewById(R.id.select_serials).setOnClickListener(v -> selectSerials());
        findViewById(R.id.select_epg).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiBlackPantherActivity.this, CategoriesActivity.class));
            }
        });
        findViewById(R.id.select_multiple_screen).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiBlackPantherActivity.this, MultipleScreenActivity.class));
            }
        });
        findViewById(R.id.select_catch_up).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiBlackPantherActivity.this, CatchUpActivity.class));
            }
        });
        findViewById(R.id.iv_radio).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiBlackPantherActivity.this, RadioActivity.class));
            }
        });

        setAutoRenew();
        setLongClickListener();
    }

    private void setAutoRenew() {
        findViewById(R.id.ll_tv_auto_renew).setOnClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_TV, type -> getLive());
            } else {
                getLive();
            }
        });
        findViewById(R.id.ll_movie_auto_renew).setOnClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_MOVIE, type -> getMovies());
            } else {
                getMovies();
            }
        });
        findViewById(R.id.ll_series_auto_renew).setOnClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_SERIES, type -> getSeries());
            } else {
                getSeries();
            }
        });
    }

    private void setLongClickListener() {
        findViewById(R.id.select_live).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_TV, type -> getLive());
            }
            return false;
        });
        findViewById(R.id.select_movie).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_TV, type -> getMovies());
            }
            return false;
        });
        findViewById(R.id.select_serials).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                DialogUtil.downloadDataDialog(UiBlackPantherActivity.this, Callback.TAG_TV, type -> getSeries());
            }
            return false;
        });
    }

    private void selectSerials() {
        if (spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
            getSeries();
        } else {
            startActivity(new Intent(UiBlackPantherActivity.this, SeriesActivity.class));
        }
    }

    private void selectMovie() {
        if (spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
            getMovies();
        } else {
            startActivity(new Intent(UiBlackPantherActivity.this, MovieActivity.class));
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void selectLive() {
        if (spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
            getLive();
        } else {
            startActivity(new Intent(UiBlackPantherActivity.this, LiveTvActivity.class));
        }
    }

    private void chalkedDataLive() {
        if (Boolean.TRUE.equals(Callback.getSuccessLive().equals("1"))){
            try {
                Callback.setSuccessLive("0");
                pbLive.setVisibility(View.VISIBLE);
                progressStatusLive = 0;
                pbLive.setProgress(progressStatusLive);
                findViewById(R.id.vw_live_tv).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_live_epg).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_catch_up).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_multiple_screen).setVisibility(View.VISIBLE);
                handlerLive.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusLive < 100) {
                            progressStatusLive++;
                            pbLive.setProgress(progressStatusLive);
                            if (progressStatusLive == 99){
                                findViewById(R.id.vw_live_tv).setVisibility(View.GONE);
                                findViewById(R.id.vw_live_epg).setVisibility(View.GONE);
                                findViewById(R.id.vw_catch_up).setVisibility(View.GONE);
                                findViewById(R.id.vw_multiple_screen).setVisibility(View.GONE);
                                pbLive.setVisibility(View.GONE);
                            }
                            spHelper.setCurrentDate(Callback.TAG_TV);
                            changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
                            handlerLive.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                Log.e(TAG, "Error chalkedDataLive",e);
            }
        }
    }

    private void chalkedDataMovie() {
        if (Boolean.TRUE.equals(Callback.getSuccessMovies().equals("1"))){
            try {
                Callback.setSuccessMovies("0");
                pbMovie.setVisibility(View.VISIBLE);
                progressStatusMovie = 0;
                pbMovie.setProgress(progressStatusMovie);
                findViewById(R.id.vw_movie).setVisibility(View.VISIBLE);
                handlerMovie.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusMovie < 100) {
                            progressStatusMovie++;
                            pbMovie.setProgress(progressStatusMovie);
                            if (progressStatusMovie == 99){
                                findViewById(R.id.vw_movie).setVisibility(View.GONE);
                                pbMovie.setVisibility(View.GONE);
                            }
                            spHelper.setCurrentDate(Callback.TAG_MOVIE);
                            changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, false);
                            handlerMovie.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                Log.e(TAG, "Error chalkedDataMovie",e);
            }
        }
    }

    private void chalkedDataSerials() {
        if (Boolean.TRUE.equals(Callback.getSuccessSeries().equals("1"))){
            try {
                Callback.setSuccessSeries("0");
                pbSerials.setVisibility(View.VISIBLE);
                progressStatusSeries = 0;
                pbSerials.setProgress(progressStatusSeries);
                findViewById(R.id.vw_serials).setVisibility(View.VISIBLE);
                handlerSeries.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusSeries < 100) {
                            progressStatusSeries++;
                            pbSerials.setProgress(progressStatusSeries);
                            if (progressStatusSeries == 99){
                                findViewById(R.id.vw_serials).setVisibility(View.GONE);
                                pbSerials.setVisibility(View.GONE);
                            }
                            spHelper.setCurrentDate(Callback.TAG_SERIES);
                            changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, false);
                            handlerSeries.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                Log.e(TAG, "Error chalkedDataSerials",e);
            }
        }
    }

    private void changeIconView(Boolean isDownload, String type, Boolean isView) {
        if (type == null) {
            return;
        }
        switch (type) {
            case "date_tv":
                autoRenewLive(isDownload, isView);
                break;
            case "date_movies":
                autoRenewMovie(isDownload, isView);
                break;
            case "date_series":
                autoRenewSeries(isDownload, isView);
                break;
            default:
                break;
        }
    }

    private void autoRenewLive(Boolean isDownload, Boolean isView) {
        ivAutoRenewLive.setImageResource(Boolean.TRUE.equals(isDownload) ? R.drawable.ic_file_download : R.drawable.ic_repeate);
        tvAutoRenewLive.setText(Boolean.TRUE.equals(isDownload) ? "" : getString(R.string.last_updated)
                + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_TV)));
        if (Boolean.TRUE.equals(isView)){
            int visibility = Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE;
            findViewById(R.id.vw_live_tv).setVisibility(visibility);
            findViewById(R.id.vw_live_epg).setVisibility(visibility);
            findViewById(R.id.vw_catch_up).setVisibility(visibility);
            findViewById(R.id.vw_multiple_screen).setVisibility(visibility);
        }
    }

    private void autoRenewSeries(Boolean isDownload, Boolean isView) {
        ivAutoRenewSeries.setImageResource(Boolean.TRUE.equals(isDownload) ? R.drawable.ic_file_download : R.drawable.ic_repeate);
        tvAutoRenewSeries.setText(Boolean.TRUE.equals(isDownload) ? "" :  getString(R.string.last_updated)
                + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_SERIES)));
        if (Boolean.TRUE.equals(isView)){
            findViewById(R.id.vw_serials).setVisibility(Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE);
        }
    }

    private void autoRenewMovie(Boolean isDownload, Boolean isView) {
        ivAutoRenewMovie.setImageResource(Boolean.TRUE.equals(isDownload) ? R.drawable.ic_file_download : R.drawable.ic_repeate);
        tvAutoRenewMovie.setText(Boolean.TRUE.equals(isDownload) ? "" : getString(R.string.last_updated)
                + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_MOVIE)));
        if (Boolean.TRUE.equals(isView)){
            findViewById(R.id.vw_movie).setVisibility(Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE);
        }
    }

    private void signOut() {
        DialogUtil.logoutDialog(UiBlackPantherActivity.this, () -> {
            Intent intent = new Intent(UiBlackPantherActivity.this, UsersListActivity.class);
            if (spHelper.isLogged()) {
                new JSHelper(this).removeAllData();
                spHelper.removeSignOut();
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
                intent.putExtra("from", "");
                Toast.makeText(UiBlackPantherActivity.this, getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
            } else {
                intent.putExtra("from", "app");
            }
            startActivity(intent);
            finish();
        });
    }

    private boolean isDownloadLive() {
        if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
            return true;
        } else {
            DialogUtil.liveDownloadDialog(this, this::getLive);
            return false;
        }
    }

    private void loadLogin() {
        if (!NetworkUtils.isConnected(this)){
            return;
        }
        LoadLogin login = new LoadLogin(new LoginListener() {
            @Override
            public void onStart() {
                // this method is empty
            }

            @Override
            public void onEnd(String success, ItemLoginUser itemLoginUser,
                              ItemLoginServer itemLoginServer , String allowedOutputFormats) {
                if (!isFinishing() && (success.equals("1"))) {
                    spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
                    spHelper.setIsLogged(true);
                }
            }
        }, spHelper.getServerURLSub(), ApplicationUtil.getAPIRequestLogin(spHelper.getUserName(), spHelper.getPassword()));
        login.execute();
    }

    private void setWifiIcon() {
        ImageView imageView = findViewById(R.id.iv_wifi);
        if (NetworkUtils.isConnected(this)) {
            if (NetworkUtils.isConnectedMobile(this)) {
                imageView.setImageResource(R.drawable.selector_none);
            } else if (NetworkUtils.isConnectedWifi(this)) {
                imageView.setImageResource(R.drawable.ic_wifi);
            } else if (NetworkUtils.isConnectedEthernet(this)) {
                imageView.setImageResource(R.drawable.ic_ethernet);
            }
        } else {
            imageView.setImageResource(R.drawable.ic_wifi_off);
        }
    }

    private void setFormattedDate() {
        TextView appDate = findViewById(R.id.iv_app_date);
        try {
            @SuppressLint("SimpleDateFormat") DateFormat df = new SimpleDateFormat("EEE, d MMM yyyy");
            appDate.setText(df.format(Calendar.getInstance().getTime()));
        } catch (Exception e) {
            Log.e(TAG, "Date formatting error", e);
        }
    }

    private void getSeries() {
        if (!NetworkUtils.isConnected(this)){
            pbSerials.setVisibility(View.GONE);
            Toasty.makeText(UiBlackPantherActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        LoadSeries loadSeries = new LoadSeries(this, new LoadSuccessListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                findViewById(R.id.vw_serials).setVisibility(View.VISIBLE);
                pbSerials.setVisibility(View.VISIBLE);
                progressStatusSeries = 0;
                pbSerials.setProgress(progressStatusSeries);
                handlerSeries.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusSeries < 50) {
                            progressStatusSeries++;
                            pbSerials.setProgress(progressStatusSeries);
                            handlerSeries.postDelayed(this, 20);
                        }
                    }
                }, 20);
            }

            @Override
            public void onEnd(String success, String msg) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                handlerSeriesEnd(success, msg);
            }
        });
        loadSeries.execute();
    }

    private void handlerSeriesEnd(String success, String msg) {
        if (success.equals("1")) {
            handlerSeries.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (progressStatusSeries < 100) {
                        progressStatusSeries++;
                        pbSerials.setProgress(progressStatusSeries);
                        if (progressStatusSeries == 99){
                            findViewById(R.id.vw_serials).setVisibility(View.GONE);
                            pbSerials.setVisibility(View.GONE);
                        }
                        handlerSeries.postDelayed(this, 10);
                    }
                }
            }, 10);
            spHelper.setCurrentDate(Callback.TAG_SERIES);
            changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,false);
            Toast.makeText(UiBlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_SERIES);
            changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,true);
            pbSerials.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiBlackPantherActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiBlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    private void getMovies() {
        if (!NetworkUtils.isConnected(this)){
            pbMovie.setVisibility(View.GONE);
            Toasty.makeText(UiBlackPantherActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
        LoadMovies loadMovies = new LoadMovies(this,  new LoadSuccessListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                findViewById(R.id.vw_movie).setVisibility(View.VISIBLE);
                pbMovie.setVisibility(View.VISIBLE);
                progressStatusMovie = 0;
                pbMovie.setProgress(progressStatusMovie);
                handlerMovie.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusMovie < 50) {
                            progressStatusMovie++;
                            pbMovie.setProgress(progressStatusMovie);
                            handlerMovie.postDelayed(this, 20);
                        }
                    }
                }, 20);
            }

            @Override
            public void onEnd(String success, String msg) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                handlerMovieEnd(success, msg);
            }
        });
        loadMovies.execute();
    }

    private void handlerMovieEnd(String success, String msg) {
        if (success.equals("1")) {
            handlerMovie.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (progressStatusMovie < 100) {
                        progressStatusMovie++;
                        pbMovie.setProgress(progressStatusMovie);
                        if (progressStatusMovie == 99){
                            findViewById(R.id.vw_movie).setVisibility(View.GONE);
                            pbMovie.setVisibility(View.GONE);
                        }
                        handlerMovie.postDelayed(this, 10);
                    }
                }
            }, 10);
            spHelper.setCurrentDate(Callback.TAG_MOVIE);
            changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,false);
            Toast.makeText(UiBlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_MOVIE);
            changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,true);
            pbMovie.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiBlackPantherActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiBlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    private void getLive() {
        if (!NetworkUtils.isConnected(this)){
            pbLive.setVisibility(View.GONE);
            Toasty.makeText(UiBlackPantherActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        LoadChannel loadChannel = new LoadChannel(this, new LoadSuccessListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                findViewById(R.id.vw_live_tv).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_live_epg).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_catch_up).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_multiple_screen).setVisibility(View.VISIBLE);
                pbLive.setVisibility(View.VISIBLE);
                progressStatusLive = 0;
                pbLive.setProgress(progressStatusLive);
                handlerLive.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusLive < 50) {
                            progressStatusLive++;
                            pbLive.setProgress(progressStatusLive);
                            handlerLive.postDelayed(this, 20);
                        }
                    }
                }, 20);
            }

            @Override
            public void onEnd(String success, String msg) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                handlerLiveEnd(success, msg);
            }
        });
        loadChannel.execute();
    }

    private void handlerLiveEnd(String success, String msg) {
        if (success.equals("1")) {
            pbLive.setProgress(progressStatusLive);
            handlerLive.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (progressStatusLive < 100) {
                        progressStatusLive++;
                        pbLive.setProgress(progressStatusLive);
                        if (progressStatusLive == 99){
                            findViewById(R.id.vw_live_tv).setVisibility(View.GONE);
                            findViewById(R.id.vw_live_epg).setVisibility(View.GONE);
                            findViewById(R.id.vw_catch_up).setVisibility(View.GONE);
                            findViewById(R.id.vw_multiple_screen).setVisibility(View.GONE);
                            pbLive.setVisibility(View.GONE);
                        }
                        handlerLive.postDelayed(this, 10);
                    }
                }
            }, 10);
            spHelper.setCurrentDate(Callback.TAG_TV);
            changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
            Toast.makeText(UiBlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_TV);
            changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
            pbLive.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiBlackPantherActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiBlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_black_panther;
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            Log.e(TAG, "Error db close",e);
        }
        super.onDestroy();
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.getIsRecreate())) {
            Callback.setIsRecreate(false);
            recreate();
        }
        super.onResume();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            DialogUtil.exitDialog(UiBlackPantherActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}