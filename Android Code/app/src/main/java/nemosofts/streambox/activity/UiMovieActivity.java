package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.ColorStateList;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
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
import androidx.palette.graphics.Palette;

import com.onesignal.Continue;
import com.onesignal.OneSignal;
import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.callback.Method;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadChannel;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.executor.LoadMovies;
import nemosofts.streambox.executor.LoadPoster;
import nemosofts.streambox.executor.LoadSeries;
import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.interfaces.PosterListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.item.ItemPoster;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.advertising.AdManagerInterAdmob;
import nemosofts.streambox.util.advertising.GDPRChecker;
import nemosofts.streambox.util.advertising.RewardAdAdmob;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class UiMovieActivity extends AppCompatActivity {

    private static final String TAG = "MovieUIActivity";
    private Helper helper;
    private SPHelper spHelper;
    private ProgressDialog progressDialog;
    private ProgressBar pbLive;
    private ProgressBar pbMovie;
    private ProgressBar pbSerials;
    private final Handler handlerLive = new Handler(Looper.getMainLooper());
    private final Handler handlerMovie = new Handler(Looper.getMainLooper());
    private final Handler handlerSeries = new Handler(Looper.getMainLooper());
    private int progressStatusLive = 0;
    private int progressStatusMovie = 0;
    private int progressStatusSeries = 0;
    private int themeBg;
    private View vwLive;
    private View vwMovies;
    private View vwSeries;
    private View vwEpg;
    private View vwMultiple;
    private View vwCatchUp;
    private Boolean vibrant = true;
    private final Handler handler = new Handler(Looper.getMainLooper());
    private Runnable runnableCode;

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
                DialogUtil.exitDialog(UiMovieActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        themeBg = ApplicationUtil.openThemeBg(this);

        helper = new Helper(this);
        spHelper = new SPHelper(this);

        progressDialog = new ProgressDialog(UiMovieActivity.this, true);

        ImageView blur = findViewById(R.id.iv_bg_blur);
        blur.setImageResource(themeBg);

        getPoster();

        vwLive = findViewById(R.id.vw_live_bg);
        vwMovies = findViewById(R.id.vw_movies_bg);
        vwSeries = findViewById(R.id.vw_series_bg);
        vwEpg = findViewById(R.id.vw_epg_bg);
        vwMultiple = findViewById(R.id.vw_multiple_bg);
        vwCatchUp = findViewById(R.id.vw_catch_up_bg);

        pbLive = findViewById(R.id.pb_live_tv);
        pbMovie = findViewById(R.id.pb_movie);
        pbSerials = findViewById(R.id.pb_serials);

        setWifiIcon();
        setFormattedDate();
        setListenerHome();

        changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
        changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, true);
        changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, true);

        loadLogin();
        chalkedDataLive();
        chalkedDataMovie();
        chalkedDataSeries();
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.select_live).requestFocus();
        }

        runnableCode = () -> {
            if (!Callback.getArrayListPoster().isEmpty()){
                if (Callback.getPosterPos() < (Callback.getArrayListPoster().size() - 1)) {
                    Callback.setPosterPos(Callback.getPosterPos() + 1);
                } else {
                    Callback.setPosterPos(0);
                }
                loadPoster();
                handler.postDelayed(runnableCode, (long) 20 * 1000);
            } else {
                handler.removeCallbacks(runnableCode);
            }
        };

        setAds();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isFinishing()){
                return;
            }
            DialogUtil.popupAdsDialog(UiMovieActivity.this);
        }, 600);
    }

    private void setAds() {
        if (ApplicationUtil.isTvBox(this)){
            return;
        }
        new GDPRChecker(UiMovieActivity.this).check();
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

    private void setWifiIcon() {
        ImageView imageView = findViewById(R.id.iv_wifi);
        if (!NetworkUtils.isConnected(this)) {
            imageView.setImageResource(R.drawable.ic_wifi_off);
            return;
        }
        if (NetworkUtils.isConnectedMobile(this)) {
            imageView.setImageResource(R.drawable.selector_none);
        } else if (NetworkUtils.isConnectedWifi(this)) {
            imageView.setImageResource(R.drawable.ic_wifi);
        } else if (NetworkUtils.isConnectedEthernet(this)) {
            imageView.setImageResource(R.drawable.ic_ethernet);
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

    private void getPoster() {
        if (!NetworkUtils.isConnected(this)){
            return;
        }
        LoadPoster loadPoster = new LoadPoster(new PosterListener() {
            @Override
            public void onStart() {
                findViewById(R.id.pb).setVisibility(View.VISIBLE);
            }

            @Override
            public void onEnd(String success, String verifyStatus, String message, ArrayList<ItemPoster> arrayList) {
                if (isFinishing()){
                    return;
                }
                findViewById(R.id.pb).setVisibility(View.GONE);
                if (success.equals("1") && (!arrayList.isEmpty())){
                    if (!Callback.getArrayListPoster().isEmpty()){
                        Callback.getArrayListPoster().clear();
                    }
                    Callback.setArrayListPoster(arrayList);
                    handler.postDelayed(runnableCode, (long) 20 * 1000);
                    loadPoster();
                }
            }
        }, helper.getAPIRequestNSofts(Method.METHOD_POSTER, "", "", "", ""));
        loadPoster.execute();
    }

    private void loadPoster() {
        ImageView poster = findViewById(R.id.iv_bg_poster);
        findViewById(R.id.pb).setVisibility(View.VISIBLE);
        Picasso.get()
                .load(Callback.getArrayListPoster().get(Callback.getPosterPos()).getPoster())
                .placeholder(themeBg)
                .error(themeBg)
                .into(poster, new com.squareup.picasso.Callback() {
                    @Override
                    public void onSuccess() {
                        loadVibrantColor();
                        findViewById(R.id.pb).setVisibility(View.GONE);
                    }

                    @Override
                    public void onError(Exception e) {
                        findViewById(R.id.pb).setVisibility(View.GONE);
                    }
                });
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setListenerHome() {
        if (!spHelper.getIsDownload()){
            findViewById(R.id.iv_file_download).setVisibility(View.GONE);
        }
        if (Boolean.FALSE.equals(spHelper.getIsRadio())){
            findViewById(R.id.iv_radio).setVisibility(View.GONE);
        }
        findViewById(R.id.iv_notifications).setOnClickListener(v -> startActivity(new Intent(UiMovieActivity.this, NotificationsActivity.class)));
        findViewById(R.id.iv_file_download).setOnClickListener(v -> startActivity(new Intent(UiMovieActivity.this, DownloadActivity.class)));
        findViewById(R.id.iv_profile).setOnClickListener(v -> startActivity(new Intent(UiMovieActivity.this, ProfileActivity.class)));
        findViewById(R.id.iv_profile_re).setOnClickListener(v ->  signOut());
        findViewById(R.id.iv_settings).setOnClickListener(v -> startActivity(new Intent(UiMovieActivity.this, SettingActivity.class)));
        findViewById(R.id.select_live).setOnClickListener(v -> selectLive());
        findViewById(R.id.select_movie).setOnClickListener(v -> selectMovie());
        findViewById(R.id.select_serials).setOnClickListener(v -> selectSerials());
        findViewById(R.id.select_epg).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiMovieActivity.this, CategoriesActivity.class));
            }
        });
        findViewById(R.id.select_multiple_screen).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiMovieActivity.this, MultipleScreenActivity.class));
            }
        });
        findViewById(R.id.select_catch_up).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiMovieActivity.this, CatchUpActivity.class));
            }
        });
        findViewById(R.id.iv_radio).setOnClickListener(v -> {
            if (isDownloadLive()) {
                startActivity(new Intent(UiMovieActivity.this, RadioActivity.class));
            }
        });
        setLongClickListener();
    }

    private void setLongClickListener() {
        findViewById(R.id.select_live).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
                DialogUtil.downloadDataDialog(UiMovieActivity.this, Callback.TAG_TV, type -> getLive());
            }
            return false;
        });
        findViewById(R.id.select_movie).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                DialogUtil.downloadDataDialog(UiMovieActivity.this, Callback.TAG_TV, type -> getMovies());
            }
            return false;
        });
        findViewById(R.id.select_serials).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                DialogUtil.downloadDataDialog(UiMovieActivity.this, Callback.TAG_TV, type -> getSeries());
            }
            return false;
        });
    }

    private void selectSerials() {
        if (spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
            getSeries();
        } else {
            startActivity(new Intent(UiMovieActivity.this, SeriesActivity.class));
        }
    }

    private void selectMovie() {
        if (spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
            getMovies();
        } else {
            startActivity(new Intent(UiMovieActivity.this, MovieActivity.class));
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void selectLive() {
        if (spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
            getLive();
        } else {
            startActivity(new Intent(UiMovieActivity.this, LiveTvActivity.class));
        }
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
                              ItemLoginServer itemLoginServer, String allowedOutputFormats) {
                if (!isFinishing() && (success.equals("1"))) {
                    spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
                    spHelper.setIsLogged(true);
                }
            }
        }, spHelper.getServerURLSub(), ApplicationUtil.getAPIRequestLogin(spHelper.getUserName(), spHelper.getPassword()));
        login.execute();
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

    private void chalkedDataSeries() {
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
                Log.e(TAG, "Error chalkedDataSeries",e);
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
        if (Boolean.TRUE.equals(isView)){
            int visibility = Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE;
            findViewById(R.id.vw_live_tv).setVisibility(visibility);
            findViewById(R.id.vw_live_epg).setVisibility(visibility);
            findViewById(R.id.vw_catch_up).setVisibility(visibility);
            findViewById(R.id.vw_multiple_screen).setVisibility(visibility);
        }
    }

    private void autoRenewMovie(Boolean isDownload, Boolean isView) {
        if (Boolean.TRUE.equals(isView)){
            findViewById(R.id.vw_movie).setVisibility(Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE);
        }
    }

    private void autoRenewSeries(Boolean isDownload, Boolean isView) {
        if (Boolean.TRUE.equals(isView)){
            findViewById(R.id.vw_serials).setVisibility(Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE);
        }
    }

    private void signOut() {
        DialogUtil.logoutDialog(UiMovieActivity.this, () -> {
            Intent intent = new Intent(UiMovieActivity.this, UsersListActivity.class);
            if (spHelper.isLogged()) {
                new JSHelper(this).removeAllData();
                spHelper.removeSignOut();
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
                intent.putExtra("from", "");
                Toast.makeText(UiMovieActivity.this, getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
            } else {
                intent.putExtra("from", "app");
            }
            startActivity(intent);
            finish();
        });
    }

    private void loadVibrantColor() {
        try {
            Picasso.get()
                    .load(Callback.getArrayListPoster().get(Callback.getPosterPos()).getPoster())
                    .centerCrop()
                    .resize(200,200)
                    .into(new Target() {
                        @Override
                        public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                            try {
                                Palette.from(bitmap).generate(palette -> {
                                    if (palette != null){
                                        try{
                                            int defaultValue = 0x000000;
                                            int vibrant = palette.getDarkVibrantColor(defaultValue);
                                            vwLive.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vwMovies.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vwSeries.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vwEpg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vwMultiple.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vwCatchUp.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                        } catch (Exception e) {
                                            e.printStackTrace();
                                        }
                                    }
                                });
                            } catch (Exception e) {
                                Log.e(TAG, "Error Palette",e);
                            }
                        }

                        @Override
                        public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                            if (Boolean.TRUE.equals(vibrant)){
                                vibrant = false;
                                loadVibrantColor();
                            }
                        }

                        @Override
                        public void onPrepareLoad(Drawable placeHolderDrawable) {
                            // document why this method is empty
                        }
                    });
        } catch (Exception e) {
            Log.e(TAG, "Error loadVibrantColor",e);
        }
    }

    private void getSeries() {
        if (!NetworkUtils.isConnected(this)){
            pbSerials.setVisibility(View.GONE);
            Toasty.makeText(UiMovieActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
            Toast.makeText(UiMovieActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_SERIES);
            changeIconView(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,true);
            pbSerials.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiMovieActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiMovieActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    private void getMovies() {
        if (!NetworkUtils.isConnected(this)){
            pbMovie.setVisibility(View.GONE);
            Toasty.makeText(UiMovieActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
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
            Toast.makeText(UiMovieActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_MOVIE);
            changeIconView(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,true);
            pbMovie.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiMovieActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiMovieActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    private void getLive() {
        if (!NetworkUtils.isConnected(this)){
            pbLive.setVisibility(View.GONE);
            Toasty.makeText(UiMovieActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
            Toast.makeText(UiMovieActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
        }  else {
            spHelper.setCurrentDateEmpty(Callback.TAG_TV);
            changeIconView(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
            pbLive.setVisibility(View.GONE);
            if (success.equals("3")){
                Toasty.makeText(UiMovieActivity.this,true, msg, Toasty.ERROR);
            } else {
                Toast.makeText(UiMovieActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_movie;
    }

    @Override
    protected void onResume() {
        super.onResume();
        handler.post(runnableCode);
        if (Boolean.TRUE.equals(Callback.getIsRecreate())) {
            Callback.setIsRecreate(false);
            recreate();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        handler.removeCallbacks(runnableCode);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        handler.removeCallbacks(runnableCode);
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            DialogUtil.exitDialog(UiMovieActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}