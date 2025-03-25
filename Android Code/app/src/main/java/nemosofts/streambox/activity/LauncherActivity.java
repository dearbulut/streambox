package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.nemosofts.EnvatoProduct;
import androidx.nemosofts.LauncherListener;
import androidx.nemosofts.theme.ThemeEngine;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadAbout;
import nemosofts.streambox.executor.LoadData;
import nemosofts.streambox.interfaces.AboutListener;
import nemosofts.streambox.interfaces.DataListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;

public class LauncherActivity extends AppCompatActivity implements LauncherListener {

    Helper helper;
    SPHelper spHelper;
    private ProgressBar pb;
    private ExoPlayer exoPlayer = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_launcher);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        helper = new Helper(this);
        spHelper = new SPHelper(this);

        int theme = spHelper.getIsTheme();
        if (theme == 2){
            findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_ui_glossy);
        } else if (theme == 3){
            findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_dark_panther);
        } else {
            int themePage = new ThemeEngine(this).getThemePage();
            if (themePage == 0){
                findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_dark);
            } else if (themePage == 1){
                findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_classic);
            } else if (themePage == 2){
                findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_grey);
            } else if (themePage == 3){
                findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_blue);
            } else {
                findViewById(R.id.theme_bg).setBackgroundResource(R.drawable.bg_dark);
            }
        }

        pb = findViewById(R.id.pb_splash);

        prepareAudio();
        loadAboutData();
    }

    private void loadAboutData() {
        if (!NetworkUtils.isConnected(this)){
            if (Boolean.TRUE.equals(spHelper.getIsAboutDetails())){
                setSaveData();
            } else {
                errorDialog(getString(R.string.err_internet_not_connected), getString(R.string.err_connect_net_try));
            }
        }

        LoadAbout loadAbout = new LoadAbout(LauncherActivity.this, new AboutListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
            }

            @Override
            public void onEnd(String success, String verifyStatus, String message){
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (success.equals("1")){
                    setSaveData();
                } else {
                    if (Boolean.TRUE.equals(spHelper.getIsAboutDetails())){
                        setSaveData();
                    } else {
                        errorDialog(getString(R.string.err_server_error), getString(R.string.err_server_not_connected));
                    }
                }
            }
        });
        loadAbout.execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void prepareAudio() {
        if (Boolean.TRUE.equals(spHelper.getIsSplashAudio())){
            exoPlayer = new ExoPlayer.Builder(this).build();
            DataSource.Factory dataSourceFactory = new DefaultDataSource.Factory(this,
                    new DefaultHttpDataSource.Factory().setUserAgent(Util.getUserAgent(this, "nemosofts_rc")));
            Uri fileUri = Uri.parse("android.resource://" + getPackageName() + "/" + R.raw.opener_logo);
            MediaSource mediaSource = new ProgressiveMediaSource.Factory(dataSourceFactory).createMediaSource(MediaItem.fromUri(fileUri));
            exoPlayer.setMediaSource(mediaSource);
            exoPlayer.prepare();
            exoPlayer.setPlayWhenReady(false);

            exoPlayer.addListener(new Player.Listener() {

                @Override
                public void onPlaybackStateChanged(int state) {
                    Player.Listener.super.onPlaybackStateChanged(state);
                    if (state == Player.STATE_ENDED){
                        if (isFinishing()){
                            return;
                        }
                        loadSettings();
                    }
                }

                @Override
                public void onPlayerError(@NonNull PlaybackException error) {
                    Player.Listener.super.onPlayerError(error);
                    if (isFinishing()){
                        return;
                    }
                    loadSettings();
                }
            });
        }
    }

    private void playAudio() {
        if (Boolean.FALSE.equals(spHelper.getIsSplashAudio())){
            loadSettings();
        } else if (exoPlayer != null){
            exoPlayer.play();
        } else {
            loadSettings();
        }
    }

    private void loadSettings() {
        if (Boolean.FALSE.equals(spHelper.getIsAboutDetails())){
            spHelper.setAboutDetails(true);
        }
        if (Boolean.TRUE.equals(Callback.getIsAppUpdate()) && Callback.getAppNewVersion() != BuildConfig.VERSION_CODE){
            openDialogActivity(Callback.DIALOG_TYPE_UPDATE);
        } else if(Boolean.TRUE.equals(spHelper.getIsMaintenance())){
            openDialogActivity(Callback.DIALOG_TYPE_MAINTENANCE);
        } else {
            handleLoginType();
        }
    }

    private void handleLoginType() {
        String loginType = spHelper.getLoginType();
        if (Callback.TAG_LOGIN_SINGLE_STREAM.equals(loginType)) {
            openSingleStream();
        } else if (Callback.TAG_LOGIN_VIDEOS.equals(loginType)) {
            openVideos();
        } else if (Callback.TAG_LOGIN_PLAYLIST.equals(loginType)) {
            openPlaylistActivity();
        } else if (isStreamOrOneUILogin(loginType)) {
            handleStreamLogin();
        } else {
            openSelectPlayer();
        }
    }

    private boolean isStreamOrOneUILogin(String loginType) {
        return Callback.TAG_LOGIN_ONE_UI.equals(loginType) || Callback.TAG_LOGIN_STREAM.equals(loginType);
    }

    private void handleStreamLogin() {
        if (Boolean.TRUE.equals(spHelper.getIsFirst()) || Boolean.FALSE.equals(spHelper.getIsAutoLogin())) {
            openSelectPlayer();
        } else {
            getData();
        }
    }

    private void getData() {
        if (isFinishing()){
            return;
        }

        if (!NetworkUtils.isConnected(this)){
            ApplicationUtil.openThemeActivity(LauncherActivity.this);
            return;
        }

        LoadData loadData = new LoadData(LauncherActivity.this, new DataListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
            }

            @Override
            public void onEnd(String success) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                ApplicationUtil.openThemeActivity(LauncherActivity.this);
            }
        });
        loadData.execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayer() {
        Intent intent = new Intent(LauncherActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSingleStream() {
        Intent intent = new Intent(LauncherActivity.this, SingleStreamActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void openVideos() {
        Intent intent = new Intent(LauncherActivity.this, LocalStorageActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void openPlaylistActivity() {
        Intent intent = new Intent(LauncherActivity.this, PlaylistActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void openDialogActivity(String type) {
        Intent intent = new Intent(LauncherActivity.this, DialogActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", type);
        startActivity(intent);
        finish();
    }

    @Override
    public void onStartPairing() {
        pb.setVisibility(View.VISIBLE);
    }

    @Override
    public void onConnected() {
        pb.setVisibility(View.GONE);
        playAudio();
    }

    @Override
    public void onUnauthorized(String message) {
        pb.setVisibility(View.GONE);
        errorDialog(getString(R.string.err_unauthorized_access), message);
    }

    @Override
    public void onError() {
        pb.setVisibility(View.GONE);
        errorDialog(getString(R.string.err_server_error), getString(R.string.err_server_not_connected));
    }

    private void errorDialog(String title, String message) {
        if (isFinishing()) {
            // Activity is finishing, no need to show dialog
            return;
        }

        runOnUiThread(() -> {
            final AlertDialog.Builder alertDialog = new AlertDialog.Builder(LauncherActivity.this, R.style.ThemeDialog);
            alertDialog.setTitle(title);
            alertDialog.setMessage(message);
            alertDialog.setCancelable(false);

            if (title.equals(getString(R.string.err_internet_not_connected))) {
                alertDialog.setNegativeButton(getString(R.string.retry), (dialog, which) -> loadSettings());
            }
            alertDialog.setPositiveButton(getString(R.string.exit), (dialog, which) -> finish());
            alertDialog.show();
        });
    }

    private void setSaveData() {
        new EnvatoProduct(LauncherActivity.this, LauncherActivity.this).execute();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            finish();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}