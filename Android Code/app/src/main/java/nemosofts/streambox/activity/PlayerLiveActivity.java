package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ActivityInfo;
import android.graphics.Color;
import android.graphics.Typeface;
import android.media.AudioManager;
import android.media.audiofx.AudioEffect;
import android.media.audiofx.LoudnessEnhancer;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.accessibility.CaptioningManager;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.dash.DashMediaSource;
import androidx.media3.exoplayer.dash.DefaultDashChunkSource;
import androidx.media3.exoplayer.hls.HlsMediaSource;
import androidx.media3.exoplayer.rtsp.RtspMediaSource;
import androidx.media3.exoplayer.smoothstreaming.DefaultSsChunkSource;
import androidx.media3.exoplayer.smoothstreaming.SsMediaSource;
import androidx.media3.exoplayer.source.DefaultMediaSourceFactory;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.exoplayer.trackselection.DefaultTrackSelector;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.session.MediaSession;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.CaptionStyleCompat;
import androidx.media3.ui.PlayerView;
import androidx.media3.ui.SubtitleView;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.dialog.PlayerChannelList;
import nemosofts.streambox.executor.LoadEpg;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.BrightnessVolumeControl;
import nemosofts.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerLiveActivity extends AppCompatActivity {

    private static final String TAG = "PlayerLiveActivity";
    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;

    private PlayerListener playerListener;
    private MediaSession mediaSession;
    private LoudnessEnhancer loudnessEnhancer;
    private BroadcastReceiver batteryReceiver;
    private AudioManager mAudioManager;
    private int boostLevel = 0;
    private boolean isTvBox;
    private int resize = 1;

    private CustomPlayerView playerView;
    private ExoPlayer exoPlayer;
    private DefaultBandwidthMeter bandwidthMeter;
    private DataSource.Factory mediaDataSourceFactory;
    private ProgressBar loadingProgressBar;

    private int playback = 0;

    private ImageView btnPlay;
    private ImageView btnFav;
    private ImageView btnTryAgain;
    private TextView playerTitle;
    private PlayerChannelList listDialog;
    private RelativeLayout rlEpg;
    private TextView epgTitle;
    private TextView epgTime;
    private ImageView previous;
    private ImageView next;

    // RewardAd
    private CountDownTimer countDownTimer;
    private long timeLeftInMillis;

    private boolean controllerVisible;
    private boolean controllerVisibleFully;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideBottomBar(this);
        IfSupported.statusBarBlackColor(this);
        IfSupported.setNavigationBarColor(this);
        isTvBox = ApplicationUtil.isTvBox(this);

        timeLeftInMillis = Callback.getRewardMinutes() * 60 * 1000; // minutes in milliseconds

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        listDialog = new PlayerChannelList(this, position -> {
            Callback.setPlayPosLive(position);
            setMediaSource();
        });

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (listDialog != null && listDialog.isShowing()) {
                    listDialog.dismissDialog();
                } else {
                    finish();
                }
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        playerTitle = findViewById(R.id.tv_player_title);
        loadingProgressBar = findViewById(R.id.pb_player);
        btnTryAgain = findViewById(R.id.iv_reset);
        btnPlay = findViewById(R.id.iv_play);
        btnFav = findViewById(R.id.iv_player_fav);

        rlEpg = findViewById(R.id.rl_player_epg);
        epgTitle = findViewById(R.id.tv_epg_title);
        epgTime = findViewById(R.id.tv_epg_time);

        previous = findViewById(R.id.iv_previous);
        next = findViewById(R.id.iv_next);

        bandwidthMeter = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);

        // Set default cookie manager if not already set
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        // https://github.com/google/ExoPlayer/issues/8571
        DefaultExtractorsFactory extractorsFactory = ApplicationUtil.getDefaultExtractorsFactory();
        DefaultRenderersFactory renderersFactory = ApplicationUtil.getDefaultRenderersFactory(this, spHelper.isHardwareDecoding());

        DefaultTrackSelector trackSelector = new DefaultTrackSelector(this);
        // Set captioning parameters if enabled
        final CaptioningManager captioningManager = (CaptioningManager) getSystemService(Context.CAPTIONING_SERVICE);
        if (!captioningManager.isEnabled()) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setIgnoredTextSelectionFlags(C.SELECTION_FLAG_DEFAULT));
        }
        Locale locale = captioningManager.getLocale();
        if (locale != null) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setPreferredTextLanguage(locale.getISO3Language()));
        }

        mAudioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

        // Build ExoPlayer instance
        exoPlayer = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(trackSelector)
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        // Set audio attributes for the player
        AudioAttributes audioAttributes = new AudioAttributes.Builder()
                .setUsage(C.USAGE_MEDIA)
                .setContentType(C.AUDIO_CONTENT_TYPE_MOVIE)
                .build();

        exoPlayer.setAudioAttributes(audioAttributes, true);
        exoPlayer.setHandleAudioBecomingNoisy(!isTvBox);
        setMediaSession();

        // Attach ExoPlayer to the player view
        playerView = findViewById(R.id.nSoftsPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setUseController(true);
        playerView.requestFocus();
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);
        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            controllerVisible = visibility == View.VISIBLE;
            controllerVisibleFully = playerView.isControllerFullyVisible();

            // https://developer.android.com/training/system-ui/immersive
            IfSupported.toggleSystemUi(PlayerLiveActivity.this, playerView, visibility == View.VISIBLE);
            if (isTvBox && visibility == View.VISIBLE) {
                // Because when using dpad controls, focus resets to first item in bottom controls bar
                btnPlay.requestFocus();
            }
        });

        setCustomSubtitle();
        setMediaSource();

        // Set player event listeners
        playerListener = new PlayerListener();
        exoPlayer.addListener(playerListener);

        btnTryAgain.setOnClickListener(v -> {
            btnTryAgain.setVisibility(View.GONE);
            previous.setVisibility(View.VISIBLE);
            next.setVisibility(View.VISIBLE);
            loadingProgressBar.setVisibility(View.VISIBLE);
            setMediaSource();
        });

        btnPlay.setOnClickListener(v -> {
            exoPlayer.setPlayWhenReady(!exoPlayer.getPlayWhenReady());
            btnPlay.setImageResource(Boolean.TRUE.equals(exoPlayer.getPlayWhenReady()) ? R.drawable.ic_pause : R.drawable.ic_play);
        });
        if (isTvBox){
            btnPlay.requestFocus();
        }

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            btnFav.setVisibility(View.INVISIBLE);
        }
        btnFav.setOnClickListener(v -> setFav());
        previous.setOnClickListener(v -> previous());
        next.setOnClickListener(v -> next());
        findViewById(R.id.ll_multiple_screen).setOnClickListener(v -> openMultipleScreenActivity());

        setBatteryInfo();
        findViewById(R.id.ll_aspect_ratio).setOnClickListener(v -> setResize());
        findViewById(R.id.iv_media_info).setOnClickListener(v -> getPlayerInfo());
        findViewById(R.id.iv_back_player).setOnClickListener(v -> finish());
        if (isTvBox){
            findViewById(R.id.iv_back_player).setVisibility(View.GONE);
            AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
        }
    }

    private void openMultipleScreenActivity() {
        Intent intent = new Intent(PlayerLiveActivity.this, MultipleScreenActivity.class);
        intent.putExtra("is_player", true);
        intent.putExtra("stream_id", Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamIcon());
        startActivity(intent);
        finish();
    }

    private void setFav() {
        if (Boolean.TRUE.equals(dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID()))){
            dbHelper.removeLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID());
            btnFav.setImageResource(R.drawable.ic_favorite_border);
            Toast.makeText(PlayerLiveActivity.this, getString(R.string.fav_remove_success), Toast.LENGTH_SHORT).show();
        } else {
            dbHelper.addToLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()), 0);
            btnFav.setImageResource(R.drawable.ic_favorite);
            Toast.makeText(PlayerLiveActivity.this, getString(R.string.fav_success), Toast.LENGTH_SHORT).show();
        }
    }

    private void setMediaSession() {
        if (mediaSession != null) {
            mediaSession.release();
        }
        if (exoPlayer.canAdvertiseSession()) {
            try {
                mediaSession = new MediaSession.Builder(this, exoPlayer).build();
            } catch (IllegalStateException e) {
                Log.e(TAG, "Failed to create MediaSession", e);
            }
        }
    }

    private void getPlayerInfo() {
        if (exoPlayer != null && exoPlayer.getPlayWhenReady() && exoPlayer.getVideoFormat() != null){
            playerView.hideController();
            DialogUtil.dialogPlayerInfo(this, exoPlayer, true);
        } else {
            Toasty.makeText(this,true, getString(R.string.please_wait_a_minute), Toasty.ERROR);
        }
    }

    private void setBatteryInfo() {
        ImageView batteryInfo = findViewById(R.id.iv_battery_info);
        if (!isTvBox){
            batteryReceiver = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS, -1);
                    int level = intent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
                    int scale = intent.getIntExtra(BatteryManager.EXTRA_SCALE, -1);
                    batteryInfo.setImageResource(ApplicationUtil.getBatteryDrawable(status,level,scale));
                }
            };
            IntentFilter filter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED);
            registerReceiver(batteryReceiver, filter);
        } else {
            batteryInfo.setVisibility(View.INVISIBLE);
        }
    }

    private void setCustomSubtitle() {
        // Set custom subtitle view style
        try {
            final CaptioningManager mCaptioningManager = (CaptioningManager) getSystemService(Context.CAPTIONING_SERVICE);
            if (mCaptioningManager == null || playerView == null){
                return;
            }
            final SubtitleView subtitleView = playerView.getSubtitleView();
            final boolean isTablet = ApplicationUtil.isTablet(this);
            float subtitlesScale = ApplicationUtil.normalizeFontScale(mCaptioningManager.getFontScale(), isTvBox || isTablet);
            if (subtitleView == null) {
                return;
            }
            final CaptioningManager.CaptionStyle userStyle = mCaptioningManager.getUserStyle();
            final CaptionStyleCompat userStyleCompat = CaptionStyleCompat.createFromCaptionStyle(userStyle);
            final CaptionStyleCompat captionStyle = new CaptionStyleCompat(
                    userStyle.hasForegroundColor() ? userStyleCompat.foregroundColor : Color.WHITE,
                    userStyle.hasBackgroundColor() ? userStyleCompat.backgroundColor : Color.TRANSPARENT,
                    userStyle.hasWindowColor() ? userStyleCompat.windowColor : Color.TRANSPARENT,
                    userStyle.hasEdgeType() ? userStyleCompat.edgeType : CaptionStyleCompat.EDGE_TYPE_OUTLINE,
                    userStyle.hasEdgeColor() ? userStyleCompat.edgeColor : Color.BLACK,
                    Typeface.create(userStyleCompat.typeface != null ? userStyleCompat.typeface : Typeface.DEFAULT, Typeface.NORMAL));
            subtitleView.setStyle(captionStyle);
            subtitleView.setApplyEmbeddedStyles(false);
            subtitleView.setBottomPaddingFraction(SubtitleView.DEFAULT_BOTTOM_PADDING_FRACTION * 2f / 3f);

            // Tweak text size as fraction size doesn't work well in portrait
            final float size = ApplicationUtil.getaFloat(this, subtitlesScale);
            subtitleView.setFractionalTextSize(size);
        } catch (Exception e) {
            Log.e(TAG, "setCustomSubtitle: ", e);
        }
    }

    private class PlayerListener implements Player.Listener {

        @Override
        public void onAudioSessionIdChanged(int audioSessionId) {
            Player.Listener.super.onAudioSessionIdChanged(audioSessionId);
            try {
                if (loudnessEnhancer != null) {
                    loudnessEnhancer.release();
                }
                loudnessEnhancer = new LoudnessEnhancer(audioSessionId);
            } catch (Exception e) {
                Log.e(TAG, "onAudioSessionIdChanged: ", e);
            }
            notifyAudioSessionUpdate(true);
        }

        @Override
        public void onIsPlayingChanged(boolean isPlaying) {
            Player.Listener.super.onIsPlayingChanged(isPlaying);
            playerView.setKeepScreenOn(isPlaying);
        }

        @Override
        public void onPlaybackStateChanged(int state) {
            Player.Listener.super.onPlaybackStateChanged(state);
            if (state == PlaybackStateEvent.STATE_PLAYING) {
                btnPlay.setImageResource(R.drawable.ic_pause);
                loadingProgressBar.setVisibility(View.GONE);
                playback = 1;
                startTimer();
            } else if (state == Player.STATE_BUFFERING) {
                loadingProgressBar.setVisibility(View.VISIBLE);
            }
        }
        @Override
        public void onPlayerError(@NonNull PlaybackException error) {
            Player.Listener.super.onPlayerError(error);
            if (isFinishing()){
                return;
            }
            if (playback < 5){
                playback = playback + 1;
                Toast.makeText(PlayerLiveActivity.this,"Playback error - "+ playback + "/5 ", Toast.LENGTH_SHORT).show();
                new Handler(Looper.getMainLooper()).postDelayed(() -> {
                    if (isFinishing()){
                        return;
                    }
                    setMediaSource();
                }, 600);
            } else {
                playback = 1;
                exoPlayer.stop();
                btnTryAgain.setVisibility(View.VISIBLE);
                previous.setVisibility(View.GONE);
                next.setVisibility(View.GONE);
                btnPlay.setImageResource(R.drawable.ic_play);
                btnPlay.setVisibility(View.GONE);
                loadingProgressBar.setVisibility(View.GONE);
                Toast.makeText(PlayerLiveActivity.this,"Failed : " + error.getErrorCodeName(), Toast.LENGTH_SHORT).show();
            }
        }
    }

    public void startTimer() {
        if (!isFinishing() && !isTvBox && countDownTimer == null && Boolean.TRUE.equals(Callback.getRewardAdLive())){
            countDownTimer = new CountDownTimer(timeLeftInMillis, 1000) {

                @Override
                public void onTick(long millisUntilFinished) {
                    timeLeftInMillis = millisUntilFinished;
                }

                @Override
                public void onFinish() {
                    if (isFinishing()){
                        return;
                    }
                    helper.showRewardAds(Callback.getRewardAdLive(),exoPlayer != null && exoPlayer.isPlaying(), playWhenReady1 -> {

                    });
                }
            }.start();
        }
    }

    void notifyAudioSessionUpdate(final boolean active) {
        final Intent intent = new Intent(active
                ? AudioEffect.ACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION
                : AudioEffect.ACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION
        );
        intent.putExtra(AudioEffect.EXTRA_AUDIO_SESSION, exoPlayer.getAudioSessionId());
        intent.putExtra(AudioEffect.EXTRA_PACKAGE_NAME, getPackageName());
        if (active) {
            intent.putExtra(AudioEffect.EXTRA_CONTENT_TYPE, AudioEffect.CONTENT_TYPE_MOVIE);
        }
        try {
            sendBroadcast(intent);
        } catch (SecurityException e) {
            Log.e(TAG, "notifyAudioSessionUpdate: ", e);
        }
    }

    private void handlePreviousNextBtn() {
        int currentPos = Callback.getPlayPosLive();
        int lastPos = Callback.getArrayListLive().size() - 1;
        previous.setVisibility(currentPos > 0 ? View.VISIBLE : View.INVISIBLE);
        next.setVisibility(currentPos < lastPos ? View.VISIBLE : View.INVISIBLE);
    }

    public void next() {
        if (validatePlaybackConditions()){
            return;
        }
        if (Callback.getPlayPosLive() < (Callback.getArrayListLive().size() - 1)) {
            Callback.setPlayPosLive(Callback.getPlayPosLive() + 1);
        } else {
            Callback.setPlayPosLive(0);
        }

        recreate();
    }

    public void previous() {
        if (validatePlaybackConditions()){
            return;
        }
        if (Callback.getPlayPosLive() > 0) {
            Callback.setPlayPosLive(Callback.getPlayPosLive() - 1);
        } else {
            Callback.setPlayPosLive(Callback.getArrayListLive().size() - 1);
        }

        recreate();
    }

    private void setMediaSource() {
        if (validatePlaybackConditions()){
            return;
        }

        findViewById(R.id.ll_channels_list).setOnClickListener(view -> {
            playerView.hideController();
            listDialog.showDialog();
        });

        if (btnTryAgain.getVisibility() == View.VISIBLE){
            btnTryAgain.setVisibility(View.GONE);
            previous.setVisibility(View.VISIBLE);
            next.setVisibility(View.VISIBLE);
        }

        playerTitle.setText(Callback.getArrayListLive().get(Callback.getPlayPosLive()).getName());
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            btnFav.setImageResource(Boolean.TRUE.equals(dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE,
                    Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID())) ? R.drawable.ic_favorite : R.drawable.ic_favorite_border);
        }

        Uri uri = Uri.parse(getChannelUrl());
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);

        try {
            if (loudnessEnhancer != null) {
                loudnessEnhancer.release();
            }
            loudnessEnhancer = new LoudnessEnhancer(exoPlayer.getAudioSessionId());
        } catch (Exception e) {
            Log.e(TAG, "setMediaSource: ", e);
        }
        notifyAudioSessionUpdate(true);

        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);
        btnPlay.setImageResource(R.drawable.ic_pause);
        btnPlay.setVisibility(View.VISIBLE);

        handlePreviousNextBtn();

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            try {
                dbHelper.addToLive(DBHelper.TABLE_RECENT_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()), spHelper.getLiveLimit());
            } catch (Exception e) {
                Log.e(TAG, "setMediaSource add to live: ", e);
            }
        }
    }

    private boolean validatePlaybackConditions() {
        if (Callback.getArrayListLive().isEmpty()) {
            Toasty.makeText(PlayerLiveActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
            return true;
        }
        if (!NetworkUtils.isConnected(this)) {
            Toasty.makeText(PlayerLiveActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return true;
        }
        return false;
    }

    private String getChannelUrl() {
        String channelUrl;
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            channelUrl = Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID();
            rlEpg.setVisibility(View.GONE);
        } else {
            String format = ".m3u8";
            if (spHelper.getLiveFormat() == 1){
                format = ".ts";
            }
            if (Boolean.TRUE.equals(spHelper.getIsXuiUser())){
                channelUrl = spHelper.getServerURL()+ spHelper.getUserName()+"/"
                        + spHelper.getPassword()+"/"+Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID()+format;
            } else {
                channelUrl = spHelper.getServerURL()+"live/"+ spHelper.getUserName()+"/"
                        + spHelper.getPassword()+"/"+Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID()+format;
            }
            if (playback == 0 || playback == 1){
                getEpgData();
            }
        }
        return channelUrl;
    }

    private void getEpgData() {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    rlEpg.setVisibility(View.GONE);
                    epgTitle.setText("");
                    epgTime.setText("");
                }

                @SuppressLint("SetTextI18n")
                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (!isFinishing() && !epgArrayList.isEmpty()){
                        epgTitle.setText(ApplicationUtil.decodeBase64(epgArrayList.get(0).getTitle()));
                        epgTime.setText(ApplicationUtil.getTimestamp(epgArrayList.get(0).getStartTimestamp(),
                                spHelper.getIs12Format()) + " - "
                                + ApplicationUtil.getTimestamp(epgArrayList.get(0).getStopTimestamp(), spHelper.getIs12Format()));
                        rlEpg.setVisibility(View.VISIBLE);
                    }
                }
            }, ApplicationUtil.getAPIRequestID("get_short_epg","stream_id",
                    Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        }
    }

    @SuppressLint("SwitchIntDef")
    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int type = Util.inferContentType(uri);
        MediaItem mediaItem = MediaItem.fromUri(uri);
        switch (type) {
            case C.CONTENT_TYPE_SS -> {
                // For SmoothStreaming (SS)
                return new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_DASH -> {
                // For Dynamic Adaptive Streaming over HTTP (DASH)
                return new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_HLS -> {
                // For HTTP Live Streaming (HLS)
                return new HlsMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_RTSP -> {
                // For Real-Time Streaming Protocol (RTSP)
                return new RtspMediaSource.Factory().createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_OTHER -> {
                // For Progressive Media
                return new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
            default -> {
                return new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
        }
    }

    private DataSource.Factory buildDataSourceFactory(boolean useBandwidthMeter) {
        return buildDataSourceFactory(useBandwidthMeter ? bandwidthMeter : null);
    }

    public DataSource.Factory buildDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        HttpDataSource.Factory httpDataSourceFactory = buildHttpDataSourceFactory(bandwidthMeter);
        return new DefaultDataSource.Factory(this, httpDataSourceFactory);
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        CookieManager cookieManager = new CookieManager();
        cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
        CookieHandler.setDefault(cookieManager);
        return new DefaultHttpDataSource.Factory().setUserAgent(spHelper.getAgentName().isEmpty()
                        ? Util.getUserAgent(PlayerLiveActivity.this, "ExoPlayerDemo")
                        : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    private void setResize() {
        if (resize == 1){
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FILL);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            resize = 2;
        } else if (resize == 2){
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_ZOOM);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            resize = 3;
        } else if (resize == 3){
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            resize = 2;
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_player_live;
    }

    @Override
    public void onStop() {
        super.onStop();
        playWhenReady(false);
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public void onPause() {
        super.onPause();
        playWhenReady(false);
    }

    @Override
    public void onResume() {
        super.onResume();
        playWhenReady(true);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        playWhenReady(true);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (countDownTimer != null) {
            countDownTimer.cancel();
        }
        releasePlayer();
    }

    private void releasePlayer() {
        try {
            if (batteryReceiver != null){
                unregisterReceiver(batteryReceiver);
            }
            if (exoPlayer != null) {
                notifyAudioSessionUpdate(false);

                if (mediaSession != null) {
                    mediaSession.release();
                }

                exoPlayer.removeListener(playerListener);
                exoPlayer.clearMediaItems();
                exoPlayer.release();
            }
        } catch (Exception e) {
           Log.e(TAG, "releasePlayer: ", e);
        }
    }

    private void playWhenReady(boolean setPlayWhenReady) {
        try {
            if (exoPlayer != null) {
                if (setPlayWhenReady){
                    exoPlayer.setPlayWhenReady(true);
                    exoPlayer.getPlaybackState();
                } else {
                    if (exoPlayer.getPlayWhenReady()){
                        exoPlayer.setPlayWhenReady(false);
                        exoPlayer.getPlaybackState();
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "playWhenReady: ", e);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (exoPlayer == null) {
            return super.onKeyDown(keyCode, event); // Handle null exoPlayer early
        }
        switch (keyCode) {
            case KeyEvent.KEYCODE_MEDIA_NEXT -> next();
            case KeyEvent.KEYCODE_MEDIA_PREVIOUS -> previous();
            case KeyEvent.KEYCODE_MEDIA_PLAY, KeyEvent.KEYCODE_MEDIA_PAUSE,
                 KeyEvent.KEYCODE_BUTTON_SELECT -> {
                handleMediaPlayPause(keyCode);
                return true;
            }
            case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE, KeyEvent.KEYCODE_HEADSETHOOK -> {
                togglePlayPause();
                return true;
            }
            case KeyEvent.KEYCODE_VOLUME_UP, KeyEvent.KEYCODE_VOLUME_DOWN -> {
                adjustVolume(keyCode == KeyEvent.KEYCODE_VOLUME_UP, event.getRepeatCount() == 0);
                return true;
            }
            case KeyEvent.KEYCODE_BUTTON_START, KeyEvent.KEYCODE_BUTTON_A, KeyEvent.KEYCODE_ENTER,
                 KeyEvent.KEYCODE_DPAD_CENTER, KeyEvent.KEYCODE_NUMPAD_ENTER,
                 KeyEvent.KEYCODE_SPACE -> {
                handlePlayPauseOnControllerVisibility();
                return true;
            }
            case KeyEvent.KEYCODE_BACK -> {
                handleBackKey();
                return true;
            }
            default -> {
                if (!controllerVisibleFully) {
                    playerView.showController();
                    return true;
                }
            }
        }

        return super.onKeyDown(keyCode, event);
    }

    private void handleMediaPlayPause(int keyCode) {
        if (keyCode == KeyEvent.KEYCODE_MEDIA_PAUSE) {
            exoPlayer.pause();
        } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PLAY || !exoPlayer.isPlaying()) {
            exoPlayer.play();
        } else {
            exoPlayer.pause();
        }
    }

    private void togglePlayPause() {
        if (exoPlayer.isPlaying()) {
            exoPlayer.pause();
        } else {
            exoPlayer.play();
        }
    }

    private void handlePlayPauseOnControllerVisibility() {
        if (!controllerVisibleFully) {
            togglePlayPause();
        }
    }

    private void handleBackKey() {
        if (ApplicationUtil.isTvBox(this)) {
            if (controllerVisible && exoPlayer.isPlaying()) {
                playerView.hideController();
            } else {
                finish();
            }
        }
    }


    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        return switch (keyCode) {
            case KeyEvent.KEYCODE_BACK, KeyEvent.KEYCODE_VOLUME_DOWN -> {
                playerView.postDelayed(playerView.textClearRunnable, CustomPlayerView.MESSAGE_TIMEOUT_KEY);
                yield true;
            }
            default -> super.onKeyUp(keyCode, event);
        };
    }

    private void adjustVolume(final boolean raise, boolean canBoost) {
        playerView.removeCallbacks(playerView.textClearRunnable);

        final int currentVolume = ApplicationUtil.getVolume(this, false, mAudioManager);
        final int maxVolume = ApplicationUtil.getVolume(this, true, mAudioManager);
        boolean volumeActive = currentVolume != 0;

        if (currentVolume != maxVolume) boostLevel = 0;

        if (loudnessEnhancer == null)
            canBoost = false;

        if (currentVolume != maxVolume || (boostLevel == 0 && !raise)) {
            mAudioManager.adjustStreamVolume(
                    AudioManager.STREAM_MUSIC,
                    raise ? AudioManager.ADJUST_RAISE : AudioManager.ADJUST_LOWER,
                    AudioManager.FLAG_REMOVE_SOUND_AND_VIBRATE
            );
            handleVolumeStep(raise, currentVolume);
        } else {
            handleBoost(raise, canBoost, maxVolume);
        }

        playerView.setIconVolume(volumeActive);
        playerView.setHighlight(boostLevel > 0);
        playerView.postDelayed(playerView.textClearRunnable, CustomPlayerView.MESSAGE_TIMEOUT_KEY);
    }

    private void handleVolumeStep(boolean raise, int oldVolume) {
        final int newVolume = ApplicationUtil.getVolume(this, false, mAudioManager);
        if (raise && oldVolume == newVolume) {
            CustomPlayerView.incrementVolumeUpsInRow();
        } else {
            CustomPlayerView.setVolumeUpsInRow(0);
        }

        if (CustomPlayerView.getVolumeUpsInRow() > 4 && !isVolumeMin(mAudioManager)) {
            mAudioManager.adjustStreamVolume(AudioManager.STREAM_MUSIC, AudioManager.ADJUST_RAISE,
                    AudioManager.FLAG_REMOVE_SOUND_AND_VIBRATE | AudioManager.FLAG_SHOW_UI
            );
        } else {
            playerView.setCustomErrorMessage(newVolume != 0 ? " " + newVolume : "");
        }
    }

    private void handleBoost(boolean raise, boolean canBoost, int maxVolume) {
        if (canBoost && raise && boostLevel < 10) {
            boostLevel++;
        } else if (!raise && boostLevel > 0) {
            boostLevel--;
        }

        if (loudnessEnhancer != null) {
            try {
                loudnessEnhancer.setTargetGain(boostLevel * 200);
                loudnessEnhancer.setEnabled(boostLevel > 0);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        playerView.setCustomErrorMessage(" " + (maxVolume + boostLevel));
    }

    public static boolean isVolumeMin(@NonNull final AudioManager audioManager) {
        int min = Build.VERSION.SDK_INT >= 28 ? audioManager.getStreamMinVolume(AudioManager.STREAM_MUSIC) : 0;
        return audioManager.getStreamVolume(AudioManager.STREAM_MUSIC) == min;
    }
}