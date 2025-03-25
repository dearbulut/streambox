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
import android.widget.TextView;
import android.widget.Toast;

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
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.BrightnessVolumeControl;
import nemosofts.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerMovieActivity extends AppCompatActivity {

    private static final String TAG = "PlayerMovieActivity";
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

    private String streamID="";
    private String container=".mp4";
    private String movieName="";
    private String streamRating="";
    private String streamIcon="";
    private TextView playerTitle;

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

        streamID = getIntent().getStringExtra("stream_id");
        container = getIntent().getStringExtra("container");
        movieName = getIntent().getStringExtra("movie_name");
        streamRating = getIntent().getStringExtra("stream_rating");
        streamIcon = getIntent().getStringExtra("stream_icon");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        loadingProgressBar = findViewById(R.id.pb_player);
        playerTitle = findViewById(R.id.tv_player_title);

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
        playerView.setShowVrButton(spHelper.getIsVR());
        playerView.setShowSubtitleButton(spHelper.getIsSubtitle());
        playerView.setShowFastForwardButton(true);
        playerView.setShowRewindButton(true);
        playerView.setShowNextButton(false);
        playerView.setShowPreviousButton(false);
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            controllerVisible = visibility == View.VISIBLE;
            controllerVisibleFully = playerView.isControllerFullyVisible();

            findViewById(R.id.rl_player_movie_top).setVisibility(visibility);

            // https://developer.android.com/training/system-ui/immersive
            IfSupported.toggleSystemUi(PlayerMovieActivity.this, playerView, visibility == View.VISIBLE);
            if (isTvBox && visibility == View.VISIBLE) {
                // Because when using dpad controls, focus resets to first item in bottom controls bar
                findViewById(androidx.media3.ui.R.id.exo_play_pause).requestFocus();
            }
        });

        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        setCustomTrackSubtitle();
        setMediaSource(dbHelper.getSeek(DBHelper.TABLE_SEEK_MOVIE, streamID, movieName));

        // Set player event listeners
        playerListener = new PlayerListener();
        exoPlayer.addListener(playerListener);

        setBatteryInfo();
        findViewById(R.id.exo_resize).setOnClickListener(v -> setResize());
        findViewById(R.id.iv_media_info).setOnClickListener(v -> getPlayerInfo());
        findViewById(R.id.iv_back_player).setOnClickListener(v -> finish());
        if (isTvBox){
            findViewById(R.id.iv_back_player).setVisibility(View.GONE);
            AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
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
                Log.e(TAG, "Failed to create media session", e);
            }
        }
    }

    private void getPlayerInfo() {
        if (exoPlayer != null && exoPlayer.getPlayWhenReady() && exoPlayer.getVideoFormat() != null){
            playerView.hideController();
            DialogUtil.dialogPlayerInfo(this, exoPlayer, false);
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

    private void setCustomTrackSubtitle() {
        ApplicationUtil.setCustomTrackNameProvider(this, playerView);

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
            Log.e(TAG, "Failed to set custom subtitle view style", e);
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
               Log.e(TAG, "Failed to create loudness enhancer", e);
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
                loadingProgressBar.setVisibility(View.GONE);
                playback = 1;
                startTimer();
            } else if (state == Player.STATE_BUFFERING) {
                loadingProgressBar.setVisibility(View.VISIBLE);
            } else if (state == Player.STATE_ENDED && !isFinishing()){
               try {
                   dbHelper.removeSeekID(DBHelper.TABLE_SEEK_MOVIE, streamID, movieName);
               } catch (Exception e) {
                   e.printStackTrace();
               }
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
                Toast.makeText(PlayerMovieActivity.this,"Playback error - "+ playback +"/5 ", Toast.LENGTH_SHORT).show();
                new Handler(Looper.getMainLooper()).postDelayed(() -> {
                    if (isFinishing()){
                        return;
                    }
                    setMediaSource(dbHelper.getSeek(DBHelper.TABLE_SEEK_MOVIE, streamID, movieName));
                }, 600);
            } else {
                playback = 1;
                exoPlayer.stop();
                loadingProgressBar.setVisibility(View.GONE);
                Toast.makeText(PlayerMovieActivity.this,"Failed : " + error.getErrorCodeName(), Toast.LENGTH_SHORT).show();
            }
        }
    }

    public void startTimer() {
        if (!isFinishing() && !isTvBox && countDownTimer == null && Boolean.TRUE.equals(Callback.getRewardAdMovie())){
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
                    helper.showRewardAds(Callback.getRewardAdMovie(),exoPlayer != null && exoPlayer.isPlaying(), playWhenReady1 -> {

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
            Log.e(TAG, "Failed to send audio session update broadcast", e);
        }
    }

    private void setMediaSource(int currentPosition) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(PlayerMovieActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        if (!spHelper.isLogged()){
            return;
        }

        playerTitle.setText(movieName);
        String channelUrl = spHelper.getServerURL() + "movie/" + spHelper.getUserName()
                + "/" + spHelper.getPassword()+ "/" + streamID + "." +container;
        Uri uri = Uri.parse(channelUrl);

        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);

        try {
            if (loudnessEnhancer != null) {
                loudnessEnhancer.release();
            }
            loudnessEnhancer = new LoudnessEnhancer(exoPlayer.getAudioSessionId());
        } catch (Exception e) {
            Log.e(TAG, "Failed to create loudness enhancer", e);
        }
        notifyAudioSessionUpdate(true);

        exoPlayer.seekTo(currentPosition);
        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);

        try {
            ItemMovies itemMovies = new ItemMovies(movieName,streamID,streamIcon,streamRating,"");
            dbHelper.addToMovie(DBHelper.TABLE_RECENT_MOVIE, itemMovies, spHelper.getMovieLimit());
        } catch (Exception e) {
            Log.e(TAG, "Failed to add to recent movies", e);
        }
    }

    @SuppressLint("SwitchIntDef")
    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int type = Util.inferContentType(uri);
        MediaItem mediaItem = MediaItem.fromUri(uri);
        return switch (type) {
            case C.CONTENT_TYPE_SS ->
                // For SmoothStreaming (SS)
                new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_DASH ->
                // For Dynamic Adaptive Streaming over HTTP (DASH)
                new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_HLS ->
                // For HTTP Live Streaming (HLS)
                new HlsMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_RTSP ->
                // For Real-Time Streaming Protocol (RTSP)
                new RtspMediaSource.Factory().createMediaSource(mediaItem);
            case C.CONTENT_TYPE_OTHER ->
                // For Progressive Media
                new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            default ->
                    new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
        };
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
                        ? Util.getUserAgent(PlayerMovieActivity.this, "ExoPlayerDemo")
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

    private long getCurrentSeekPosition() {
        return exoPlayer.getCurrentPosition();
    }

    private long getFullSeekPosition() {
        int progress = 0;
        if (exoPlayer != null) {
            long currentPosition = exoPlayer.getCurrentPosition();
            long duration = exoPlayer.getDuration();
            if (duration > 0) {
                progress = (int) ((currentPosition * 100) / duration);
            }
        }
        return progress;
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_player_movie;
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

                dbHelper.addToSeek(DBHelper.TABLE_SEEK_MOVIE, String.valueOf(getCurrentSeekPosition()),
                        String.valueOf(getFullSeekPosition()), streamID, movieName);

                exoPlayer.removeListener(playerListener);
                exoPlayer.clearMediaItems();
                exoPlayer.release();
                exoPlayer = null;
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to release player", e);
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
            Log.e(TAG, "Failed to set play when ready", e);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (exoPlayer == null) {
            return super.onKeyDown(keyCode, event);
        }
        switch (keyCode) {
            case KeyEvent.KEYCODE_MEDIA_PLAY, KeyEvent.KEYCODE_MEDIA_PAUSE,
                 KeyEvent.KEYCODE_BUTTON_SELECT -> {
                handlePlayPauseToggle(keyCode);
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
                if (!controllerVisibleFully) {
                    togglePlayPause();
                    return true;
                }
            }
            case KeyEvent.KEYCODE_DPAD_LEFT, KeyEvent.KEYCODE_BUTTON_L2,
                 KeyEvent.KEYCODE_MEDIA_REWIND -> {
                if (!controllerVisibleFully) {
                    seekBy(-10 * 1000L); // Seek backward by 10 seconds
                    return true;
                }
            }
            case KeyEvent.KEYCODE_DPAD_RIGHT, KeyEvent.KEYCODE_BUTTON_R2,
                 KeyEvent.KEYCODE_MEDIA_FAST_FORWARD -> {
                if (!controllerVisibleFully) {
                    seekBy(10 * 1000L); // Seek forward by 10 seconds
                    return true;
                }
            }
            case KeyEvent.KEYCODE_BACK -> {
                handleBackButton();
                return true;
            }
            case KeyEvent.KEYCODE_UNKNOWN -> {
                return super.onKeyDown(keyCode, event);
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

    private void handlePlayPauseToggle(int keyCode) {
        if (keyCode == KeyEvent.KEYCODE_MEDIA_PAUSE) {
            exoPlayer.pause();
        } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PLAY) {
            exoPlayer.play();
        } else if (exoPlayer.isPlaying()) {
            exoPlayer.pause();
        } else {
            exoPlayer.play();
        }
    }

    private void togglePlayPause() {
        if (exoPlayer.isPlaying()) {
            exoPlayer.pause();
        } else {
            exoPlayer.play();
        }
    }

    private void handleBackButton() {
        if (ApplicationUtil.isTvBox(this)) {
            if (controllerVisible && exoPlayer != null && exoPlayer.isPlaying()) {
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

    private void seekBy(long positionMs) {
        try {
            if (exoPlayer != null) {
                long currentPosition = exoPlayer.getCurrentPosition();
                long newPosition = currentPosition + positionMs;
                // Ensure the new position is within the bounds of the media duration
                long duration = exoPlayer.getDuration();
                newPosition = Math.max(0, Math.min(newPosition, duration));
                // Seek to the new position
                exoPlayer.seekTo(newPosition);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to seek", e);
        }
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
                Log.e(TAG, "Failed to set loudness enhancer target gain", e);
            }
        }
        playerView.setCustomErrorMessage(" " + (maxVolume + boostLevel));
    }

    public static boolean isVolumeMin(@NonNull final AudioManager audioManager) {
        int min = Build.VERSION.SDK_INT >= 28 ? audioManager.getStreamMinVolume(AudioManager.STREAM_MUSIC) : 0;
        return audioManager.getStreamVolume(AudioManager.STREAM_MUSIC) == min;
    }
}