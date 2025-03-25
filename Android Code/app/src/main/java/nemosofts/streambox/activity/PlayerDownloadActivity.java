package nemosofts.streambox.activity;

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
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.accessibility.CaptioningManager;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.datasource.DataSource;
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.ExoPlayer;
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

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.encrypter.EncryptedFileDataSourceFactory;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.BrightnessVolumeControl;
import nemosofts.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerDownloadActivity extends AppCompatActivity {

    private static final String TAG = "PlayerDownloadActivity";
    private PlayerListener playerListener;
    private MediaSession mediaSession;
    private LoudnessEnhancer loudnessEnhancer;
    private BroadcastReceiver batteryReceiver;
    private AudioManager mAudioManager;
    private int boostLevel = 0;

    private CustomPlayerView playerView;
    private ExoPlayer exoPlayer;
    private DataSource.Factory dataSourceFactory;
    private ProgressBar loadingProgressBar;

    private String channelTitle = "";
    private String channelUrl="";
    private TextView playerTitle;

    private boolean controllerVisible;
    private boolean controllerVisibleFully;
    boolean isTvBox;
    private int resize = 1;

    Cipher mCipher = null;
    SecretKeySpec secretKeySpec;
    byte[] secretKey = BuildConfig.ENC_KEY.getBytes();
    byte[] initialIv = BuildConfig.IV.getBytes();

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

        channelTitle = getIntent().getStringExtra("channel_title");
        channelUrl = getIntent().getStringExtra("channel_url");

        loadingProgressBar = findViewById(R.id.pb_player);
        playerTitle = findViewById(R.id.tv_player_title);

        DefaultBandwidthMeter.Builder bandwidthMeter = new DefaultBandwidthMeter.Builder(this);

        // Set default cookie manager if not already set
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        if (mCipher == null) {
            String aes = "AES";
            String tr = "AES/CTR/NoPadding";
            secretKeySpec = new SecretKeySpec(secretKey, aes);
            try {
                mCipher = Cipher.getInstance(tr);
                mCipher.init(Cipher.DECRYPT_MODE, secretKeySpec, new IvParameterSpec(initialIv));
            } catch (Exception e) {
                Log.e(TAG, "Error onCreate mCipher: ", e);
            }
        }
        dataSourceFactory = new EncryptedFileDataSourceFactory(mCipher, secretKeySpec, new IvParameterSpec(initialIv), bandwidthMeter.build());

        // https://github.com/google/ExoPlayer/issues/8571
        DefaultExtractorsFactory extractorsFactory = ApplicationUtil.getDefaultExtractorsFactory();
        DefaultRenderersFactory renderersFactory = ApplicationUtil.getDefaultRenderersFactory(this, new SPHelper(this).isHardwareDecoding());

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

        SPHelper spHelper = new SPHelper(this);

        // Attach ExoPlayer to the player view
        playerView = findViewById(R.id.nSoftsPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setShowVrButton(spHelper.getIsVR());
        playerView.setShowSubtitleButton(spHelper.getIsSubtitle());
        playerView.setShowFastForwardButton(true);
        playerView.setShowRewindButton(true);
        playerView.setShowNextButton(false);
        playerView.setShowPreviousButton(false);
        playerView.setShowShuffleButton(true);
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);
        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            controllerVisible = visibility == View.VISIBLE;
            controllerVisibleFully = playerView.isControllerFullyVisible();

            findViewById(R.id.rl_player_top).setVisibility(visibility);

            // https://developer.android.com/training/system-ui/immersive
            IfSupported.toggleSystemUi(PlayerDownloadActivity.this, playerView, visibility == View.VISIBLE);
            if (isTvBox && visibility == View.VISIBLE) {
                // Because when using dpad controls, focus resets to first item in bottom controls bar
                findViewById(androidx.media3.ui.R.id.exo_play_pause).requestFocus();
            }
        });

        setCustomTrackSubtitle();
        setMediaSource();

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
            Log.e(TAG, "Error setCustomSubtitle: ", e);
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
                Log.e(TAG, "Error onAudioSessionIdChanged: ", e);
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
            exoPlayer.stop();
            loadingProgressBar.setVisibility(View.GONE);
            Toasty.makeText(PlayerDownloadActivity.this,true, "Failed : " + error.getErrorCodeName(), Toasty.ERROR);
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
            Log.e(TAG, "Error notifyAudioSessionUpdate: ", e);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_player_single;
    }

    private void setMediaSource() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(PlayerDownloadActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        playerTitle.setText(channelTitle);
        Uri uri = Uri.parse(channelUrl);
        MediaSource sampleSource = new ProgressiveMediaSource.Factory(dataSourceFactory)
                .createMediaSource(new MediaItem.Builder().setUri(uri).build());
        exoPlayer.setMediaSource(sampleSource);

        try {
            if (loudnessEnhancer != null) {
                loudnessEnhancer.release();
            }
            loudnessEnhancer = new LoudnessEnhancer(exoPlayer.getAudioSessionId());
        } catch (Exception e) {
            Log.e(TAG, "Error setMediaSource loudnessEnhancer: ", e);
        }
        notifyAudioSessionUpdate(true);

        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);
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
                exoPlayer = null;
            }
        } catch (Exception e) {
            Log.e(TAG, "Error releasePlayer: ", e);
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
            Log.e(TAG, "Error playWhenReady: ", e);
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
                handlePlayPause(keyCode);
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
                if (ApplicationUtil.isTvBox(this)) {
                    handleBackKey();
                    return true;
                }
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

    private void handlePlayPause(int keyCode) {
        if (keyCode == KeyEvent.KEYCODE_MEDIA_PAUSE) {
            exoPlayer.pause();
        } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PLAY) {
            exoPlayer.play();
        } else {
            if (exoPlayer.isPlaying()) {
                exoPlayer.pause();
            } else {
                exoPlayer.play();
            }
        }
    }

    private void togglePlayPause() {
        if (exoPlayer.isPlaying()) {
            exoPlayer.pause();
        } else {
            exoPlayer.play();
        }
    }

    private void handleBackKey() {
        if (controllerVisible && exoPlayer != null && exoPlayer.isPlaying()) {
            playerView.hideController();
        } else {
            finish();
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
           Log.e(TAG, "Error seeking: ", e);
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