package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ActivityInfo;
import android.media.AudioFocusRequest;
import android.media.AudioManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.PowerManager;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.MediaMetadata;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.dash.DashMediaSource;
import androidx.media3.exoplayer.dash.DefaultDashChunkSource;
import androidx.media3.exoplayer.hls.HlsMediaSource;
import androidx.media3.exoplayer.rtsp.RtspMediaSource;
import androidx.media3.exoplayer.smoothstreaming.DefaultSsChunkSource;
import androidx.media3.exoplayer.smoothstreaming.SsMediaSource;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterRadio;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class RadioActivity extends AppCompatActivity implements Player.Listener {

    private static final String TAG = "RadioActivity";
    private int playback = 1;
    private SPHelper spHelper;
    private JSHelper jsHelper;
    private FrameLayout frameLayout;
    private RecyclerView rv;
    private ArrayList<ItemChannel> arrayList;
    private ProgressDialog progressDialog;
    private ImageView previous;
    private ImageView play;
    private ImageView next;
    private ProgressBar pb;
    private TextView radioCat;
    private Boolean isNewSong = false;
    private DefaultBandwidthMeter bandwidthMeter;
    private DataSource.Factory mediaDataSourceFactory;
    private ExoPlayer exoPlayer = null;
    private AudioManager mAudioManager;
    private PowerManager.WakeLock mWakeLock;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        findViewById(R.id.iv_back_page).setOnClickListener(view -> onBackPressed());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        progressDialog = new ProgressDialog(RadioActivity.this, true);

        jsHelper = new JSHelper(this);
        spHelper = new SPHelper(this);

        mAudioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            AudioFocusRequest audioFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                    .setOnAudioFocusChangeListener(onAudioFocusChangeListener)
                    .build();

            mAudioManager.requestAudioFocus(audioFocusRequest);
        } else {
            // For older API levels (pre-Oreo), you can continue using the deprecated method
            mAudioManager.requestAudioFocus(onAudioFocusChangeListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN);
        }

        try {
            registerReceiver(onCallIncome, new IntentFilter("android.intent.action.PHONE_STATE"));
            registerReceiver(onHeadPhoneDetect, new IntentFilter(AudioManager.ACTION_AUDIO_BECOMING_NOISY));
        } catch (Exception e) {
            Log.e(TAG, "Failed to register receiver", e);
        }

        bandwidthMeter = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        exoPlayer = new ExoPlayer.Builder(this).build();
        exoPlayer.addListener(this);

        PowerManager powerManager = (PowerManager) getSystemService(Context.POWER_SERVICE);
        mWakeLock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, getClass().getName());
        mWakeLock.setReferenceCounted(false);

        AudioAttributes audioAttributes = new AudioAttributes.Builder()
                .setUsage(C.USAGE_MEDIA)
                .setContentType(C.AUDIO_CONTENT_TYPE_MUSIC)
                .build();
        exoPlayer.setAudioAttributes(audioAttributes, true);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);

        radioCat = findViewById(R.id.tv_radio_cat_name);

        previous = findViewById(R.id.iv_min_previous);
        play = findViewById(R.id.iv_min_play);
        next = findViewById(R.id.iv_min_next);
        pb = findViewById(R.id.pb_min);

        play.setOnClickListener(v -> playPause());
        next.setOnClickListener(v -> navigateSong(true));
        previous.setOnClickListener(v -> navigateSong(false));

        GridLayoutManager grid;
        if (ApplicationUtil.isTvBox(this)){
            grid = new GridLayoutManager(this, 8);
            grid.setSpanCount(8);
        } else {
            grid = new GridLayoutManager(this, 6);
            grid.setSpanCount(6);
        }
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        new Handler(Looper.getMainLooper()).postDelayed(this::getData, 0);

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                try {
                    Callback.setIsPlayed(false);

                    exoPlayer.setPlayWhenReady(false);
                    changePlayPauseIcon(false);
                    exoPlayer.stop();
                    exoPlayer.release();
                    exoPlayer = null;

                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                        AudioFocusRequest audioFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                                .setOnAudioFocusChangeListener(onAudioFocusChangeListener)
                                .build();
                        mAudioManager.abandonAudioFocusRequest(audioFocusRequest);
                    } else {
                        // For lower API levels, fallback to the old method
                        mAudioManager.abandonAudioFocus(onAudioFocusChangeListener);
                    }
                    unregisterReceiver(onCallIncome);
                    unregisterReceiver(onHeadPhoneDetect);
                } catch (Exception e) {
                    Log.e(TAG, "Failed to unregister receiver", e);
                }
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }

    public void isBuffering(Boolean isBuffer) {
        if (isBuffer != null){
            if (Boolean.FALSE.equals(isBuffer)) {
                pb.setVisibility(View.INVISIBLE);
                changePlayPauseIcon(true);
            } else {
                pb.setVisibility(View.VISIBLE);
            }
            next.setEnabled(!isBuffer);
            previous.setEnabled(!isBuffer);
        }
    }

    public void changePlayPauseIcon(Boolean isPlay) {
        if (Boolean.FALSE.equals(isPlay)) {
           play.setImageResource(R.drawable.ic_play);
        } else {
            play.setImageResource(R.drawable.ic_pause);
        }
    }

    private void startNewSong() {
        setPlayer();
        isNewSong = true;
        isBuffering(true);
        String finalUrl;
        if (Boolean.TRUE.equals(spHelper.getIsXuiUser())){
            finalUrl = spHelper.getServerURL()+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"
                    +Callback.getArrayListRadio().get(Callback.getPlayPos()).getStreamID()+".m3u8";
        } else {
            finalUrl = spHelper.getServerURL()+"live/"+ spHelper.getUserName()+"/"+ spHelper.getPassword()
                    +"/"+Callback.getArrayListRadio().get(Callback.getPlayPos()).getStreamID()+".m3u8";
        }
        Uri uri = Uri.parse(finalUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);
        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);
    }

    private void togglePlay() {
        exoPlayer.setPlayWhenReady(!exoPlayer.getPlayWhenReady());
        changePlayPauseIcon(exoPlayer.getPlayWhenReady());
    }

    public void playPause() {
        if (Callback.getArrayListRadio().isEmpty()) {
            Toast.makeText(RadioActivity.this, getString(R.string.error_no_radio_selected), Toast.LENGTH_SHORT).show();
            return;
        }
        if (Boolean.TRUE.equals(Callback.getIsPlayed())) {
            togglePlay();
        } else {
            startNewSong();
        }
    }

    private void navigateSong(boolean isNext) {
        if (Callback.getArrayListRadio().isEmpty()) {
            Toast.makeText(RadioActivity.this, getString(R.string.error_no_radio_selected), Toast.LENGTH_SHORT).show();
            return;
        }
        if (!NetworkUtils.isConnected(this)) {
            Toast.makeText(RadioActivity.this, getString(R.string.err_internet_not_connected), Toast.LENGTH_SHORT).show();
            return;
        }
        isNewSong = true;
        isBuffering(true);
        int newPos = Callback.getPlayPos() + (isNext ? 1 : -1);
        if (newPos >= 0 && newPos < Callback.getArrayListRadio().size()) {
            Callback.setPlayPos(newPos);
        } else {
            Callback.setPlayPos(isNext ? 0 : Callback.getArrayListRadio().size() - 1);
        }
        startNewSong();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_radio;
    }

    private void getData() {
        new AsyncTaskExecutor<String, String, String>() {

            final ArrayList<ItemChannel> itemChannels = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    itemChannels.addAll(jsHelper.getLiveRadio());
                    return "1";
                } catch (Exception e) {
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (itemChannels.isEmpty()) {
                    setEmpty();
                } else {
                    arrayList.addAll(itemChannels);
                    setAdapterToListview();
                }
            }
        }.execute();
    }

    public void setAdapterToListview() {
        Callback.setPlayPos(0);
        Callback.setArrayListRadio(arrayList);
        setPlayer();
        AdapterRadio adapter = new AdapterRadio(this, arrayList, (itemRadio, position) -> {
            Callback.setPlayPos(position);
            if (!Callback.getArrayListRadio().isEmpty()){
                Callback.getArrayListRadio().clear();
            }
            Callback.setArrayListRadio(arrayList);
            startNewSong();
        });
        rv.setAdapter(adapter);
        setEmpty();
    }

    private void setPlayer() {
        ImageView logo = findViewById(R.id.iv_radio_logo);
        TextView radioName = findViewById(R.id.tv_radio_name);
        radioName.setText(Callback.getArrayListRadio().get(Callback.getPlayPos()).getName());

        Picasso.get()
                .load(Callback.getArrayListRadio().get(Callback.getPlayPos()).getStreamIcon().isEmpty()
                        ? "null" : Callback.getArrayListRadio().get(Callback.getPlayPos()).getStreamIcon())
                .resize(300, 300)
                .centerCrop()
                .placeholder(R.drawable.logo)
                .error(R.drawable.logo)
                .into(logo);
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                rv.requestFocus();
            }
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

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
        return new DefaultHttpDataSource.Factory()
                .setUserAgent(Util.getUserAgent(this, "ExoPlayerDemo"))
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    @Override
    public void onPlaybackStateChanged(int playbackState) {
        Player.Listener.super.onPlaybackStateChanged(playbackState);
        if (playbackState == Player.STATE_READY) {
            playback = 1;
            exoPlayer.play();
            if (Boolean.TRUE.equals(isNewSong)) {
                isNewSong = false;
                Callback.setIsPlayed(true);
                isBuffering(false);
            }
        }
    }

    @Override
    public void onIsPlayingChanged(boolean isPlaying) {
        Player.Listener.super.onIsPlayingChanged(isPlaying);
        changePlayPauseIcon(isPlaying);
        if (isPlaying) {
            if (!mWakeLock.isHeld()) {
                mWakeLock.acquire(60000);
            }
        } else {
            if (mWakeLock.isHeld()) {
                mWakeLock.release();
            }
        }
    }

    @Override
    public void onPlayerError(@NonNull PlaybackException error) {
        Player.Listener.super.onPlayerError(error);
        if (playback < 5){
            playback = playback + 1;
            Toast.makeText(RadioActivity.this,"Playback error - "+ playback+"/5 "
                    + error.getMessage(), Toast.LENGTH_SHORT).show();
            startNewSong();
        } else {
            playback = 1;
            exoPlayer.setPlayWhenReady(false);
            isBuffering(false);
            changePlayPauseIcon(false);
            Toast.makeText(getApplicationContext(), "Failed : " + error.getErrorCodeName(), Toast.LENGTH_SHORT).show();
        }
    }

    @Override
    public void onMediaMetadataChanged(@NonNull MediaMetadata mediaMetadata) {
        try {
            String metadata = String.valueOf(mediaMetadata.title);
            radioCat.setText(metadata.isEmpty() || metadata.equals("null") ? getString(R.string.app_name) : metadata);
        } catch (Exception e) {
            e.printStackTrace();
        }
        Player.Listener.super.onMediaMetadataChanged(mediaMetadata);
    }

    BroadcastReceiver onCallIncome = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, @NonNull Intent intent) {
            String a = intent.getStringExtra(TelephonyManager.EXTRA_STATE);
            try {
                if (exoPlayer.getPlayWhenReady()) {
                    assert a != null;
                    if (a.equals(TelephonyManager.EXTRA_STATE_OFFHOOK)
                            || a.equals(TelephonyManager.EXTRA_STATE_RINGING)) {
                        exoPlayer.setPlayWhenReady(false);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    };

    BroadcastReceiver onHeadPhoneDetect = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
        try {
            if (exoPlayer.getPlayWhenReady()) {
                togglePlay();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        }
    };

    AudioManager.OnAudioFocusChangeListener onAudioFocusChangeListener = focusChange -> {
        if (focusChange == AudioManager.AUDIOFOCUS_LOSS
                || focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
            try {
                if (exoPlayer.getPlayWhenReady()) {
                    togglePlay();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    };

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                finish();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @SuppressLint("Wakelock")
    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()) {
            progressDialog.cancel();
        }

        try {
            Callback.setIsPlayed(false);

            exoPlayer.setPlayWhenReady(false);
            changePlayPauseIcon(false);
            exoPlayer.stop();
            exoPlayer.release();
            exoPlayer = null;

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                // New method for handling audio focus on API 26 and above
                AudioFocusRequest audioFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                        .setOnAudioFocusChangeListener(onAudioFocusChangeListener)
                        .build();
                mAudioManager.abandonAudioFocusRequest(audioFocusRequest);
            } else {
                // Fallback for older versions (below API 26)
                mAudioManager.abandonAudioFocus(onAudioFocusChangeListener);
            }
            unregisterReceiver(onCallIncome);
            unregisterReceiver(onHeadPhoneDetect);
        } catch (Exception e) {
            Log.e(TAG, "Failed to unregister receiver", e);
        }
        try {
            if (mWakeLock != null && mWakeLock.isHeld()) {
                mWakeLock.release();
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to release wake lock", e);
        }
        super.onDestroy();
    }
}