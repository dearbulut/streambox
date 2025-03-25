package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
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
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterChannelEpg;
import nemosofts.streambox.adapter.AdapterEpg;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadEpg;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.item.ItemPost;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class EPGTwoActivity extends AppCompatActivity {

    private static final String TAG = "EPGTwoActivity";
    private SPHelper spHelper;
    private JSHelper jsHelper;
    private String catID = "0";
    private RecyclerView rvLive;
    private ArrayList<ItemChannel> arrayList;
    private final ArrayList<ItemPost> arrayListPost = new ArrayList<>();
    AdapterChannelEpg adapter;
    private ProgressBar pb;
    private int position = 0;
    private TextView tvTitle;
    private TextView tvTime;

    private ExoPlayer exoPlayer;
    private DefaultBandwidthMeter bandwidthMeter;
    private DataSource.Factory mediaDataSourceFactory;

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
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        catID = getIntent().getStringExtra("cat_id");

        jsHelper = new JSHelper(this);
        spHelper = new SPHelper(this);

        arrayList = new ArrayList<>();

        tvTitle = findViewById(R.id.tv_title);
        tvTime = findViewById(R.id.tv_time);

        pb = findViewById(R.id.pb);
        rvLive = findViewById(R.id.rv_live);
        rvLive.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rvLive.setLayoutManager(llm);
        rvLive.setNestedScrollingEnabled(false);

        intPlayer();

        getData();

        LinearLayout adView = findViewById(R.id.ll_adView);
        new Helper(this).showBannerAd(adView, Callback.getBannerEpg());
    }

    private void intPlayer() {
        bandwidthMeter = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }
        exoPlayer = new ExoPlayer.Builder(this).build();
        PlayerView playerView = findViewById(R.id.exoPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FILL);
        exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        exoPlayer.addListener(new Player.Listener(){

            @Override
            public void onIsPlayingChanged(boolean isPlaying) {
                Player.Listener.super.onIsPlayingChanged(isPlaying);
                playerView.setKeepScreenOn(isPlaying);
            }

            @Override
            public void onPlaybackStateChanged(int playbackState) {
                Player.Listener.super.onPlaybackStateChanged(playbackState);
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    findViewById(R.id.pb_player).setVisibility(View.GONE);
                } else if (playbackState == Player.STATE_BUFFERING) {
                    findViewById(R.id.pb_player).setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                exoPlayer.stop();
                findViewById(R.id.pb_player).setVisibility(View.GONE);
                Toast.makeText(EPGTwoActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
                Player.Listener.super.onPlayerError(error);
            }
        });
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_epg_two;
    }

    private void getData() {
        new AsyncTaskExecutor<String, String, String>() {

            final ArrayList<ItemChannel> itemChannels = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                pb.setVisibility(View.VISIBLE);
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    itemChannels.addAll(jsHelper.getLive(catID));
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
                pb.setVisibility(View.GONE);
                if (itemChannels.isEmpty()) {
                    findViewById(R.id.ll_epg).setVisibility(View.GONE);
                    findViewById(R.id.ll_epg_empty).setVisibility(View.VISIBLE);
                } else {
                    arrayList.addAll(itemChannels);
                    setAdapterToListview();
                }
            }
        }.execute();
    }

    public void setAdapterToListview() {
        adapter = new AdapterChannelEpg(this, arrayList, (itemCat, pos) -> {
            position = pos;
            adapter.select(pos);
            setMediaSource(pos);
        });
        rvLive.setAdapter(adapter);
        setMediaSource(position);
        adapter.select(position);
        findViewById(R.id.vw_player).setOnClickListener(v -> {
            if (!arrayList.isEmpty()){
                Callback.setPlayPosLive(position);
                if (!Callback.getArrayListLive().isEmpty()) {
                    Callback.getArrayListLive().clear();
                }
                Callback.setArrayListLive(arrayList);
                startActivity(new Intent(EPGTwoActivity.this, PlayerLiveActivity.class));
            }
        });
    }

    private void setMediaSource(int playPos) {
        String channelUrl = getChannelUrl(playPos);
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);
        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);

        if (!arrayListPost.isEmpty()){
            arrayListPost.clear();
        }
        ItemPost itemPost = new ItemPost("1","logo");
        ArrayList<ItemChannel> arrayListLive = new ArrayList<>();
        arrayListLive.add(arrayList.get(playPos));
        itemPost.setArrayListLive(arrayListLive);
        arrayListPost.add(itemPost);

        getEpgData(playPos);
    }

    private @NonNull String getChannelUrl(int playPos) {
        String channelUrl;
        String format = ".m3u8";
        if (spHelper.getLiveFormat() == 1){
            format = ".ts";
        }
        if (Boolean.TRUE.equals(spHelper.getIsXuiUser())){
            channelUrl = spHelper.getServerURL()+ spHelper.getUserName()+"/"+
                    spHelper.getPassword()+"/"+arrayList.get(playPos).getStreamID()+format;
        } else {
            channelUrl = spHelper.getServerURL()+"live/"+ spHelper.getUserName()+"/"+
                    spHelper.getPassword()+"/"+arrayList.get(playPos).getStreamID()+format;
        }
        return channelUrl;
    }

    private void getEpgData(int playPos) {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {

                @Override
                public void onStart() {
                    pb.setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (isFinishing()){
                        return;
                    }
                    pb.setVisibility(View.GONE);
                    if (!epgArrayList.isEmpty()){
                        setEpg(epgArrayList);
                    } else {
                        setEpg(null);
                    }
                }
            }, ApplicationUtil.getAPIRequestID("get_simple_data_table","stream_id",
                    arrayList.get(playPos).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void setEpg(ArrayList<ItemEpg> arrayListEpg) {
        ItemPost itemPost = new ItemPost("1","listings");
        if (arrayListEpg != null && !arrayListEpg.isEmpty()){
            itemPost.setArrayListEpg(arrayListEpg);
            loadTimeData();
        } else {
            ArrayList<ItemEpg> arrayListEp = new ArrayList<>();
            arrayListEp.add(new ItemEpg("","", ApplicationUtil.encodeBase64("No Data Found"),"",""));
            itemPost.setArrayListEpg(arrayListEp);
            tvTitle.setText(R.string.err_no_data_found);
            tvTime.setText("-");
        }
        arrayListPost.add(itemPost);

        RecyclerView rv = findViewById(R.id.rv_epg);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv.setLayoutManager(llm);
        rv.setItemAnimator(new DefaultItemAnimator());
        AdapterEpg adapterHome = new AdapterEpg(this, spHelper.getIs12Format(), arrayListPost);
        rv.setAdapter(adapterHome);
        rv.scrollToPosition(arrayListPost.size() - 1);
    }

    private void loadTimeData() {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    // this method is empty
                }

                @SuppressLint("SetTextI18n")
                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (isFinishing()){
                        return;
                    }
                    if (!epgArrayList.isEmpty()){
                        tvTitle.setText(ApplicationUtil.decodeBase64(epgArrayList.get(0).getTitle()));
                        tvTime.setText(ApplicationUtil.getTimestamp(epgArrayList.get(0).getStartTimestamp(),
                                spHelper.getIs12Format()) + " - "
                                + ApplicationUtil.getTimestamp(epgArrayList.get(0).getStopTimestamp(),
                                spHelper.getIs12Format()));
                    } else {
                        tvTitle.setText(R.string.err_no_data_found);
                        tvTime.setText("-");
                    }
                }
            }, ApplicationUtil.getAPIRequestID("get_short_epg","stream_id",
                    arrayList.get(position).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

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
            default -> throw new IllegalStateException("Unsupported type: " + type);
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
                        ? Util.getUserAgent(EPGTwoActivity.this, "ExoPlayerDemo")
                        : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
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
            if (exoPlayer != null) {
                exoPlayer.clearMediaItems();
                exoPlayer.release();
                exoPlayer = null;
            }
        } catch (Exception e) {
            Log.e(TAG, "releasePlayer ",e);
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
            Log.e(TAG, "playWhenReady ",e);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        switch (keyCode) {
            case KeyEvent.KEYCODE_BACK:
                finish();
                break;
            case KeyEvent.KEYCODE_HOME:
                ApplicationUtil.openHomeActivity(this);
                break;
            case KeyEvent.KEYCODE_UNKNOWN:
                return super.onKeyDown(keyCode, event);
            default:
                break;
        }
        return super.onKeyDown(keyCode, event);
    }
}