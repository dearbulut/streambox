package nemosofts.streambox.activity;

import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.BlurImage;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterMovieCast;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.executor.LoadMovieID;
import nemosofts.streambox.interfaces.MovieIDListener;
import nemosofts.streambox.item.ItemCast;
import nemosofts.streambox.item.ItemInfoMovies;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemMoviesData;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class DetailsMovieActivity extends AppCompatActivity {

    private static final String TAG = "DetailsMovieActivity";
    Helper helper;
    private int playback = 0;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private ItemInfoMovies itemMovies;
    private ItemMoviesData itemData;
    private ImageView poster;
    private ImageView star1;
    private ImageView star2;
    private ImageView star3;
    private ImageView star4;
    private ImageView star5;
    private TextView pageTitle;
    private TextView directed;
    private TextView release;
    private TextView duration;
    private TextView genre;
    private TextView cast;
    private TextView plot;
    private String streamID;
    private String streamName;
    private String streamIcon;
    private String streamRating;
    private ImageView btnFav;
    private ImageView btnDownload;
    private ImageView btnDownloadClose;
    private int themeBg;
    private final Handler seekHandler = new Handler(Looper.getMainLooper());
    private ProgressBar pbDownload;
    private ProgressDialog progressDialog;
    private LinearLayout llPage;
    private FrameLayout shimmer;
    private static final String TAG_STREAM_ID = "stream_id";

    private ActivityResultLauncher<String> permissionLauncher;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        themeBg = ApplicationUtil.openThemeBg(this);

        ImageView blur = findViewById(R.id.iv_bg_blur);
        blur.setImageResource(themeBg);

        ImageView alpha = findViewById(R.id.iv_alpha);
        alpha.setImageResource(themeBg);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        progressDialog = new ProgressDialog(DetailsMovieActivity.this, true);

        streamID = getIntent().getStringExtra(TAG_STREAM_ID);
        streamName = getIntent().getStringExtra("stream_name");
        streamIcon = getIntent().getStringExtra("stream_icon");
        streamRating = getIntent().getStringExtra("stream_rating");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        llPage = findViewById(R.id.ll_page);
        shimmer = findViewById(R.id.fl_shimmer);
        poster = findViewById(R.id.iv_poster);
        pageTitle = findViewById(R.id.tv_page_title);
        btnFav = findViewById(R.id.iv_fav);
        btnDownload = findViewById(R.id.iv_download);
        pbDownload = findViewById(R.id.pb_download);
        btnDownloadClose = findViewById(R.id.iv_download_close);

        directed = findViewById(R.id.tv_directed);
        release = findViewById(R.id.tv_release);
        duration = findViewById(R.id.tv_duration);
        genre = findViewById(R.id.tv_genre);
        cast = findViewById(R.id.tv_cast);
        plot = findViewById(R.id.tv_plot);

        star1 = findViewById(R.id.iv_star_1);
        star2 = findViewById(R.id.iv_star_2);
        star3 = findViewById(R.id.iv_star_3);
        star4 = findViewById(R.id.iv_star_4);
        star5 = findViewById(R.id.iv_star_5);

        ProgressBar prMovies  = findViewById(R.id.pr_movies);
        TextView playMovie = findViewById(R.id.tv_play_movie);
        if (Boolean.TRUE.equals(dbHelper.checkSeek(DBHelper.TABLE_SEEK_MOVIE, streamID, streamName))){
            playMovie.setText(R.string.resume);
            try {
                long seekFull = dbHelper.getSeekFull(DBHelper.TABLE_SEEK_MOVIE, streamID, streamName);
                if (seekFull > 0){
                    prMovies.setVisibility(View.VISIBLE);
                    prMovies.setProgress(Math.toIntExact(seekFull));
                } else {
                    prMovies.setVisibility(View.GONE);
                }
            } catch (Exception e) {
                prMovies.setVisibility(View.GONE);
            }
        } else {
            playMovie.setText(R.string.play);
            prMovies.setVisibility(View.GONE);
        }

        setClickListener();
        getData();

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.ll_play_movie).requestFocus();
        }

        LinearLayout adView = findViewById(R.id.ll_adView);
        helper.showBannerAd(adView, Callback.getBannerMovie());

        // Initialize the permission launcher
        permissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted ->
                Toast.makeText(DetailsMovieActivity.this, Boolean.TRUE.equals(isGranted)
                        ? "Permission granted"
                        : getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show()
        );
    }

    private void setClickListener() {
        btnFav.setOnClickListener(v -> setFav());
        btnDownloadClose.setOnClickListener(v -> closeDownload());
        findViewById(R.id.ll_play_movie).setOnClickListener(v -> play());
        findViewById(R.id.ll_play_trailer).setOnClickListener(v -> setTrailer());
        findViewById(R.id.ll_images).setOnClickListener(v -> setImages());
        findViewById(R.id.iv_feedback).setOnClickListener(v -> {
            if (itemMovies == null) return;
            new FeedBackDialog(this).showDialog("Movies - "+itemMovies.getName());
        });
    }

    private void setImages() {
        if (itemMovies != null && !itemMovies.getTmdbID().isEmpty()) {
            Intent intent = new Intent(DetailsMovieActivity.this, GalleryActivity.class);
            intent.putExtra("tmdb_id", itemMovies.getTmdbID());
            startActivity(intent);
        }
    }

    private void closeDownload() {
        try {
            if (DownloadService.getArrayListVideo() != null && !DownloadService.getArrayListVideo().isEmpty()){
                Intent serviceIntent = new Intent(this, DownloadService.class);
                serviceIntent.setAction(DownloadService.ACTION_STOP);
                startService(serviceIntent);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setTrailer() {
        if (findViewById(R.id.pb_trailer).getVisibility() == View.GONE && itemMovies != null && !itemMovies.getYoutubeTrailer().isEmpty()) {
            String videoId;
            if (itemMovies.getYoutubeTrailer().contains("https://")){
                videoId = ApplicationUtil.getVideoId(itemMovies.getYoutubeTrailer());
            } else {
                videoId = itemMovies.getYoutubeTrailer();
            }
            Intent intent = new Intent(DetailsMovieActivity.this, YouTubePlayerActivity.class);
            intent.putExtra(TAG_STREAM_ID, videoId);
            startActivity(intent);
        }
    }

    private void setFav() {
        if (Boolean.TRUE.equals(dbHelper.checkMovie(DBHelper.TABLE_FAV_MOVIE, streamID))){
            dbHelper.removeMovie(DBHelper.TABLE_FAV_MOVIE, streamID);
            btnFav.setImageResource(R.drawable.ic_favorite_border);
            Toasty.makeText(this, true ,getResources().getString(R.string.fav_remove_success), Toasty.SUCCESS);
        } else {
            dbHelper.addToMovie(DBHelper.TABLE_FAV_MOVIE, new ItemMovies(streamName, streamID, streamIcon, streamRating,""), 0);
            btnFav.setImageResource(R.drawable.ic_favorite);
            Toasty.makeText(this, true ,getResources().getString(R.string.fav_success), Toasty.SUCCESS);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_details_movie;
    }

    private void getData() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(DetailsMovieActivity.this, true,getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        LoadMovieID loadSeriesID = new LoadMovieID(this, new MovieIDListener() {
            @Override
            public void onStart() {
                addShimmer();
            }

            @Override
            public void onEnd(String success, ArrayList<ItemInfoMovies> arrayListInfo,
                              ArrayList<ItemMoviesData> arrayListMoviesData) {
                if (isFinishing()){
                   return;
                }
                handleLoadResult(success, arrayListInfo, arrayListMoviesData);
            }
        }, ApplicationUtil.getAPIRequestID("get_vod_info","vod_id",
                streamID, spHelper.getUserName(), spHelper.getPassword()));
        loadSeriesID.execute();
    }

    private void handleLoadResult(@NonNull String success, ArrayList<ItemInfoMovies> arrayListInfo,
                                  ArrayList<ItemMoviesData> arrayListMoviesData) {
        if (success.equals("1")) {
            if (!arrayListInfo.isEmpty()){
                itemMovies = arrayListInfo.get(0);
            } else {
                itemMovies = new ItemInfoMovies(
                        "",streamName,streamIcon,"N/A", "0",
                        "","N/A","N/A","N/A","N/A",streamRating
                );
            }
            if (!arrayListMoviesData.isEmpty()){
                itemData =  arrayListMoviesData.get(0);
            }
            llPage.setVisibility(View.VISIBLE);
            removeShimmer();
            setInfo();
        }  else {
            if (playback < 3){
                playback = playback + 1;
                Toast.makeText(DetailsMovieActivity.this, "Server Error - "+ playback + "/3", Toast.LENGTH_SHORT).show();
                getData();
            } else {
                playback = 1;
                Toasty.makeText(DetailsMovieActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                removeShimmer();
            }
        }
    }

    private void removeShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            shimmer.setVisibility(View.GONE);
            shimmer.removeAllViews();
        } else {
            if (progressDialog.isShowing()){
                progressDialog.dismiss();
            }
        }
    }

    private void addShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            llPage.setVisibility(View.GONE);
            shimmer.setVisibility(View.VISIBLE);
            shimmer.removeAllViews();
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.load_movie_page, null);
            shimmer.addView(myView);
        } else {
            llPage.setVisibility(View.VISIBLE);
            if (!progressDialog.isShowing()){
                progressDialog.show();
            }
        }
    }

    private void setInfo() {
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.ll_play_movie).requestFocus();
        }
        if (itemData != null){
            if (Boolean.FALSE.equals(dbHelper.checkDownload(DBHelper.TABLE_DOWNLOAD_MOVIES, streamID,
                    ApplicationUtil.containerExtension(itemData.getContainerExtension())))) {
                btnDownload.setImageResource(R.drawable.iv_downloading);
            } else {
                btnDownload.setImageResource(R.drawable.ic_check);
            }
        } else {
            findViewById(R.id.ll_download).setVisibility(View.GONE);
        }
        if (!spHelper.getIsDownload()){
            findViewById(R.id.ll_download).setVisibility(View.GONE);
        } else {
            if (!spHelper.getIsDownloadUser()){
                findViewById(R.id.ll_download).setVisibility(View.GONE);
            }
        }
        seekUpdating();

        Picasso.get()
                .load(itemMovies.getMovieImage().isEmpty() ? "null" : itemMovies.getMovieImage())
                .placeholder(R.drawable.material_design_default)
                .error(R.drawable.material_design_default)
                .into(poster);

        ApplicationUtil.setRating(itemMovies.getRating(), star1, star2, star3, star4, star5);

        btnFav.setImageResource(Boolean.TRUE.equals(dbHelper.checkMovie(DBHelper.TABLE_FAV_MOVIE, streamID))
                ? R.drawable.ic_favorite
                : R.drawable.ic_favorite_border
        );

        pageTitle.setText(itemMovies.getName());
        directed.setText(itemMovies.getDirector().isEmpty() || itemMovies.getDirector().equals("null") ? "N/A" : itemMovies.getDirector());
        release.setText(itemMovies.getReleaseDate());
        genre.setText(itemMovies.getGenre());
        cast.setText(itemMovies.getCast());
        duration.setText(ApplicationUtil.timeFormat(itemMovies.getEpisodeRunTime()));
        plot.setText(itemMovies.getPlot());

        findViewById(R.id.ll_play_trailer).setVisibility(itemMovies.getYoutubeTrailer().isEmpty() ? View.GONE : View.VISIBLE);

        setupDownloadButton();
        setBlur();
        handleCastVisibility();
    }

    private void handleCastVisibility() {
        if (spHelper.getIsCast()){
            if (itemMovies.getTmdbID().isEmpty()){
                findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
                findViewById(R.id.rv_cast).setVisibility(View.GONE);
                findViewById(R.id.pb_cast).setVisibility(View.GONE);
            } else {
                getTmdb(itemMovies.getTmdbID());
            }
        } else {
            findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
            findViewById(R.id.rv_cast).setVisibility(View.GONE);
            findViewById(R.id.pb_cast).setVisibility(View.GONE);
        }
    }

    private void setupDownloadButton() {
        findViewById(R.id.ll_download).setOnClickListener(v -> {
            if (!Boolean.TRUE.equals(checkPer())) {
                checkPer();
                return;
            }

            if (itemData == null || !Boolean.TRUE.equals(checkPerNotification())) return;

            if (itemData.isDownload()){
                Toasty.makeText(this, true ,getResources().getString(R.string.already_download), Toasty.WARNING);
                return;
            }

            if (Boolean.FALSE.equals(dbHelper.checkDownload(DBHelper.TABLE_DOWNLOAD_MOVIES, streamID,
                    ApplicationUtil.containerExtension(itemData.getContainerExtension())))) {
                try{
                    itemData.setDownload(true);
                    String channelUrl = spHelper.getServerURL()+"movie/"+ spHelper.getUserName()+"/"+
                            spHelper.getPassword()+"/"+streamID+"."+itemData.getContainerExtension();
                    ItemVideoDownload download  = new ItemVideoDownload(streamName, streamID, streamIcon,
                            channelUrl, ApplicationUtil.containerExtension(itemData.getContainerExtension()));
                    helper.download(download, DBHelper.TABLE_DOWNLOAD_MOVIES);
                    new Handler().postDelayed(this::seekUpdating, 0);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } else {
                Toasty.makeText(this, true ,getResources().getString(R.string.downloading), Toasty.WARNING);
            }
        });
    }

    private void getTmdb(String movieId) {
        new AsyncTaskExecutor<String, String, String>(){

            final ArrayList<ItemCast> arrayListCast = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                super.onPreExecute();
                findViewById(R.id.tv_top_cast).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_cast).setVisibility(View.VISIBLE);
                findViewById(R.id.rv_cast).setVisibility(View.GONE);
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    String json = ApplicationUtil.getMovieCredits(movieId, spHelper.getTmdbKEY());
                    JSONObject jsonObject = new JSONObject(json);

                    JSONArray c =  jsonObject.getJSONArray("cast");
                    for (int i = 0; i < c.length(); i++) {
                        JSONObject objectCategory = c.getJSONObject(i);

                        String id = objectCategory.getString("id");
                        String name = objectCategory.getString("name");
                        String profile = objectCategory.getString("profile_path").isEmpty()
                                ? "null"
                                : objectCategory.getString("profile_path").replace(" ", "%20");

                        ItemCast objItem = new ItemCast(id,name,profile);
                        arrayListCast.add(objItem);
                    }
                    return "1";
                } catch (Exception ee) {
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                if (isFinishing()){
                    return;
                }
                findViewById(R.id.pb_cast).setVisibility(View.GONE);
                if (s.equals("1")){
                    setAdapterCast(arrayListCast);
                }
            }
        }.execute();
    }

    private void setAdapterCast(ArrayList<ItemCast> arrayListCast) {
        if (arrayListCast != null && !arrayListCast.isEmpty()){
            RecyclerView rvCast = findViewById(R.id.rv_cast);
            LinearLayoutManager linearLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
            rvCast.setLayoutManager(linearLayoutManager);
            rvCast.setItemAnimator(new DefaultItemAnimator());
            AdapterMovieCast adapterMovieCast = new AdapterMovieCast(arrayListCast, (itemCast, position) ->
                    Toast.makeText(DetailsMovieActivity.this, arrayListCast.get(position).getName(), Toast.LENGTH_SHORT).show());
            rvCast.setAdapter(adapterMovieCast);
            findViewById(R.id.tv_top_cast).setVisibility(View.VISIBLE);
            findViewById(R.id.rv_cast).setVisibility(View.VISIBLE);
        } else {
            findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
        }
    }

    private void setBlur() {
        ImageView blurView = findViewById(R.id.iv_bg_blur);
        if (itemMovies.getMovieImage() == null || itemMovies.getMovieImage().isEmpty()){
            blurView.setImageResource(themeBg);
            return;
        }

        try {
            Target target = new Target() {
                @Override
                public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                    if (isFinishing() || bitmap == null){
                        return;
                    }
                    try {
                        blurView.setImageBitmap(BlurImage.fastBlur(bitmap, 1f, spHelper.getBlurRadius()));
                    } catch (Exception e) {
                        Log.e(TAG, "onBitmapLoaded: ", e);
                    }
                }

                @Override
                public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                    if (isFinishing()){
                        return;
                    }
                    blurView.setImageResource(themeBg);
                }
                @Override
                public void onPrepareLoad(Drawable placeHolderDrawable) {
                    // Not used
                }
            };
            blurView.setTag(target);
            Picasso.get()
                    .load(itemMovies.getMovieImage().isEmpty() ? "null" : itemMovies.getMovieImage())
                    .placeholder(themeBg)
                    .into(target);
        } catch (Exception e) {
            blurView.setImageResource(themeBg);
        }
    }

    private void play() {
        if (itemData == null){
            return;
        }
        Intent intent = new Intent(DetailsMovieActivity.this, PlayerMovieActivity.class);
        intent.putExtra(TAG_STREAM_ID, itemData.getStreamID());
        intent.putExtra("movie_name", itemData.getName());
        intent.putExtra("container", itemData.getContainerExtension());
        intent.putExtra("stream_rating", streamRating);
        intent.putExtra("stream_icon", streamIcon);
        startActivity(intent);
    }

    @NonNull
    private Boolean checkPer() {
        String permission;
        if (Build.VERSION.SDK_INT >= 33) {
            permission = READ_MEDIA_VIDEO;
        } else if (Build.VERSION.SDK_INT >= 29) {
            permission = READ_EXTERNAL_STORAGE;
        } else {
            permission = WRITE_EXTERNAL_STORAGE;
        }
        if (ContextCompat.checkSelfPermission(DetailsMovieActivity.this, permission) != PackageManager.PERMISSION_GRANTED) {
            permissionLauncher.launch(permission);  // Request permission using the new API
            return false;
        }
        return true;
    }

    @NonNull
    private Boolean checkPerNotification() {
        if (!ApplicationUtil.isTvBox(this) && (android.os.Build.VERSION.SDK_INT >= 33)) {
            if (ContextCompat.checkSelfPermission(DetailsMovieActivity.this, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                permissionLauncher.launch(POST_NOTIFICATIONS);  // Request permission using the new API
                return false;
            } else {
                return true;
            }
        }
        return true;
    }

    private final Runnable run = this::seekUpdating;

    public void seekUpdating() {
        try {
            if (DownloadService.getArrayListVideo() != null && !DownloadService.getArrayListVideo().isEmpty()){
                boolean found = false;
                int pos = 0;

                // Find the position of the stream_id in arrayListVideo
                for (int i = 0; i < DownloadService.getArrayListVideo().size(); i++) {
                    if (streamID.equals(DownloadService.getArrayListVideo().get(i).getStreamID())) {
                        pos = i;
                        found = true;
                        break;
                    }
                }

                // If stream_id is found in arrayListVideo
                if (found) {
                    // Update itemData if not null
                    if (itemData != null) {
                        itemData.setDownload(true);
                    }

                    // Show progress bar and close button
                    pbDownload.setVisibility(View.VISIBLE);
                    btnDownloadClose.setVisibility(View.VISIBLE);

                    // Set progress to progress bar
                    pbDownload.setProgress(DownloadService.getArrayListVideo().get(pos).getProgress());
                } else {
                    // Hide progress bar and close button if stream_id is not found
                    hideDownloadViews();
                }
            } else {
                // Hide progress bar and close button if arrayListVideo is null or empty
                hideDownloadViews();
            }
        } catch (Exception e) {
            Log.e(TAG, "seekUpdating: ", e);
        } finally {
            // Schedule next update after 1 second (1000 milliseconds)
            seekHandler.removeCallbacks(run);
            seekHandler.postDelayed(run, 1000);
        }
    }

    private void hideDownloadViews() {
        pbDownload.setVisibility(View.GONE);
        btnDownloadClose.setVisibility(View.GONE);
    }

    @Override
    protected void onPause() {
        try {
            seekHandler.removeCallbacks(run);
        } catch (Exception e) {
            Log.e(TAG, "onPause: ", e);
        }
        super.onPause();
    }

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
}