package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.BlurImage;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterEpisodes;
import nemosofts.streambox.adapter.AdapterSeason;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.executor.LoadSeriesID;
import nemosofts.streambox.interfaces.SeriesIDListener;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemInfoSeasons;
import nemosofts.streambox.item.ItemSeasons;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;

public class DetailsSeriesActivity extends AppCompatActivity {

    Helper helper;
    private int playback = 0;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private String seriesID = "0";
    private String seriesName = "";
    private String seriesRating = "";
    private String seriesCover = "";
    private TextView pageTitle;
    private TextView directed;
    private TextView release;
    private TextView genre;
    private TextView plot;
    private ImageView poster;
    private ImageView star1;
    private ImageView star2;
    private ImageView star3;
    private ImageView star4;
    private ImageView star5;
    private ArrayList<ItemSeasons> arraySeasons;
    private ArrayList<ItemEpisodes> arrayAllEpisodes;
    private ArrayList<ItemEpisodes> arrayEpisodes;
    private RecyclerView rvEpisodes;
    private AdapterEpisodes adapterEpisodes;
    private String seasonID = "0";
    private String plotData = "";
    String youtubeURL = "";
    private ImageView btnFav;
    private int themeBg;
    private ProgressDialog progressDialog;
    private LinearLayout llPage;
    private FrameLayout shimmer;

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

        seriesID = getIntent().getStringExtra("series_id");
        seriesName = getIntent().getStringExtra("series_name");
        seriesRating = getIntent().getStringExtra("series_rating");
        seriesCover = getIntent().getStringExtra("series_cover");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        helper = new Helper(this, (position, type) -> openPlayerEpisodesActivity(position));

        arraySeasons = new ArrayList<>();
        arrayAllEpisodes = new ArrayList<>();
        arrayEpisodes = new ArrayList<>();

        progressDialog = new ProgressDialog(DetailsSeriesActivity.this, true);

        llPage = findViewById(R.id.ll_page);
        shimmer = findViewById(R.id.fl_shimmer);
        pageTitle = findViewById(R.id.tv_page_title);
        poster = findViewById(R.id.iv_series);
        directed = findViewById(R.id.tv_directed);
        release = findViewById(R.id.tv_release);
        genre = findViewById(R.id.tv_genre);
        plot = findViewById(R.id.tv_plot);
        btnFav = findViewById(R.id.iv_fav);

        star1 = findViewById(R.id.iv_star_1);
        star2 = findViewById(R.id.iv_star_2);
        star3 = findViewById(R.id.iv_star_3);
        star4 = findViewById(R.id.iv_star_4);
        star5 = findViewById(R.id.iv_star_5);

        rvEpisodes = findViewById(R.id.rv_episodes);
        LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rvEpisodes.setLayoutManager(manager);
        rvEpisodes.setNestedScrollingEnabled(false);

        btnFav.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, seriesID))){
                dbHelper.removeFavSeries(DBHelper.TABLE_FAV_SERIES, seriesID);
                btnFav.setImageResource(R.drawable.ic_favorite_border);
                Toasty.makeText(this, true ,getResources().getString(R.string.fav_remove_success), Toasty.SUCCESS);
            } else {
                ItemSeries itemSeries = new ItemSeries(seriesName,seriesID,seriesCover,seriesRating);
                dbHelper.addToSeries(DBHelper.TABLE_FAV_SERIES, itemSeries, 0);
                btnFav.setImageResource(R.drawable.ic_favorite);
                Toasty.makeText(this, true ,getResources().getString(R.string.fav_success), Toasty.SUCCESS);
            }
        });

        findViewById(R.id.ll_play_trailer).setOnClickListener(v -> openYouTubePlayerActivity());

        getData();

        LinearLayout adView = findViewById(R.id.ll_adView);
        helper.showBannerAd(adView, Callback.getBannerSeries());
    }

    private void openYouTubePlayerActivity() {
        if (findViewById(R.id.pb_trailer).getVisibility() == View.GONE && youtubeURL != null && !youtubeURL.isEmpty()) {
            Intent intent = new Intent(DetailsSeriesActivity.this, YouTubePlayerActivity.class);
            intent.putExtra("stream_id", youtubeURL);
            startActivity(intent);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openPlayerEpisodesActivity(int position) {
        if (arrayEpisodes.isEmpty()){
            return;
        }
        Callback.setPlayPosEpisodes(position);
        if (!Callback.getArrayListEpisodes().isEmpty()) {
            Callback.getArrayListEpisodes().clear();
        }
        Callback.setArrayListEpisodes(arrayEpisodes);
        startActivity(new Intent(DetailsSeriesActivity.this, PlayerEpisodesActivity.class));
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_details_series;
    }

    private void getData() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(DetailsSeriesActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        LoadSeriesID loadSeriesID = new LoadSeriesID(this, new SeriesIDListener() {
            @Override
            public void onStart() {
                addShimmer();
            }

            @Override
            public void onEnd(String success,
                              ItemInfoSeasons infoSeasons, ArrayList<ItemSeasons> arrayListSeasons,
                              ArrayList<ItemEpisodes> arrayListEpisodes) {
                if (isFinishing()){
                    return;
                }
                handleLoadSeriesResult(success, infoSeasons, arrayListSeasons, arrayListEpisodes);
            }
        }, ApplicationUtil.getAPIRequestID("get_series_info","series_id", seriesID,
                spHelper.getUserName(), spHelper.getPassword()));
        loadSeriesID.execute();
    }

    private void handleLoadSeriesResult(@NonNull String success, ItemInfoSeasons infoSeasons,
                                        ArrayList<ItemSeasons> arrayListSeasons,
                                        ArrayList<ItemEpisodes> arrayListEpisodes) {
        if (success.equals("1")) {
            if (infoSeasons != null){
                plotData = infoSeasons.getPlot();
                setInfo(infoSeasons);
            }
            if (!arrayListEpisodes.isEmpty()){
                arrayAllEpisodes.addAll(arrayListEpisodes);
            }
            if (!arrayListSeasons.isEmpty()){
                arraySeasons.addAll(arrayListSeasons);
            }

            llPage.setVisibility(View.VISIBLE);
            removeShimmer();
            setSeasonsAdapter();
        }  else {
            if (playback < 2){
                playback = playback + 1;
                Toast.makeText(DetailsSeriesActivity.this, "Checking error - "+ playback +"/2", Toast.LENGTH_SHORT).show();
                getData();
            } else {
                removeShimmer();
                playback = 1;
                Toasty.makeText(DetailsSeriesActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
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
            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.load_series_page, null);
            shimmer.addView(myView);
        } else {
            llPage.setVisibility(View.VISIBLE);
            if (!progressDialog.isShowing()){
                progressDialog.show();
            }
        }
    }

    private void setSeasonsAdapter() {
        RecyclerView rvSeasons = findViewById(R.id.rv_seasons);
        LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rvSeasons.setLayoutManager(manager);
        rvSeasons.setNestedScrollingEnabled(false);

        if (!arraySeasons.isEmpty()) {
            AdapterSeason adapterColors = new AdapterSeason(this, arraySeasons, (itemSeasons, position) -> {
                seasonID = arraySeasons.get(position).getSeasonNumber();
                setSeasonAdapter();
            });
            rvSeasons.setAdapter(adapterColors);
            seasonID = arraySeasons.get(0).getSeasonNumber();
            setSeasonAdapter();
            if (ApplicationUtil.isTvBox(this)){
                rvSeasons.requestFocus();
            }
        } else {
            Toasty.makeText(this, true ,getResources().getString(R.string.err_no_data_found), Toasty.WARNING);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setSeasonAdapter() {
        if (arrayAllEpisodes.isEmpty()) {
            findViewById(R.id.tv_empty_msg).setVisibility(View.VISIBLE);
            return;
        }

        if (!arrayEpisodes.isEmpty()){
            arrayEpisodes.clear();
        }

        if (!seasonID.equals("0")){
            for (ItemEpisodes episode : arrayAllEpisodes) {
                if (episode.getSeason().equals(seasonID)) {
                    arrayEpisodes.add(episode);
                }
            }
        } else {
            arrayEpisodes.addAll(arrayAllEpisodes);
        }

        if (!arrayEpisodes.isEmpty()){
            adapterEpisodes = new AdapterEpisodes(this, arrayEpisodes,
                    seriesCover , plotData, (itemEpisodes, position) -> helper.showInterAd(position,"")
            );
            rvEpisodes.setAdapter(adapterEpisodes);
        } else if (adapterEpisodes != null){
            adapterEpisodes.notifyDataSetChanged();
        }
        findViewById(R.id.tv_empty_msg).setVisibility(arrayEpisodes.isEmpty()? View.VISIBLE : View.GONE);
    }

    private void setInfo(ItemInfoSeasons itemInfoSeasons) {
        if (itemInfoSeasons == null){
            return;
        }

        findViewById(R.id.iv_feedback).setOnClickListener(v -> new FeedBackDialog(this).showDialog("Series - "+itemInfoSeasons.getName()));

        pageTitle.setText(itemInfoSeasons.getName());
        directed.setText(itemInfoSeasons.getDirector().isEmpty() || itemInfoSeasons.getDirector().equals("null") ? "N/A" : itemInfoSeasons.getDirector());
        release.setText(itemInfoSeasons.getReleaseDate());
        genre.setText(itemInfoSeasons.getGenre().isEmpty() || itemInfoSeasons.getGenre().equals("null") ? "N/A" : itemInfoSeasons.getGenre());
        plot.setText(itemInfoSeasons.getPlot());

        btnFav.setImageResource(Boolean.TRUE.equals(dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, seriesID))
                ? R.drawable.ic_favorite
                : R.drawable.ic_favorite_border
        );

        Picasso.get()
                .load(itemInfoSeasons.getCover().isEmpty() ? "null" : itemInfoSeasons.getCover())
                .placeholder(R.drawable.material_design_default)
                .error(R.drawable.material_design_default)
                .into(poster);

        ApplicationUtil.setRating(itemInfoSeasons.getRating5based(), star1, star2, star3, star4, star5);

        setBlur(seriesCover);

        if (itemInfoSeasons.getYoutubeTrailer().isEmpty()){
            findViewById(R.id.ll_play_trailer).setVisibility(View.GONE);
        } else {
            findViewById(R.id.ll_play_trailer).setVisibility(View.VISIBLE);
            if (itemInfoSeasons.getYoutubeTrailer().contains("https://")){
                youtubeURL = ApplicationUtil.getVideoId(itemInfoSeasons.getYoutubeTrailer());
            } else {
                youtubeURL = itemInfoSeasons.getYoutubeTrailer();
            }
        }

        try {
            ItemSeries itemSeries = new ItemSeries(seriesName,seriesID,seriesCover,seriesRating);
            dbHelper.addToSeries(DBHelper.TABLE_RECENT_SERIES, itemSeries, spHelper.getMovieLimit());
        } catch (Exception e) {
            Log.e("DetailsSeriesActivity", "setInfo", e);
        }
    }

    private void setBlur(String cover) {
        ImageView blurView = findViewById(R.id.iv_bg_blur);
        if (cover == null){
            blurView.setImageResource(themeBg);
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
                        Log.e("DetailsSeriesActivity", "onBitmapLoaded", e);
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
                    // this method is empty
                }
            };
            blurView.setTag(target);
            Picasso.get()
                    .load(cover)
                    .placeholder(themeBg)
                    .into(target);

        } catch (Exception e) {
            blurView.setImageResource(themeBg);
        }
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
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