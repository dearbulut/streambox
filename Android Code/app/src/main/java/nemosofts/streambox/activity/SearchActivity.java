package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterChannel;
import nemosofts.streambox.adapter.AdapterMovie;
import nemosofts.streambox.adapter.AdapterSeries;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.GetChannelSearch;
import nemosofts.streambox.executor.GetMovieSearch;
import nemosofts.streambox.executor.GetSeriesSearch;
import nemosofts.streambox.interfaces.GetChannelListener;
import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SearchActivity extends AppCompatActivity {

    private EditText edtSearch;
    private FrameLayout frameLayout;
    private RecyclerView rv;
    private ProgressBar pb;
    private String page;

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

        page = getIntent().getStringExtra("page");

        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 1);
        grid.setSpanCount(6);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        edtSearch = findViewById(R.id.edt_search);
        edtSearch.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_SEARCH){
                InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                inputManager.hideSoftInputFromWindow(Objects.requireNonNull(this.getCurrentFocus()).getWindowToken(),InputMethodManager.HIDE_NOT_ALWAYS);
                searchData();
            }
            return true;
        });

        if (ApplicationUtil.isTvBox(this)){
            edtSearch.requestFocus();
        }

        findViewById(R.id.iv_search).setOnClickListener(view -> searchData());
    }

    private void searchData() {
        switch (page) {
            case "Movie" :
                searchMovie(false);
                break;
            case "MoviePlaylist" :
                searchMovie(true);
                break;
            case "Live" :
                searchLive(false);
                break;
            case "LivePlaylist" :
                searchLive(true);
                break;
            case "Series" :
                searchSeries();
                break;
            default :
                break;
        }
    }

    private void searchSeries() {
        new GetSeriesSearch(this, edtSearch.getText().toString(), new GetSeriesListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemSeries> arrayListSeries) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (success.equals("1")) {
                    if (arrayListSeries.isEmpty()) {
                        setEmpty(true);
                        Toasty.makeText(SearchActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
                    } else {
                        setAdapterToSeries(arrayListSeries);
                    }
                } else {
                    Toasty.makeText(SearchActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                    setEmpty(true);
                }
            }
        }).execute();
    }

    private void searchMovie(boolean isPlaylist) {
        new GetMovieSearch(this, isPlaylist, edtSearch.getText().toString(), new GetMovieListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (success.equals("1")) {
                    if (arrayListMovies.isEmpty()) {
                        setEmpty(true);
                        Toasty.makeText(SearchActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
                    } else {
                        setAdapterToMovies(arrayListMovies);
                    }
                } else {
                    Toasty.makeText(SearchActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                    setEmpty(true);
                }
            }
        }).execute();
    }

    private void searchLive(boolean isPlaylist) {
        new GetChannelSearch(this, isPlaylist, edtSearch.getText().toString(), new GetChannelListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemChannel> arrayListLive) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (success.equals("1")) {
                    if (arrayListLive.isEmpty()) {
                        setEmpty(true);
                        Toasty.makeText(SearchActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
                    } else {
                        setAdapterToLive(arrayListLive);
                    }
                } else {
                    Toasty.makeText(SearchActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                    setEmpty(true);
                }
            }
        }).execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToMovies(ArrayList<ItemMovies> arrayListMovies) {
        AdapterMovie adapter = new AdapterMovie(this, arrayListMovies, (itemCat, position) -> {
            Intent intent;
            if (new SPHelper(this).getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                intent = new Intent(SearchActivity.this, PlayerSingleURLActivity.class);
                intent.putExtra("channel_title", arrayListMovies.get(0).getName());
                intent.putExtra("channel_url", arrayListMovies.get(0).getStreamID());
                startActivity(intent);
            } else {
                intent = new Intent(this, DetailsMovieActivity.class);
                intent.putExtra("stream_id", arrayListMovies.get(position).getStreamID());
                intent.putExtra("stream_name", arrayListMovies.get(position).getName());
                intent.putExtra("stream_icon", arrayListMovies.get(position).getStreamIcon());
                intent.putExtra("stream_rating", arrayListMovies.get(position).getRating());
                startActivity(intent);
            }
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToLive(ArrayList<ItemChannel> arrayListLive) {
        AdapterChannel adapter = new AdapterChannel(this, arrayListLive, (itemCat, position) -> {
            Intent intent = new Intent(SearchActivity.this, PlayerLiveActivity.class);
            Callback.setPlayPosLive(position);
            if (!Callback.getArrayListLive().isEmpty()) {
                Callback.getArrayListLive().clear();
            }
            Callback.setArrayListLive(arrayListLive);

            if (ApplicationUtil.isAdultsCount(arrayListLive.get(position).getName())) {
                DialogUtil.childCountDialog(this, position, pos -> startActivity(intent));
            } else {
                startActivity(intent);
            }
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    private void setAdapterToSeries(ArrayList<ItemSeries> arrayListSeries) {
        AdapterSeries adapter = new AdapterSeries(this, arrayListSeries, (itemCat, position) -> {
            Intent intent = new Intent(this, DetailsSeriesActivity.class);
            intent.putExtra("series_id", arrayListSeries.get(position).getSeriesID());
            intent.putExtra("series_name", arrayListSeries.get(position).getName());
            intent.putExtra("series_rating", arrayListSeries.get(position).getRating());
            intent.putExtra("series_cover", arrayListSeries.get(position).getCover());
            startActivity(intent);
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    private void setEmpty(Boolean isEmpty) {
        if (Boolean.FALSE.equals(isEmpty)) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
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

    @Override
    public int setContentViewID() {
        return R.layout.activity_search;
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