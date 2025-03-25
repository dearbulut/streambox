package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCategory;
import nemosofts.streambox.adapter.AdapterMovie;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.GetCategory;
import nemosofts.streambox.executor.GetMoviesPlaylist;
import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.recycler.EndlessRecyclerViewScrollListener;

public class PlaylistMovieActivity extends AppCompatActivity {

    Helper helper;
    private ProgressDialog progressDialog;
    // Category
    private AdapterCategory adapterCategory;
    private RecyclerView rvCat;
    private ArrayList<ItemCat> arrayListCat;
    // Movies
    private FrameLayout frameLayout;
    private Boolean isOver = false;
    private Boolean isScroll = false;
    private Boolean isLoading = false;
    private int page = 1;
    private String catName = "";
    private AdapterMovie adapter;
    private ArrayList<ItemMovies> arrayList;
    private RecyclerView rv;
    private ProgressBar pb;
    private GetMoviesPlaylist loadMovies;
    private int pos = 0;

    @OptIn(markerClass = UnstableApi.class)
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

        TextView title = findViewById(R.id.tv_page_title);
        title.setText(getString(R.string.movies_home));

        // Initialize UI components
        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        rvCat = findViewById(R.id.rv_cat);

        progressDialog = new ProgressDialog(PlaylistMovieActivity.this, true);
        helper = new Helper(this, (position, type) -> openPlayerSingleURLActivity(position));

        initRecyclerViews();
        initListeners();

        // Initialize data lists
        arrayList = new ArrayList<>();
        arrayListCat = new ArrayList<>();

        // Fetch initial data
        new Handler(Looper.getMainLooper()).postDelayed(this::getDataCat, 0);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openPlayerSingleURLActivity(int position) {
        Intent intent = new Intent(PlaylistMovieActivity.this, PlayerSingleURLActivity.class);
        intent.putExtra("channel_title", arrayList.get(position).getName());
        intent.putExtra("channel_url", arrayList.get(position).getStreamID());
        startActivity(intent);
    }

    private void initRecyclerViews() {
        GridLayoutManager grid = new GridLayoutManager(this, 1);
        grid.setSpanCount(ApplicationUtil.isTvBox(this) ? 6 : 5);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.addOnScrollListener(new EndlessRecyclerViewScrollListener(grid) {
            @Override
            public void onLoadMore(int p, int totalItemsCount) {
                if ((Boolean.FALSE.equals(isOver) && (Boolean.FALSE.equals(isLoading)))) {
                    isLoading = true;
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        isScroll = true;
                        getData();
                    }, 0);
                }
            }
        });

        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rvCat.setLayoutManager(llm);
        rvCat.setItemAnimator(new DefaultItemAnimator());
    }

    private void initListeners() {
        findViewById(R.id.iv_filter).setOnClickListener(v -> DialogUtil.filterDialog(this, 2, () ->
                new Handler(Looper.getMainLooper()).postDelayed(() -> recreateData(pos), 0))
        );
        findViewById(R.id.iv_search).setOnClickListener(view -> {
            Intent intent = new Intent(PlaylistMovieActivity.this, SearchActivity.class);
            intent.putExtra("page", "MoviePlaylist");
            startActivity(intent);
        });
    }

    private void getDataCat() {
         new GetCategory(this, 5, new GetCategoryListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ArrayList<ItemCat> itemCat) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1") && !itemCat.isEmpty()) {
                    if (!arrayListCat.isEmpty()){
                        arrayListCat.clear();
                    }
                    arrayListCat.addAll(itemCat);
                    catName = itemCat.get(0).getName();
                    setAdapterToCatListview();
                } else {
                    setEmpty();
                }
            }
        }).execute();
    }

    private void getData() {
        loadMovies = new GetMoviesPlaylist(this, page, catName, new GetMovieListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                if (!isFinishing() && Boolean.FALSE.equals(isOver)){
                    pb.setVisibility(View.GONE);
                    if (success.equals("1")) {
                        if (arrayListMovies.isEmpty()) {
                            isOver = true;
                            setEmpty();
                        } else {
                            page = page + 1;
                            arrayList.addAll(arrayListMovies);
                            setAdapterToListview();
                        }
                    } else {
                        setEmpty();
                    }
                    isLoading = false;
                }
            }
        });
        loadMovies.execute();
    }

    public void setAdapterToCatListview() {
        adapterCategory = new AdapterCategory(this, arrayListCat, position ->
                new Handler(Looper.getMainLooper()).postDelayed(() -> recreateData(position), 0)
        );
        rvCat.setAdapter(adapterCategory);
        adapterCategory.select(0);
        catName = arrayListCat.get(0).getName();
        pos = 0;

        // Handle adult content verification dialog or immediate data fetch
        handleAdultContentVerification();

        // Set up search functionality
        setupSearchFunctionality();
    }

    private void setupSearchFunctionality() {
        EditText edtSearch = findViewById(R.id.edt_search);
        edtSearch.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_SEARCH) {
                InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                inputManager.hideSoftInputFromWindow(Objects.requireNonNull(this.getCurrentFocus()).getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
            }
            return true;
        });
        edtSearch.addTextChangedListener(searchWatcher);
    }

    TextWatcher searchWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            // this method is empty
        }

        @SuppressLint("NotifyDataSetChanged")
        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (adapterCategory != null) {
                adapterCategory.getFilter().filter(s.toString());
                adapterCategory.notifyDataSetChanged();
            }
        }

        @Override
        public void afterTextChanged(Editable s) {
            // this method is empty
        }
    };

    @SuppressLint("NotifyDataSetChanged")
    private void recreateData(int position) {
        if (position >= 0 && position < arrayListCat.size()) {
            pos = position;
            catName = arrayListCat.get(position).getName();
            adapterCategory.select(position);

            // Cancel any ongoing task
            if (loadMovies != null) {
                loadMovies.shutDown();
            }
            isOver = true;

            // Clear the list
            if (!arrayList.isEmpty()){
                arrayList.clear();
            }

            // Notify adapter of data change
            if (adapter != null){
                adapter.notifyDataSetChanged();
            }

            // Reset pagination and fetch new data
            new Handler(Looper.getMainLooper()).postDelayed(this::resetPaginationAndFetchData, 0);
        }
    }

    private void resetPaginationAndFetchData() {
        isOver = false;
        isScroll = false;
        isLoading = false;
        page = 1;

        // Handle adult content verification dialog or immediate data fetch
        handleAdultContentVerification();
    }

    private void handleAdultContentVerification() {
        if (ApplicationUtil.isAdultsCount(arrayListCat.get(pos).getName())) {
            DialogUtil.childCountDialog(this, pos, position -> getData());
        } else {
            // Delayed data fetch if not adult content
            new Handler(Looper.getMainLooper()).postDelayed(this::getData, 0);
        }
    }

    public void setAdapterToListview() {
        if(Boolean.FALSE.equals(isScroll)) {
            adapter = new AdapterMovie(this, arrayList, (itemCat, position) -> helper.showInterAd(position,""));
            rv.setAdapter(adapter);
            setEmpty();
        } else {
            adapter.notifyItemInserted(arrayList.size()-1);
        }
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_channel;
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

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
    }
}