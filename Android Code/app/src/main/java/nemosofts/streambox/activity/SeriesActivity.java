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
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCategory;
import nemosofts.streambox.adapter.AdapterSeries;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.GetCategory;
import nemosofts.streambox.executor.GetSeries;
import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.recycler.EndlessRecyclerViewScrollListener;

public class SeriesActivity extends AppCompatActivity {

    Helper helper;
    private FrameLayout frameLayout;
    private ProgressDialog progressDialog;
    // Category
    private AdapterCategory adapterCategory;
    private RecyclerView rvCat;
    private ArrayList<ItemCat> arrayListCat;
    // Series
    private Boolean isOver = false;
    private Boolean isScroll = false;
    private Boolean isLoading = false;
    private int page = 1;
    private String catID = "0";
    private AdapterSeries adapter;
    private ArrayList<ItemSeries> arrayList;
    private RecyclerView rv;
    private ProgressBar pb;
    private int isPage = 0;
    private GetSeries loadSeries;
    private int pos = 1;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        TextView title = findViewById(R.id.tv_page_title);
        title.setText(getString(R.string.series_home));

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));
        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        // Initialize UI components
        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        rvCat = findViewById(R.id.rv_cat);

        // Initialize helpers and dialogs
        progressDialog = new ProgressDialog(SeriesActivity.this, true);
        helper = new Helper(this, (position, type) -> openDetailsSeriesActivity(position));

        // Initialize RecyclerViews
        initRecyclerViews();

        // Initialize listeners
        initListeners();

        // Initialize data lists
        arrayList = new ArrayList<>();
        arrayListCat = new ArrayList<>();

        // Fetch initial data
        new Handler(Looper.getMainLooper()).postDelayed(this::getDataCat, 0);
    }

    private void openDetailsSeriesActivity(int position) {
        Intent intent = new Intent(this, DetailsSeriesActivity.class);
        intent.putExtra("series_id", arrayList.get(position).getSeriesID());
        intent.putExtra("series_name", arrayList.get(position).getName());
        intent.putExtra("series_rating", arrayList.get(position).getRating());
        intent.putExtra("series_cover", arrayList.get(position).getCover());
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
                if (Boolean.TRUE.equals(isPage == 0) && (Boolean.FALSE.equals(isOver) && (Boolean.FALSE.equals(isLoading)))) {
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
        findViewById(R.id.iv_filter).setOnClickListener(v -> DialogUtil.filterDialog(this, 3, () -> recreateData(pos)));
        findViewById(R.id.iv_search).setOnClickListener(view -> {
            Intent intent = new Intent(SeriesActivity.this, SearchActivity.class);
            intent.putExtra("page", "Series");
            startActivity(intent);
        });
    }

    private void getDataCat() {
        new GetCategory(this, 3, new GetCategoryListener() {
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
                    arrayListCat.add(new ItemCat("01",getString(R.string.favourite),""));
                    arrayListCat.add(new ItemCat("02",getString(R.string.recently),""));
                    arrayListCat.add(new ItemCat("03",getString(R.string.recently_add),""));
                    arrayListCat.addAll(itemCat);
                    catID = itemCat.get(0).getId();
                    setAdapterToCatListview();
                } else {
                    setEmpty();
                }
            }
        }).execute();
    }

    private void getData() {
        loadSeries = new GetSeries(this, page,  catID, isPage, new GetSeriesListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemSeries> arrayListSeries) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (Boolean.FALSE.equals(isOver)){
                    if (success.equals("1")) {
                        if (arrayListSeries.isEmpty()) {
                            isOver = true;
                            setEmpty();
                        } else {
                            arrayList.addAll(arrayListSeries);
                            page = page + 1;
                            setAdapterToListview();
                        }
                    } else {
                        setEmpty();
                    }
                    isLoading = false;
                }
            }
        });
        loadSeries.execute();
    }

    public void setAdapterToCatListview() {
        adapterCategory = new AdapterCategory(this, arrayListCat, position -> {
            if (pos != position){
                recreateData(position);
            }
        });
        rvCat.setAdapter(adapterCategory);
        adapterCategory.select(3);
        pos = 3;

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
                View currentFocus = this.getCurrentFocus();
                if (currentFocus != null) {
                    inputManager.hideSoftInputFromWindow(currentFocus.getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
                }
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
            catID = arrayListCat.get(position).getId();
            adapterCategory.select(position);

            // Cancel any ongoing task
            if (loadSeries != null) {
                loadSeries.shutDown();
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

            // Determine page type based on category
            determinePageType(position);

            // Reset pagination and fetch new data
            new Handler(Looper.getMainLooper()).postDelayed(this::resetPaginationAndFetchData, 0);
        }
    }

    private void determinePageType(int position) {
        switch (arrayListCat.get(position).getId()) {
            case "01":
                isPage = 1; // Favorites
                break;
            case "02":
                isPage = 2; // Recently watched
                break;
            case "03":
                isPage = 3; // Recently added
                break;
            default:
                isPage = 0; // Default category
                break;
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
            adapter = new AdapterSeries(this, arrayList, (itemCat, position) -> helper.showInterAd(position,""));
            rv.setAdapter(adapter);
            rv.scheduleLayoutAnimation();
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