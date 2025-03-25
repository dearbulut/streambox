package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.StaggeredGridLayoutManager;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterWallpaper;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadWallpaper;
import nemosofts.streambox.interfaces.WallpaperListener;
import nemosofts.streambox.item.ItemWallpaper;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;

public class GalleryActivity extends AppCompatActivity {

    private ProgressBar pb;
    private RecyclerView rv;
    private FrameLayout frameLayout;
    private ArrayList<ItemWallpaper> arrayList;
    private String errorMsg;

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

        String tmdbID = getIntent().getStringExtra("tmdb_id");

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        pb = findViewById(R.id.pb);

        StaggeredGridLayoutManager grid = new StaggeredGridLayoutManager(ApplicationUtil.isTvBox(this) ? 6 : 5, StaggeredGridLayoutManager.VERTICAL);
        grid.setGapStrategy(StaggeredGridLayoutManager.GAP_HANDLING_MOVE_ITEMS_BETWEEN_SPANS);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setLayoutManager(grid);

        getData(tmdbID);
    }

    private void getData(String tmdbID) {
        if (!NetworkUtils.isConnected(this)){
            errorMsg = getString(R.string.err_internet_not_connected);
            setEmpty();
            return;
        }
        LoadWallpaper loadWallpaper = new LoadWallpaper(this, tmdbID, new WallpaperListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()) {
                    frameLayout.setVisibility(View.GONE);
                    pb.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemWallpaper> arrayListWallpaper) {
                if (success.equals("1")) {
                    if (!arrayListWallpaper.isEmpty()) {
                        arrayList.addAll(arrayListWallpaper);
                        setAdapter();
                    } else {
                        errorMsg = getString(R.string.err_no_data_found);
                        setEmpty();
                    }
                } else {
                    errorMsg = getString(R.string.err_server_not_connected);
                    setEmpty();
                }
            }
        });
        loadWallpaper.execute();
    }

    private void setAdapter() {
        AdapterWallpaper adapter = new AdapterWallpaper(arrayList, position ->
                DialogUtil.wallpaperDialog(GalleryActivity.this, arrayList.get(position).getFilePath())
        );
        rv.setAdapter(adapter);
        rv.scheduleLayoutAnimation();
        setEmpty();
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            pb.setVisibility(View.INVISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);
            pb.setVisibility(View.INVISIBLE);

            frameLayout.removeAllViews();
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            TextView textView = myView.findViewById(R.id.tv_empty_msg_title);
            textView.setText(errorMsg);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_gallery;
    }
}