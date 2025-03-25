package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.View;

import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterClear;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemSetting;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingClearDataActivity extends AppCompatActivity {

    SPHelper spHelper;
    private DBHelper dbHelper;
    private RecyclerView rv;
    private Boolean isTvBox;
    private String cacheSize;
    private AdapterClear adapter;
    private ArrayList<ItemSetting> arrayList;
    private ProgressDialog progressDialog;
    private Boolean isClearChannels = false;
    private Boolean isClearMovies = false;
    private Boolean isClearSeries = false;
    private Boolean isPlaybackSeries = false;
    private Boolean isPlaybackEpisodes = false;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        isTvBox  = ApplicationUtil.isTvBox(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        progressDialog = new ProgressDialog(SettingClearDataActivity.this, true);

        arrayList = new ArrayList<>();

        initializeCache();

        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);

        setAdapterToListview();
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setAdapterToListview() {
        if (!arrayList.isEmpty()){
            arrayList.clear();
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.clear_cache)+" "+cacheSize, R.drawable.ic_clean_code));

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.clear_channels), R.drawable.ic_trash));
            arrayList.add(new ItemSetting(getResources().getString(R.string.clear_movies), R.drawable.ic_trash));
            arrayList.add(new ItemSetting(getResources().getString(R.string.clear_series), R.drawable.ic_trash));

            arrayList.add(new ItemSetting(getResources().getString(R.string.clear_playback_movies), R.drawable.ic_trash));
            arrayList.add(new ItemSetting(getResources().getString(R.string.clear_playback_episodes), R.drawable.ic_trash));
        }

        if (adapter == null){
            adapter = new AdapterClear(arrayList, position -> setOnClick(arrayList.get(position).getName()));
            rv.setAdapter(adapter);
            if (Boolean.TRUE.equals(isTvBox)){
                rv.requestFocus();
            }
        } else {
            adapter.notifyDataSetChanged();
        }
    }

    private void setOnClick(String name) {
        if (name == null){
            return;
        }
        final String SUCCESS = " Success";
        if (name.equals(getResources().getString(R.string.clear_channels))){
            if (Boolean.TRUE.equals(isClearChannels)){
                Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
                return;
            }
            isClearChannels = true;
            progressDialog.show();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                dbHelper.clearData(DBHelper.TABLE_RECENT_LIVE);
                progressDialog.dismiss();
                Toasty.makeText(this,true, getResources().getString(R.string.clear_channels) + SUCCESS, Toasty.SUCCESS);
            }, 500);
        } else if (name.equals(getResources().getString(R.string.clear_movies))){
            if (Boolean.TRUE.equals(isClearMovies)){
                Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
                return;
            }
            isClearMovies = true;
            progressDialog.show();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                dbHelper.clearData(DBHelper.TABLE_RECENT_MOVIE);
                progressDialog.dismiss();
                Toasty.makeText(this, true,getResources().getString(R.string.clear_movies)+SUCCESS, Toasty.SUCCESS);
            }, 500);
        } else if (name.equals(getResources().getString(R.string.clear_series))){
            if (Boolean.TRUE.equals(isClearSeries)){
                Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
                return;
            }
            isClearSeries = true;
            progressDialog.show();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                dbHelper.clearData(DBHelper.TABLE_RECENT_SERIES);
                progressDialog.dismiss();
                Toasty.makeText(this,true, getResources().getString(R.string.clear_series)+SUCCESS, Toasty.SUCCESS);
            }, 500);
        } else if (name.equals(getResources().getString(R.string.clear_playback_movies))){
            if (Boolean.TRUE.equals(isPlaybackSeries)){
                Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
                return;
            }
            isPlaybackSeries = true;
            progressDialog.show();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                dbHelper.clearData(DBHelper.TABLE_SEEK_MOVIE);
                progressDialog.dismiss();
                Toasty.makeText(this,true, getResources().getString(R.string.clear_playback_movies)+SUCCESS, Toasty.SUCCESS);
            }, 500);
        } else if (name.equals(getResources().getString(R.string.clear_playback_episodes))){
            if (Boolean.TRUE.equals(isPlaybackEpisodes)){
                Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
                return;
            }
            isPlaybackEpisodes = true;
            progressDialog.show();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                dbHelper.clearData(DBHelper.TABLE_SEEK_EPISODES);
                progressDialog.dismiss();
                Toasty.makeText(this,true, getResources().getString(R.string.clear_playback_episodes)+SUCCESS, Toasty.SUCCESS);
            }, 500);
        } else {
            clearCache();
        }
    }

    private void initializeCache() {
        try {
            long size = 0;
            size += getDirSize(this.getCacheDir());
            size += getDirSize(this.getExternalCacheDir());
            cacheSize = ApplicationUtil.readableFileSize(size);
        } catch (Exception e) {
            cacheSize ="0 MB";
        }
    }

    private long getDirSize(File dir) {
        long size = 0;
        try {
            for (File file : Objects.requireNonNull(dir.listFiles())) {
                if (file != null && file.isDirectory()) {
                    size += getDirSize(file);
                } else if (file != null && file.isFile()) {
                    size += file.length();
                }
            }
        } catch (Exception e) {
            return size;
        }
        return size;
    }

    private void clearCache() {
        if (cacheSize.equals("0 MB")){
            Toasty.makeText(this,true, getResources().getString(R.string.already_cleared), Toasty.WARNING);
            return;
        }
        new AsyncTaskExecutor<String, String, String>() {
            @Override
            protected void onPreExecute() {
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    FileUtils.deleteQuietly(getCacheDir());
                    FileUtils.deleteQuietly(getExternalCacheDir());
                    return "1";
                } catch (Exception e) {
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                progressDialog.dismiss();
                cacheSize = "0 MB";
                setAdapterToListview();
                Toasty.makeText(SettingClearDataActivity.this,true, getResources().getString(R.string.clear_cache)+" Success", Toasty.SUCCESS);
            }
        }.execute();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting_clear_data;
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            Log.e("SettingClearDataActivity", "Error dbHelper close",e);
        }
        super.onDestroy();
    }

}