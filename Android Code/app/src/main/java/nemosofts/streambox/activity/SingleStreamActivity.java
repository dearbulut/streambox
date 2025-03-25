package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterSingleURL;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemSingleURL;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.advertising.AdManagerInterAdmob;
import nemosofts.streambox.util.advertising.GDPRChecker;
import nemosofts.streambox.util.advertising.RewardAdAdmob;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class SingleStreamActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private RecyclerView rv;
    private ArrayList<ItemSingleURL> arrayList;
    private FrameLayout frameLayout;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(SingleStreamActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        dbHelper = new DBHelper(this);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);

        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        getData();
        setListener();
        getNetworkInfo();

        setAds();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isFinishing()){
                return;
            }
            DialogUtil.popupAdsDialog(SingleStreamActivity.this);
        }, 600);
    }

    private void setAds() {
        if (ApplicationUtil.isTvBox(this)){
            return;
        }
        new GDPRChecker(SingleStreamActivity.this).check();
        if (Boolean.TRUE.equals(Callback.getRewardAdMovie() || Callback.getRewardAdEpisodes()
                || Callback.getRewardAdLive() || Callback.getRewardAdSingle() || Callback.getRewardAdLocal())) {
            RewardAdAdmob rewardAdAdmob = new RewardAdAdmob(getApplicationContext());
            rewardAdAdmob.createAd();
        }
        if (Boolean.TRUE.equals(Callback.getIsInterAd())) {
            AdManagerInterAdmob adManagerInterAdmob = new AdManagerInterAdmob(getApplicationContext());
            adManagerInterAdmob.createAd();
        }
    }

    private void setListener() {
        findViewById(R.id.iv_settings).setOnClickListener(v -> startActivity(new Intent(SingleStreamActivity.this, SettingActivity.class)));
        findViewById(R.id.iv_notifications).setOnClickListener(v -> startActivity(new Intent(SingleStreamActivity.this, NotificationsActivity.class)));
        findViewById(R.id.iv_file_download).setOnClickListener(v -> startActivity(new Intent(SingleStreamActivity.this, DownloadActivity.class)));
        findViewById(R.id.ll_url_add).setOnClickListener(v -> {
            new SPHelper(this).setLoginType(Callback.TAG_LOGIN);
            Intent intent = new Intent(SingleStreamActivity.this, SelectPlayerActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        });
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_single_stream;
    }

    private void getNetworkInfo() {
        ImageView imageView = findViewById(R.id.iv_wifi);
        if (!NetworkUtils.isConnected(this)) {
            imageView.setImageResource(R.drawable.ic_wifi_off);
        }
        if (NetworkUtils.isConnectedMobile(this)) {
            imageView.setImageResource(R.drawable.selector_none);
        } else if (NetworkUtils.isConnectedWifi(this)) {
            imageView.setImageResource(R.drawable.ic_wifi);
        } else if (NetworkUtils.isConnectedEthernet(this)) {
            imageView.setImageResource(R.drawable.ic_ethernet);
        }
    }

    private void getData() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected String doInBackground(String strings) {
                try {
                    arrayList.addAll(dbHelper.loadSingleURL());
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
                if (!arrayList.isEmpty()){
                    setAdapter();
                } else {
                    setEmpty();
                }
            }
        }.execute();
    }

    public void setAdapter() {
        AdapterSingleURL adapter = new AdapterSingleURL(this,arrayList, (itemCat, position) -> openPlayerSingleURLActivity(position));
        rv.setAdapter(adapter);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }
        setEmpty();
    }

    private void openPlayerSingleURLActivity(int position) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(SingleStreamActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        new SPHelper(this).setLoginType(Callback.TAG_LOGIN_SINGLE_STREAM);
        Intent intent = new Intent(SingleStreamActivity.this, PlayerSingleURLActivity.class);
        intent.putExtra("channel_title", arrayList.get(position).getAnyName());
        intent.putExtra("channel_url", arrayList.get(position).getSingleURL());
        startActivity(intent);
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_url_add).requestFocus();
            }

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
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            DialogUtil.exitDialog(SingleStreamActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            Log.e("SingleStreamActivity", "Error db close",e);
        }
        super.onDestroy();
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.getIsRecreate())) {
            Callback.setIsRecreate(false);
            recreate();
        }
        super.onResume();
    }
}