package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterSetting;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.ParentalControlDialog;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.item.ItemSetting;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private RecyclerView rv;
    private Boolean isTvBox;

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

        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 4);
        grid.setSpanCount(4);
        rv.setLayoutManager(grid);

        setAdapterToListview();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting;
    }

    private void setAdapterToListview() {
        ArrayList<ItemSetting> arrayList = new ArrayList<>();
        arrayList.add(new ItemSetting(getResources().getString(R.string.general_setting), R.drawable.ic_player_setting));
        arrayList.add(new ItemSetting(getResources().getString(R.string.ui), R.drawable.ic_pencil_ruler));
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.stream_format), R.drawable.ic_file_settings));
        }
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.multiple_screen), R.drawable.ic_grid_view));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.clear_data),  R.drawable.ic_clean_code));
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.automation), R.drawable.ic_engineering));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.player_setting), R.drawable.ic_video_settings));
        arrayList.add(new ItemSetting(getResources().getString(R.string.time_format), R.drawable.ic_time));
        arrayList.add(new ItemSetting(getResources().getString(R.string.wifi_setting), R.drawable.ic_wifi));
        if (!ApplicationUtil.isTvBox(this)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.post_notification), R.drawable.ic_round_notification));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.notifications), R.drawable.ic_round_notifications));
        arrayList.add(new ItemSetting(getResources().getString(R.string.parental_control), R.drawable.ic_player_lock));
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.profile), R.drawable.ic_profile));
        }

        if (spHelper.isOVEN()){
            arrayList.add(new ItemSetting(getResources().getString(R.string.vpn), R.drawable.ic_vpn));
        }

        arrayList.add(new ItemSetting(getResources().getString(R.string.speed_test), R.drawable.ic_speed));
        if (!spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.feedback), R.drawable.ic_feedback));
        }

        arrayList.add(new ItemSetting(getResources().getString(R.string.privacy_policy), R.drawable.ic_shield_keyhole));
        arrayList.add(new ItemSetting(getResources().getString(R.string.terms_and_conditions), R.drawable.ic_todo));

        arrayList.add(new ItemSetting(getResources().getString(R.string.about), R.drawable.ic_information));

        AdapterSetting adapter = new AdapterSetting(this, arrayList, (itemSerials, position) -> setOnClick(arrayList.get(position).getName()));
        rv.setAdapter(adapter);
        if (Boolean.TRUE.equals(isTvBox)){
            rv.requestFocus();
        }
    }

    private void setOnClick(String name) {
        if (name == null) return;

        // Create a map of action names to intents or actions
        Map<String, Runnable> actionMap = new HashMap<>();

        actionMap.put(getResources().getString(R.string.wifi_setting), () -> startActivity(new Intent(android.provider.Settings.ACTION_WIFI_SETTINGS)));
        actionMap.put(getResources().getString(R.string.stream_format), () -> startActivity(new Intent(SettingActivity.this, SettingFormatActivity.class)));
        actionMap.put(getResources().getString(R.string.notifications), () -> startActivity(new Intent(SettingActivity.this, NotificationsActivity.class)));
        actionMap.put(getResources().getString(R.string.post_notification), this::notification);
        actionMap.put(getResources().getString(R.string.clear_data), () -> startActivity(new Intent(SettingActivity.this, SettingClearDataActivity.class)));
        actionMap.put(getResources().getString(R.string.parental_control), () -> new ParentalControlDialog(this));
        actionMap.put(getResources().getString(R.string.profile), () -> startActivity(new Intent(SettingActivity.this, ProfileActivity.class)));
        actionMap.put(getResources().getString(R.string.speed_test), () -> startActivity(new Intent(SettingActivity.this, NetworkSpeedActivity.class)));
        actionMap.put(getResources().getString(R.string.multiple_screen), () -> startActivity(new Intent(SettingActivity.this, SettingMultiScreenActivity.class)));
        actionMap.put(getResources().getString(R.string.automation), () -> startActivity(new Intent(SettingActivity.this, SettingAutomationActivity.class)));
        actionMap.put(getResources().getString(R.string.time_format), () -> startActivity(new Intent(SettingActivity.this, SettingTimeFormatActivity.class)));
        actionMap.put(getResources().getString(R.string.player_setting), () -> startActivity(new Intent(SettingActivity.this, SettingPlayerActivity.class)));
        actionMap.put(getResources().getString(R.string.feedback), () -> new FeedBackDialog(this).showDialog(getString(R.string.feedback)));
        actionMap.put(getResources().getString(R.string.general_setting), () -> startActivity(new Intent(SettingActivity.this, SettingGeneralActivity.class)));
        actionMap.put(getResources().getString(R.string.ui), () -> startActivity(new Intent(SettingActivity.this, SettingUIActivity.class)));
        actionMap.put(getResources().getString(R.string.privacy_policy), () -> {
            Intent intent = new Intent(SettingActivity.this, WebActivity.class);
            intent.putExtra("web_url", BuildConfig.BASE_URL+"data.php?privacy_policy");
            intent.putExtra("page_title", getResources().getString(R.string.privacy_policy));
            ActivityCompat.startActivity(SettingActivity.this, intent, null);
        });
        actionMap.put(getResources().getString(R.string.terms_and_conditions), () -> {
            Intent intent = new Intent(SettingActivity.this, WebActivity.class);
            intent.putExtra("web_url", BuildConfig.BASE_URL+"data.php?terms");
            intent.putExtra("page_title", getResources().getString(R.string.terms_and_conditions));
            ActivityCompat.startActivity(SettingActivity.this, intent, null);
        });
        actionMap.put(getResources().getString(R.string.about), () -> startActivity(new Intent(SettingActivity.this, AboutUsActivity.class)));
        actionMap.put(getResources().getString(R.string.vpn), () -> {

        });

        // Execute the corresponding action if found
        Runnable action = actionMap.get(name);
        if (action != null) {
            action.run();
        }
    }

    private void notification() {
        try {
            Intent intent = new Intent();
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                intent.setAction("android.settings.APP_NOTIFICATION_SETTINGS");
                intent.putExtra("android.provider.extra.APP_PACKAGE", getPackageName());
            } else {
                intent.setAction("android.settings.APPLICATION_DETAILS_SETTINGS");
                intent.addCategory(Intent.CATEGORY_DEFAULT);
                intent.setData(android.net.Uri.parse("package:" + getPackageName()));
            }
            startActivity(intent);
        } catch (Exception e) {
            Log.e("SettingActivity", "Error notification",e);
        }
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.getIsRecreateUi())) {
            Callback.setIsRecreateUi(false);
            Callback.setIsRecreate(true);
            recreate();
        }
        super.onResume();
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