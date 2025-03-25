package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemRadioButton;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingAutomationActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private TextView autoUpdate;
    private int autoUpdateData = 0;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        Boolean isTvBox  = ApplicationUtil.isTvBox(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        autoUpdate = findViewById(R.id.tv_auto_update);

        CheckBox live = findViewById(R.id.cbox_auto_update_live);
        live.setChecked(spHelper.getIsUpdateLive());

        CheckBox movies = findViewById(R.id.cbox_auto_update_movies);
        movies.setChecked(spHelper.getIsUpdateMovies());

        CheckBox series = findViewById(R.id.cbox_auto_update_series);
        series.setChecked(spHelper.getIsUpdateSeries());

        autoUpdateData = spHelper.getAutoUpdate();

        findViewById(R.id.ll_auto_update).setOnClickListener(v -> dialogAutoUpdate());
        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            spHelper.setIsUpdateLive(live.isChecked());
            spHelper.setIsUpdateMovies(movies.isChecked());
            spHelper.setIsUpdateSeries(series.isChecked());
            spHelper.setAutoUpdate(autoUpdateData);
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingAutomationActivity.this, true,"Save Data", Toasty.SUCCESS);
            }, 500);
        });

        getRecently();

        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.cbox_auto_update_live).requestFocus();
        }
    }

    private void dialogAutoUpdate() {
        ArrayList<ItemRadioButton> arrayList = new ArrayList<>();
        arrayList.add(new ItemRadioButton(1,"1 Hours"));
        arrayList.add(new ItemRadioButton(3,"3 Hours"));
        arrayList.add(new ItemRadioButton(4,"4 Hours"));
        arrayList.add(new ItemRadioButton(5,"5 Hours"));
        arrayList.add(new ItemRadioButton(7,"7 Hours"));
        arrayList.add(new ItemRadioButton(10,"10 Hours"));
        DialogUtil.radioBtnDialog(this, arrayList, spHelper.getAutoUpdate(), getString(R.string.select_reload_hours), update -> {
            Toasty.makeText(SettingAutomationActivity.this, true,"Changed hours ("+ update +")", Toasty.SUCCESS);
            autoUpdateData = update;
            getRecently();
        });
    }

    private void getRecently() {
        String update = autoUpdateData +" Hours";
        autoUpdate.setText(update);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting_automation;
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