package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemRadioButton;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingGeneralActivity extends AppCompatActivity {

    JSHelper jsHelper;
    private SPHelper spHelper;
    private TextView recentlyMovie;
    private TextView recentlyLive;
    private int recentlyMovieData = 0;
    private int recentlyLiveData = 0;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        Boolean isTvBox = ApplicationUtil.isTvBox(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);
        jsHelper = new JSHelper(this);

        recentlyMovie = findViewById(R.id.tv_add_recently_movie);
        recentlyLive = findViewById(R.id.tv_add_recently_live);

        CheckBox autoplay = findViewById(R.id.cbox_autoplay_episode);
        autoplay.setChecked(spHelper.getIsAutoplayEpisode());

        CheckBox reverse = findViewById(R.id.cbox_cat_reverse);
        reverse.setChecked(jsHelper.getIsCategoriesOrder());

        CheckBox audio = findViewById(R.id.cbox_splash_audio);
        audio.setChecked(spHelper.getIsSplashAudio());

        EditText agent = findViewById(R.id.et_agent);
        agent.setText(spHelper.getAgentName());

        recentlyMovieData = spHelper.getMovieLimit();
        recentlyLiveData = spHelper.getLiveLimit();
        getRecently();

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            findViewById(R.id.ll_recently_movie).setVisibility(View.GONE);
            findViewById(R.id.ll_recently_live).setVisibility(View.GONE);
            autoplay.setVisibility(View.GONE);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            findViewById(R.id.ll_recently_movie).setVisibility(View.GONE);
            findViewById(R.id.ll_recently_live).setVisibility(View.GONE);
            autoplay.setVisibility(View.GONE);
            reverse.setVisibility(View.GONE);
        }

        findViewById(R.id.ll_recently_movie).setOnClickListener(v -> dialogRecentlyMovie());
        findViewById(R.id.ll_recently_live).setOnClickListener(v -> dialogRecentlyLive());
        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            spHelper.setAgentName(agent.getText().toString());
            if (!spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                spHelper.setIsAutoplayEpisode(autoplay.isChecked());
            }
            jsHelper.setIsCategoriesOrder(reverse.isChecked());
            spHelper.setIsAudio(audio.isChecked());
            spHelper.setRecentlyLimit(recentlyLiveData, recentlyMovieData);
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingGeneralActivity.this,true, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.cbox_autoplay_episode).requestFocus();
        }
    }

    private void dialogRecentlyLive() {
        ArrayList<ItemRadioButton> arrayList = new ArrayList<>();
        arrayList.add(new ItemRadioButton(10,"10 Lists"));
        arrayList.add(new ItemRadioButton(20,"20 Lists"));
        arrayList.add(new ItemRadioButton(30,"30 Lists"));
        arrayList.add(new ItemRadioButton(40,"40 Lists"));
        arrayList.add(new ItemRadioButton(50,"50 Lists"));
        arrayList.add(new ItemRadioButton(55,"55 Lists"));
        DialogUtil.radioBtnDialog(this, arrayList, spHelper.getLiveLimit(), getString(R.string.select_recently_limit), update -> {
            Toasty.makeText(SettingGeneralActivity.this, true,"Changed limit ("+ update +")", Toasty.SUCCESS);
            recentlyLiveData = update;
            getRecently();
        });
    }

    private void dialogRecentlyMovie() {
        ArrayList<ItemRadioButton> arrayList = new ArrayList<>();
        arrayList.add(new ItemRadioButton(10,"10 Lists"));
        arrayList.add(new ItemRadioButton(20,"20 Lists"));
        arrayList.add(new ItemRadioButton(30,"30 Lists"));
        arrayList.add(new ItemRadioButton(40,"40 Lists"));
        arrayList.add(new ItemRadioButton(50,"50 Lists"));
        arrayList.add(new ItemRadioButton(55,"55 Lists"));
        DialogUtil.radioBtnDialog(this, arrayList, spHelper.getMovieLimit(), getString(R.string.select_recently_limit), update -> {
            Toasty.makeText(SettingGeneralActivity.this, true,"Changed limit ("+ update +")", Toasty.SUCCESS);
            recentlyMovieData = update;
            getRecently();
        });
    }

    private void getRecently() {
        recentlyMovie.setText(String.valueOf(recentlyMovieData));
        recentlyLive.setText(String.valueOf(recentlyLiveData));
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting_general;
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