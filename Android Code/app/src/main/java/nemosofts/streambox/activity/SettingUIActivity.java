package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.nemosofts.theme.ColorUtils;
import androidx.nemosofts.theme.ThemeEngine;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemRadioButton;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingUIActivity extends AppCompatActivity {

    SPHelper spHelper;
    private ThemeEngine themeEngine;
    private TextView classic;
    private TextView darkGrey;
    private TextView dark;
    private TextView darkBlue;
    private TextView blurRadius;
    private int blurRadiusData = 0;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        Boolean isTvBox  = ApplicationUtil.isTvBox(this);
        themeEngine = new ThemeEngine(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        CheckBox shimmerHome = findViewById(R.id.cbox_shimmering_home);
        CheckBox shimmerDetails = findViewById(R.id.cbox_shimmering_details);

        shimmerHome.setChecked(spHelper.getIsShimmeringHome());
        shimmerDetails.setChecked(spHelper.getIsShimmeringDetails());

        CheckBox cardTitle = findViewById(R.id.cbox_card_title);
        cardTitle.setChecked(spHelper.getUICardTitle());

        CheckBox cast = findViewById(R.id.cbox_cast);
        cast.setChecked(spHelper.getIsCast());

        CheckBox download = findViewById(R.id.cbox_download);
        download.setChecked(spHelper.getIsDownloadUser());
        if (!spHelper.getIsDownload()){
            download.setVisibility(View.GONE);
        }

        int theme = spHelper.getIsTheme();

        CheckBox snowFall = findViewById(R.id.cbox_snow_fall);
        snowFall.setChecked(spHelper.isSnowFall());
        if (theme == 6){
            snowFall.setVisibility(View.VISIBLE);
        } else {
            snowFall.setVisibility(View.GONE);
        }

        CheckBox playerSubtitle = findViewById(R.id.cbox_subtitle);
        CheckBox playerVR = findViewById(R.id.cbox_vr);

        playerSubtitle.setChecked(spHelper.getIsSubtitle());
        playerVR.setChecked(spHelper.getIsVR());

        blurRadius = findViewById(R.id.tv_blur_radius);
        blurRadiusData = spHelper.getBlurRadius();
        recreateBlurRadius();
        findViewById(R.id.ll_blur_radius).setOnClickListener(v -> dialogBlurRadius());

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            spHelper.setIsUI(cardTitle.isChecked(), download.isChecked(), cast.isChecked());
            spHelper.setIsShimmering(shimmerHome.isChecked(), shimmerDetails.isChecked());
            spHelper.setIsPlayerUI(playerSubtitle.isChecked(),playerVR.isChecked());
            spHelper.setSnowFall(snowFall.isChecked());
            spHelper.setBlurRadius(blurRadiusData);
            Callback.setIsDataUpdate(true);
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingUIActivity.this,true, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            shimmerDetails.setVisibility(View.GONE);
            cast.setVisibility(View.GONE);
            download.setVisibility(View.GONE);
            findViewById(R.id.ll_blur_radius).setVisibility(View.GONE);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            shimmerDetails.setVisibility(View.GONE);
            cast.setVisibility(View.GONE);
            download.setVisibility(View.GONE);
            cardTitle.setVisibility(View.GONE);
            findViewById(R.id.ll_blur_radius).setVisibility(View.GONE);
        }

        if (theme == 1 || theme == 4 || theme == 5 || theme == 6 || theme == 7){
            findViewById(R.id.ll_theme).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_theme).requestFocus();
        } else {
            findViewById(R.id.ll_theme).setVisibility(View.GONE);
            findViewById(R.id.cbox_card_title).requestFocus();
        }

        classic = findViewById(R.id.tv_classic);
        darkGrey = findViewById(R.id.tv_dark_grey);
        dark = findViewById(R.id.tv_dark);
        darkBlue = findViewById(R.id.tv_dark_blue);
        setListener();
        getThemeData();
    }

    private void dialogBlurRadius() {
        ArrayList<ItemRadioButton> arrayList = new ArrayList<>();
        arrayList.add(new ItemRadioButton(10,"10 Radius"));
        arrayList.add(new ItemRadioButton(25,"25 Radius"));
        arrayList.add(new ItemRadioButton(40,"40 Radius"));
        arrayList.add(new ItemRadioButton(70,"70 Radius"));
        arrayList.add(new ItemRadioButton(80,"80 Radius"));
        arrayList.add(new ItemRadioButton(90,"90 Radius"));
        DialogUtil.radioBtnDialog(this, arrayList, spHelper.getBlurRadius(), getString(R.string.select_blur_radius), update -> {
            Toasty.makeText(SettingUIActivity.this, true,"Changed radius ("+ update +")", Toasty.SUCCESS);
            blurRadiusData = update;
            recreateBlurRadius();
        });
    }

    private void recreateBlurRadius() {
        blurRadius.setText(String.valueOf(blurRadiusData));
    }

    private void setListener() {
        classic.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 0){
                setThemeMode(false, 0);
            }
        });
        darkGrey.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 2){
                setThemeMode(true, 2);
            }
        });
        darkBlue.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 3){
                setThemeMode(true, 3);
            }
        });
        dark.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 1){
                setThemeMode(true, 1);
            }
        });
    }

    private void setThemeMode(Boolean isChecked, int isTheme) {
        themeEngine.setThemeMode(isChecked);
        themeEngine.setThemePage(isTheme);
        Callback.setIsRecreateUi(true);
        recreate();
    }

    private void getThemeData() {
        int theme = themeEngine.getThemePage();
        if (theme == 0){
            classic.setBackgroundResource(R.drawable.focused_btn_accent);
            darkGrey.setBackgroundResource(R.drawable.focused_save_btn);
            darkBlue.setBackgroundResource(R.drawable.focused_save_btn);
            dark.setBackgroundResource(R.drawable.focused_save_btn);

            classic.setTextColor(ColorUtils.colorWhite(this));
            darkGrey.setTextColor(ColorUtils.colorTitle(this));
            darkBlue.setTextColor(ColorUtils.colorTitle(this));
            dark.setTextColor(ColorUtils.colorTitle(this));

        } else if (theme == 1){
            classic.setBackgroundResource(R.drawable.focused_save_btn);
            darkGrey.setBackgroundResource(R.drawable.focused_save_btn);
            darkBlue.setBackgroundResource(R.drawable.focused_save_btn);
            dark.setBackgroundResource(R.drawable.focused_btn_accent);

            classic.setTextColor(ColorUtils.colorTitle(this));
            darkGrey.setTextColor(ColorUtils.colorTitle(this));
            darkBlue.setTextColor(ColorUtils.colorTitle(this));
            dark.setTextColor(ColorUtils.colorWhite(this));

        } else if (theme == 2){
            classic.setBackgroundResource(R.drawable.focused_save_btn);
            darkGrey.setBackgroundResource(R.drawable.focused_btn_accent);
            darkBlue.setBackgroundResource(R.drawable.focused_save_btn);
            dark.setBackgroundResource(R.drawable.focused_save_btn);

            classic.setTextColor(ColorUtils.colorTitle(this));
            darkGrey.setTextColor(ColorUtils.colorWhite(this));
            darkBlue.setTextColor(ColorUtils.colorTitle(this));
            dark.setTextColor(ColorUtils.colorTitle(this));

        } else if (theme == 3){
            classic.setBackgroundResource(R.drawable.focused_save_btn);
            darkGrey.setBackgroundResource(R.drawable.focused_save_btn);
            darkBlue.setBackgroundResource(R.drawable.focused_btn_accent);
            dark.setBackgroundResource(R.drawable.focused_save_btn);

            classic.setTextColor(ColorUtils.colorTitle(this));
            darkGrey.setTextColor(ColorUtils.colorTitle(this));
            darkBlue.setTextColor(ColorUtils.colorWhite(this));
            dark.setTextColor(ColorUtils.colorTitle(this));
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting_ui;
    }
}