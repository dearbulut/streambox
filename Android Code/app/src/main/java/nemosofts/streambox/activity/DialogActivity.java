package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class DialogActivity extends AppCompatActivity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        String from = getIntent().getStringExtra("from");
        switch (Objects.requireNonNull(from)){
            case Callback.DIALOG_TYPE_UPDATE:
                DialogUtil.upgradeDialog(this, this::openMainActivity);
                break;
            case Callback.DIALOG_TYPE_MAINTENANCE:
                DialogUtil.maintenanceDialog(this);
                break;
            case Callback.DIALOG_TYPE_DEVELOPER:
                DialogUtil.dModeDialog(this);
                break;
            case Callback.DIALOG_TYPE_VPN:
                DialogUtil.vpnDialog(this);
                break;
            default:
                openMainActivity();
                break;
        }
    }

    private void openMainActivity() {
        SPHelper spHelper = new SPHelper(this);
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            new Handler(Looper.getMainLooper()).postDelayed(this::openSingleStream, 2000);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            if (Boolean.TRUE.equals(spHelper.getIsFirst())) {
                new Handler(Looper.getMainLooper()).postDelayed(this::openSelectPlayer, 2000);
            } else {
                if (Boolean.FALSE.equals(spHelper.getIsAutoLogin())) {
                    new Handler(Looper.getMainLooper()).postDelayed(this::openSelectPlayer, 2000);
                } else {
                    ApplicationUtil.openThemeActivity(DialogActivity.this);
                }
            }
        } else {
            new Handler(Looper.getMainLooper()).postDelayed(this::openSelectPlayer, 2000);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayer() {
        Intent intent = new Intent(DialogActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSingleStream() {
        Intent intent = new Intent(DialogActivity.this, SingleStreamActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_launcher;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            finish();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}