package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemSingleURL;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class AddSingleURLActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private SPHelper spHelper;
    private EditText etAnyName;
    private EditText etUrl;
    private ProgressDialog progressDialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(AddSingleURLActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new ProgressDialog(AddSingleURLActivity.this, true);

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        etAnyName = findViewById(R.id.et_any_name);
        etUrl = findViewById(R.id.et_url);

        findViewById(R.id.ll_btn_add).setOnClickListener(v -> addURL());
        findViewById(R.id.rl_list_single).setOnClickListener(view -> openSingleStreamActivity());

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.rl_list_single).setFocusableInTouchMode(false);
            etAnyName.requestFocus();
        }
    }

    private void openSingleStreamActivity() {
        spHelper.setLoginType(Callback.TAG_LOGIN_SINGLE_STREAM);
        Intent intent = new Intent(AddSingleURLActivity.this, SingleStreamActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_add_single_url;
    }

    private void addURL() {
        etAnyName.setError(null);
        etUrl.setError(null);

        // Store values at the time of the login attempt.
        String anyName = etAnyName.getText().toString();
        String videoUrl = etUrl.getText().toString();

        boolean cancel = false;
        View focusView = null;

        // Check for a valid any name.
        if (TextUtils.isEmpty(anyName)) {
            etAnyName.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = etAnyName;
            cancel = true;
        }

        // Check for a valid url.
        if (TextUtils.isEmpty(videoUrl)) {
            etUrl.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = etUrl;
            cancel = true;
        }

        if (cancel) {
            focusView.requestFocus();
        } else {
            playVideo();
        }
    }

    private void playVideo() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected void onPreExecute() {
                setEnabled(false);
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    dbHelper.addToSingleURL(new ItemSingleURL("", etAnyName.getText().toString(), etUrl.getText().toString()));
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
                progressDialog.dismiss();
                if (s.equals("1")){
                    setData();
                } else {
                    setEnabled(true);
                    Toasty.makeText(AddSingleURLActivity.this, true, getString(R.string.err_file_invalid), Toasty.ERROR);
                }
            }
        }.execute();
    }

    private void setData() {
        spHelper.setLoginType(Callback.TAG_LOGIN_SINGLE_STREAM);
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            Intent intent = new Intent(AddSingleURLActivity.this, SingleStreamActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(intent);
            finish();
        }, 500);
    }

    private void setEnabled(boolean isEnabled) {
        if (!isEnabled){
            findViewById(R.id.iv_add).setVisibility(View.GONE);
            findViewById(R.id.pb_add).setVisibility(View.VISIBLE);
            return;
        }
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            findViewById(R.id.iv_add).setVisibility(View.VISIBLE);
            findViewById(R.id.pb_add).setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_btn_add).requestFocus();
            }
        }, 400);
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            Log.e("AddSingleURLActivity", "Error closing database",e);
        }
        super.onDestroy();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.exitDialog(AddSingleURLActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}