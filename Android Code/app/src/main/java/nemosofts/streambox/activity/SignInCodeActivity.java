package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.callback.Method;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.executor.LoadUsers;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.interfaces.UsersListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.item.ItemUsers;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;

public class SignInCodeActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private Helper helper;
    private EditText etActivationCode;
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
                DialogUtil.exitDialog(SignInCodeActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new ProgressDialog(SignInCodeActivity.this,true);

        helper = new Helper(this);
        spHelper = new SPHelper(this);

        etActivationCode  = findViewById(R.id.et_activation_code);

        findViewById(R.id.ll_btn_add).setOnClickListener(v -> attemptLogin());
        findViewById(R.id.rl_list_users).setOnClickListener(view -> openSelectPlayerActivity());
        findViewById(R.id.rl_vpn).setOnClickListener(view -> openOVPNActivity());
        findViewById(R.id.rl_vpn).setVisibility(spHelper.isOVEN() ? View.VISIBLE : View.GONE);

        if (ApplicationUtil.isTvBox(this)){
            etActivationCode.requestFocus();
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayerActivity() {
        Intent intent = new Intent(SignInCodeActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void openOVPNActivity() {
        Intent intent = new Intent(SignInCodeActivity.this, OpenVPNActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void attemptLogin() {
        etActivationCode.setError(null);

        String code = etActivationCode.getText().toString();

        boolean cancel = false;
        View focusView = null;

        if (TextUtils.isEmpty(code)) {
            etActivationCode.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = etActivationCode;
            cancel = true;
        }

        if (cancel) {
            if (focusView != null) {
                focusView.requestFocus();
            }
        } else {
            loadActivationCode();
        }
    }

    private void loadActivationCode() {
        if (!NetworkUtils.isConnected(this)){
            Toast.makeText(this, getString(R.string.err_internet_not_connected), Toast.LENGTH_SHORT).show();
            return;
        }
        LoadUsers loadUsers = new LoadUsers(new UsersListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, String verifyStatus, String message, ArrayList<ItemUsers> arrayListUsers) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1")) {
                    if (arrayListUsers.isEmpty()) {
                        Toasty.makeText(SignInCodeActivity.this,true, getString(R.string.err_activation_code_incorrect), Toasty.ERROR);
                    } else {
                        loadLogin(arrayListUsers.get(0));
                    }
                } else {
                    Toasty.makeText(SignInCodeActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                }
            }
        }, helper.getAPIRequestNSofts(Method.METHOD_GET_ACTIVATION_CODE, "",
                "", "", etActivationCode.getText().toString()));
        loadUsers.execute();
    }

    private void loadLogin(ItemUsers itemUsers) {
        if (!NetworkUtils.isConnected(this)){
            Toast.makeText(this, getString(R.string.err_internet_not_connected), Toast.LENGTH_SHORT).show();
            return;
        }
        LoadLogin login = new LoadLogin(new LoginListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ItemLoginUser itemLoginUser, ItemLoginServer itemLoginServer, String allowedOutputFormats) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (!success.equals("1")) {
                    Toast.makeText(SignInCodeActivity.this, getString(R.string.err_login_not_incorrect), Toast.LENGTH_SHORT).show();
                    return;
                }

                spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
                if (Boolean.TRUE.equals(itemUsers.getUserType().equals("xui"))){
                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                } else {
                    spHelper.setLoginType(Callback.TAG_LOGIN_STREAM);
                }

                if (!allowedOutputFormats.isEmpty()){
                    if (allowedOutputFormats.contains("m3u8")){
                        spHelper.setLiveFormat(2);
                    } else {
                        spHelper.setLiveFormat(1);
                    }
                } else {
                    spHelper.setLiveFormat(0);
                }

                spHelper.setAnyName(itemUsers.getUserName());
                spHelper.setIsFirst(false);
                spHelper.setIsLogged(true);
                spHelper.setIsAutoLogin(true);

                Toast.makeText(SignInCodeActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                ApplicationUtil.openThemeActivity(SignInCodeActivity.this);
            }
        },itemUsers.getDnsBase(), ApplicationUtil.getAPIRequestLogin(itemUsers.getUserName(),
                itemUsers.getUserPassword()));
        login.execute();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_sign_in_code;
    }
}