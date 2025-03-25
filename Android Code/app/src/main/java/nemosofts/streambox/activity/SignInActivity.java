package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.text.TextUtils;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterSignInDns;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemDns;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class SignInActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private SPHelper spHelper;
    private EditText etAnyName;
    private EditText etUserName;
    private EditText etLoginPassword;
    private EditText etUrl;
    private Boolean isVisibility = false;
    private LinearLayout llUrl;
    private AdapterSignInDns adapter;
    private Boolean isXui = true;
    private ProgressDialog progressDialog;
    private RecyclerView rvDns;

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
                DialogUtil.exitDialog(SignInActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        String from = getIntent().getStringExtra("from");
        if (from != null && from.equals("stream")) {
            isXui = false;
        }

        progressDialog = new ProgressDialog(SignInActivity.this,true);

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        llUrl  = findViewById(R.id.ll_url);
        etAnyName = findViewById(R.id.et_any_name);
        etUserName = findViewById(R.id.et_user_name);
        etLoginPassword = findViewById(R.id.et_login_password);
        etUrl = findViewById(R.id.et_url);

        ImageView visibility = findViewById(R.id.iv_visibility);
        visibility.setImageResource(Boolean.TRUE.equals(isVisibility) ? R.drawable.ic_login_visibility : R.drawable.ic_login_visibility_off);
        visibility.setOnClickListener(v -> {
            isVisibility = !isVisibility;
            visibility.setImageResource(Boolean.TRUE.equals(isVisibility) ? R.drawable.ic_login_visibility : R.drawable.ic_login_visibility_off);
            etLoginPassword.setTransformationMethod(Boolean.TRUE.equals(isVisibility)
                    ? HideReturnsTransformationMethod.getInstance()  : PasswordTransformationMethod.getInstance());
        });

        findViewById(R.id.ll_btn_add).setOnClickListener(v -> attemptLogin());
        findViewById(R.id.rl_list_users).setOnClickListener(view -> openUsersListActivity());
        findViewById(R.id.rl_vpn).setOnClickListener(view -> openOVPNActivity());
        findViewById(R.id.rl_vpn).setVisibility(spHelper.isOVEN() ? View.VISIBLE : View.GONE);

        rvDns = findViewById(R.id.rv_dns);
        rvDns.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rvDns.setLayoutManager(llm);
        rvDns.setNestedScrollingEnabled(false);
        setDNSData();

        if (ApplicationUtil.isTvBox(this)){
            etAnyName.requestFocus();
        }
    }

    private void openOVPNActivity() {
        Intent intent = new Intent(SignInActivity.this, OpenVPNActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void openUsersListActivity() {
        Intent intent = new Intent(SignInActivity.this, UsersListActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void setDNSData() {
        ArrayList<ItemDns> dnsList = new ArrayList<>();
        dnsList.add(new ItemDns("External", ""));

        try {
            boolean isDnsEnabled;
            String tableName;

            if (Boolean.TRUE.equals(isXui)) {
                isDnsEnabled = Boolean.TRUE.equals(spHelper.getIsXuiDNS());
                tableName = DBHelper.TABLE_DNS_XUI;
            } else {
                isDnsEnabled = Boolean.TRUE.equals(spHelper.getIssStreamDNS());
                tableName = DBHelper.TABLE_DNS_STREAM;
            }

            if (isDnsEnabled) {
                ArrayList<ItemDns> loadedDns = new ArrayList<>(dbHelper.loadDNS(tableName));
                if (!loadedDns.isEmpty()) {
                    dnsList.addAll(loadedDns);
                }
            } else {
                rvDns.setVisibility(View.GONE);
            }
        } catch (Exception e) {
            Log.e("SignInActivity", "Error loading DNS data", e);
        }

        setDnsAdapter(dnsList);
    }

    private void setDnsAdapter(ArrayList<ItemDns> arrayList) {
        adapter = new AdapterSignInDns(this, arrayList, (itemDns, position) -> {
            if (llUrl.getVisibility() == View.GONE && position == 0){
                etUrl.setText("");
            }
            llUrl.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
            adapter.setSelectedFocus(position);
        });
        rvDns.setAdapter(adapter);
        adapter.setSelected(0);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_sign_in;
    }

    private void attemptLogin() {
        etUserName.setError(null);
        etLoginPassword.setError(null);
        etAnyName.setError(null);

        if (llUrl.getVisibility() == View.GONE) {
            String selectedBase = adapter.getSelectedBase();
            etUrl.setText(selectedBase.isEmpty() ? "https://nemosofts.com" : selectedBase);
        }

        // Store values at the time of the login attempt.
        String anyName = etAnyName.getText().toString();
        String userName = etUserName.getText().toString();
        String password = etLoginPassword.getText().toString();
        String urlData = etUrl.getText().toString().replace(" ", "");

        boolean cancel = false;
        View focusView = null;

        // Validate inputs
        if (isInputInvalid(password, userName, anyName, urlData)) {
            cancel = true;
            focusView = getFocusView();
        }

        // Check if the URL is blacklisted
        if (isUrlBlacklisted(urlData)) {
            cancel = true;
            Toasty.makeText(SignInActivity.this, true,"Blacklist", Toasty.ERROR);
            focusView = etUrl;
        }

        // Handle login or focus correction
        if (cancel && focusView != null) {
            focusView.requestFocus();
        } else {
            loadLogin();
        }
    }

    private boolean isInputInvalid(String password, String userName, String anyName, String urlData) {
        boolean isInvalid = false;
        if (TextUtils.isEmpty(password)) {
            etLoginPassword.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            isInvalid = true;
        } else if (password.endsWith(" ")) {
            etLoginPassword.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_pass_end_space)));
            isInvalid = true;
        }

        if (TextUtils.isEmpty(userName)) {
            etUserName.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            isInvalid = true;
        } else if (TextUtils.isEmpty(anyName)) {
            etAnyName.setText(userName);
        }

        if (TextUtils.isEmpty(urlData)) {
            etUrl.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            isInvalid = true;
        }

        return isInvalid;
    }

    private View getFocusView() {
        if (etLoginPassword.getError() != null) {
            return etLoginPassword;
        } else if (etUserName.getError() != null) {
            return etUserName;
        } else if (etUrl.getError() != null) {
            return etUrl;
        }
        return null;
    }

    private boolean isUrlBlacklisted(String url) {
        boolean isBlacklisted = false;
        if (!Callback.getArrayBlacklist().isEmpty()) {
            for (int i = 0; i < Callback.getArrayBlacklist().size(); i++) {
                if (url.toLowerCase().contains(Callback.getArrayBlacklist().get(i).getBase().toLowerCase())) {
                    isBlacklisted = true;
                    break;
                }
            }
        }
        return isBlacklisted;
    }

    private void loadLogin() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(SignInActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        LoadLogin login = new LoadLogin(new LoginListener() {
            @Override
            public void onStart() {
                setEnabled(false);
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ItemLoginUser itemLoginUser,
                              ItemLoginServer itemLoginServer, String allowedOutputFormats) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                handlerLoginEnd(success, itemLoginUser, itemLoginServer, allowedOutputFormats);
            }
        },etUrl.getText().toString().replace(" ",""), ApplicationUtil.getAPIRequestLogin(etUserName.getText().toString(),
                etLoginPassword.getText().toString()));
        login.execute();
    }

    private void handlerLoginEnd(String success, ItemLoginUser itemLoginUser, ItemLoginServer itemLoginServer, String allowedOutputFormats) {
        if (!success.equals("1")) {
            setEnabled(true);
            Toasty.makeText(SignInActivity.this,true, getString(R.string.err_login_not_incorrect), Toasty.ERROR);
            return;
        }
        String userId = dbHelper.addToUserDB(new ItemUsersDB("",etAnyName.getText().toString(),
                etUserName.getText().toString(), etLoginPassword.getText().toString(),
                etUrl.getText().toString().replace(" ",""),Boolean.TRUE.equals(isXui) ? "xui" : "stream")
        );
        spHelper.setUserId(userId);

        spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
        spHelper.setLoginType(Boolean.TRUE.equals(isXui) ? Callback.TAG_LOGIN_ONE_UI : Callback.TAG_LOGIN_STREAM);

        if (!allowedOutputFormats.isEmpty()){
            if (allowedOutputFormats.contains("m3u8")){
                spHelper.setLiveFormat(2);
            } else {
                spHelper.setLiveFormat(1);
            }
        } else {
            spHelper.setLiveFormat(0);
        }

        spHelper.setAnyName(etAnyName.getText().toString());
        spHelper.setIsFirst(false);
        spHelper.setIsLogged(true);
        spHelper.setIsAutoLogin(true);

        Toast.makeText(SignInActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
        ApplicationUtil.openThemeActivity(SignInActivity.this);
    }

    private void setEnabled(boolean isEnabled) {
        if (isEnabled){
            findViewById(R.id.iv_add).setVisibility(View.VISIBLE);
            findViewById(R.id.pb_add).setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_btn_add).requestFocus();
            }
        } else {
            findViewById(R.id.iv_add).setVisibility(View.GONE);
            findViewById(R.id.pb_add).setVisibility(View.VISIBLE);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.exitDialog(SignInActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}