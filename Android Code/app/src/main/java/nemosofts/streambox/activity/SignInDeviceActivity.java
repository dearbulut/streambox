package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterUsersDeviceID;
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

@UnstableApi
public class SignInDeviceActivity extends AppCompatActivity {

    private Helper helper;
    private SPHelper spHelper;
    private RecyclerView rv;
    private ArrayList<ItemUsers> arrayList;
    private ProgressBar pb;
    private String deviceID = "N/A";
    private ProgressDialog progressDialog;
    private FrameLayout frameLayout;

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
                DialogUtil.exitDialog(SignInDeviceActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new ProgressDialog(SignInDeviceActivity.this,true);

        deviceID = ApplicationUtil.getDeviceID(this);

        TextView tvDevice = findViewById(R.id.tv_device_id);
        tvDevice.setText("ID - " + deviceID);

        helper = new Helper(this);
        spHelper = new SPHelper(this);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        pb = findViewById(R.id.pb);
        rv = findViewById(R.id.rv);

        LinearLayoutManager llm = new LinearLayoutManager(this);
        rv.setLayoutManager(llm);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setNestedScrollingEnabled(false);

        loadDeviceData();

        findViewById(R.id.rl_list_player).setOnClickListener(view -> openSelectPlayerActivity());
        findViewById(R.id.rl_vpn).setOnClickListener(view -> openOVPNActivity());
        findViewById(R.id.rl_vpn).setVisibility(spHelper.isOVEN() ? View.VISIBLE : View.GONE);
    }

    private void openSelectPlayerActivity() {
        Intent intent = new Intent(SignInDeviceActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void openOVPNActivity() {
        Intent intent = new Intent(SignInDeviceActivity.this, OpenVPNActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void loadDeviceData() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(SignInDeviceActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            setEmpty();
            return;
        }
        LoadUsers loadUsers = new LoadUsers(new UsersListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()) {
                    rv.setVisibility(View.GONE);
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, String verifyStatus, String message, ArrayList<ItemUsers> arrayListUsers) {
                if (isFinishing()){
                    return;
                }
                if (success.equals("1")) {
                    if (arrayListUsers.isEmpty()) {
                        Toasty.makeText(SignInDeviceActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
                        setEmpty();
                    } else {
                        arrayList.addAll(arrayListUsers);
                        setAdapter();
                    }
                } else {
                    Toasty.makeText(SignInDeviceActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                    setEmpty();
                }
            }
        }, helper.getAPIRequestNSofts(Method.METHOD_GET_DEVICE_ID, "", "", "", deviceID));
        loadUsers.execute();
    }

    private void setAdapter() {
        AdapterUsersDeviceID adapterUsersDeviceID = new AdapterUsersDeviceID(this, arrayList, (itemUsers, position) -> loadLogin(arrayList.get(position)));
        rv.setAdapter(adapterUsersDeviceID);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }
        setEmpty();
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()){
            rv.setVisibility(View.VISIBLE);
            pb.setVisibility(View.GONE);
            frameLayout.setVisibility(View.GONE);
        } else {
            pb.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);
            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
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
            public void onEnd(String success, ItemLoginUser itemLoginUser,
                              ItemLoginServer itemLoginServer , String allowedOutputFormats) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (!success.equals("1")) {
                    Toast.makeText(SignInDeviceActivity.this, getString(R.string.err_login_not_incorrect), Toast.LENGTH_SHORT).show();
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

                Toast.makeText(SignInDeviceActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                ApplicationUtil.openThemeActivity(SignInDeviceActivity.this);
            }
        },itemUsers.getDnsBase(), ApplicationUtil.getAPIRequestLogin(itemUsers.getUserName(), itemUsers.getUserPassword()));
        login.execute();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_sign_in_device;
    }
}