package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.executor.LoadPlaylist;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class GetActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private SPHelper spHelper;
    private JSHelper jsHelper;
    private ProgressDialog progressDialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);
        jsHelper = new JSHelper(this);

        progressDialog = new ProgressDialog(GetActivity.this, true);

        String anyNameData = "any_name";
        String dmsData = "dms_url";

        Intent intent = getIntent();
        if (Intent.ACTION_SEARCH.equals(intent.getAction())) {
            String type = intent.getStringExtra("login_type");
            if (type != null){
                if (type.equals("xtream") || type.equals("stream")){
                    String anyName = intent.getStringExtra(anyNameData);
                    String userName = intent.getStringExtra("user_name");
                    String userPass = intent.getStringExtra("user_pass");
                    String dmsUrl = intent.getStringExtra(dmsData);
                    loadLogin(anyName, userName, userPass, dmsUrl, type);
                } else if (type.equals("playlist")){
                    String anyName = intent.getStringExtra(anyNameData);
                    String dmsUrl = intent.getStringExtra(dmsData);
                    loadLoginPlaylist(anyName, dmsUrl);
                } else {
                    Toasty.makeText(GetActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
                }
            }
            try {
                intent.removeExtra("login_type");
                intent.removeExtra(anyNameData);
                intent.removeExtra("user_name");
                intent.removeExtra("user_pass");
                intent.removeExtra(dmsData);
            } catch (Exception e) {
               Log.e("GetActivity", "Error in removeExtra",e);
            }
        }
    }

    private void loadLoginPlaylist(String anyName, String userURL) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(GetActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        LoadPlaylist playlist = new LoadPlaylist(this,false, userURL, new LoadPlaylistListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                if (spHelper.isLogged()) {
                    jsHelper.removeAllData();
                    spHelper.removeSignOut();
                }
            }

            @Override
            public void onEnd(String success, String msg, ArrayList<ItemPlaylist> arrayListPlaylist) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1")) {
                    if (arrayListPlaylist.isEmpty()){
                        Toast.makeText(GetActivity.this, getString(R.string.err_no_data_found), Toast.LENGTH_SHORT).show();
                    } else {

                        jsHelper.addToPlaylistData(arrayListPlaylist);

                        Toast.makeText(GetActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                        spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
                        spHelper.setAnyName(anyName);
                        Intent intent = new Intent(GetActivity.this, PlaylistActivity.class);
                        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        startActivity(intent);
                        finish();
                    }
                }  else {
                    Toasty.makeText(GetActivity.this,true, msg, Toasty.ERROR);
                }
            }
        });
        playlist.execute();
    }

    private void loadLogin(String anyName, String userName, String userPass, String dmsUrl, String loginType) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(GetActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        LoadLogin login = new LoadLogin(new LoginListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                jsHelper.removeAllData();
                spHelper.removeSignOut();
            }

            @Override
            public void onEnd(String success, ItemLoginUser itemLoginUser,
                              ItemLoginServer itemLoginServer , String allowedOutputFormats) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (!success.equals("1")) {
                    Toasty.makeText(GetActivity.this,true, getString(R.string.err_login_not_incorrect), Toasty.ERROR);
                    return;
                }

                String type;
                if (Boolean.TRUE.equals(loginType.equals("xtream"))){
                    type = "xui";
                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                } else {
                    type  = "stream";
                    spHelper.setLoginType(Callback.TAG_LOGIN_STREAM);
                }

                String userId = dbHelper.addToUserDB(new ItemUsersDB("", anyName, userName, userPass, dmsUrl,type));
                spHelper.setUserId(userId);

                spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
                if (!allowedOutputFormats.isEmpty()){
                    if (allowedOutputFormats.contains("m3u8")){
                        spHelper.setLiveFormat(2);
                    } else {
                        spHelper.setLiveFormat(1);
                    }
                } else {
                    spHelper.setLiveFormat(0);
                }

                spHelper.setAnyName(anyName);
                spHelper.setIsFirst(false);
                spHelper.setIsLogged(true);
                spHelper.setIsAutoLogin(true);

                Toast.makeText(GetActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
                ApplicationUtil.openThemeActivity(GetActivity.this);
            }
        },dmsUrl, ApplicationUtil.getAPIRequestLogin(userName, userPass));
        login.execute();
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