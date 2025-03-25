package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterUsers;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadLogin;
import nemosofts.streambox.executor.LoadPlaylist;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class UsersListActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private DBHelper dbHelper;
    private RecyclerView rv;
    private ArrayList<ItemUsersDB> arrayList;
    private AdapterUsers adapter;
    private FrameLayout frameLayout;
    private ProgressDialog progressDialog;

    @OptIn(markerClass = UnstableApi.class)
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(UsersListActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        progressDialog = new ProgressDialog(UsersListActivity.this, true);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        getUserData();

        findViewById(R.id.ll_user_add).setOnClickListener(v -> openSelectPlayerActivity());
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayerActivity() {
        Intent intent = new Intent(UsersListActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_users_list;
    }

    private void getUserData() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected String doInBackground(String strings) {
                try {
                    arrayList.addAll(dbHelper.loadUsersDB());
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
                if (arrayList != null && !arrayList.isEmpty()){
                    setAdapter();
                } else {
                    setEmpty();
                }
            }
        }.execute();
    }

    public void setAdapter() {
        adapter = new AdapterUsers(this, arrayList, position -> {
            if (position >= 0 && position < arrayList.size()) {
                if (arrayList.get(position).getUserType().equals("xui") || arrayList.get(position).getUserType().equals("stream")) {
                    loadLogin(arrayList.get(position));
                } else if (arrayList.get(position).getUserType().equals("playlist")) {
                    loadLoginPlaylist(arrayList.get(position).getAnyName(), arrayList.get(position).getUserURL());
                }
            } else {
                Toasty.makeText(UsersListActivity.this,true, "Position out of bounds: " + position, Toasty.ERROR);
            }
        });
        rv.setAdapter(adapter);
        setupSearchFunctionality();
    }

    private void setupSearchFunctionality() {
        EditText edtSearch = findViewById(R.id.edt_search);
        edtSearch.setVisibility(View.VISIBLE);
        edtSearch.setOnEditorActionListener((v, actionId, event) -> {
            InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
            View currentFocus = this.getCurrentFocus();
            if (currentFocus != null) {
                inputManager.hideSoftInputFromWindow(currentFocus.getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
                rv.requestFocus();
            }
            return true;
        });
        edtSearch.addTextChangedListener(searchWatcher);
        setEmpty();
    }

    TextWatcher searchWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            // this method is empty
        }

        @SuppressLint("NotifyDataSetChanged")
        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (adapter != null) {
                adapter.getFilter().filter(s.toString());
                adapter.notifyDataSetChanged();
            }
        }

        @Override
        public void afterTextChanged(Editable s) {
            // this method is empty
        }
    };

    private void loadLoginPlaylist(String anyName, String userURL) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(UsersListActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        LoadPlaylist playlist = new LoadPlaylist(this,false, userURL, new LoadPlaylistListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, String msg, ArrayList<ItemPlaylist> arrayListPlaylist) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1")) {
                    if (arrayListPlaylist.isEmpty()){
                        Toast.makeText(UsersListActivity.this, getString(R.string.err_no_data_found), Toast.LENGTH_SHORT).show();
                    } else {

                        new JSHelper(UsersListActivity.this).addToPlaylistData(arrayListPlaylist);

                        Toast.makeText(UsersListActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                        spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
                        spHelper.setAnyName(anyName);
                        Intent intent = new Intent(UsersListActivity.this, PlaylistActivity.class);
                        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        startActivity(intent);
                        finish();
                    }
                }  else {
                    Toasty.makeText(UsersListActivity.this,true, msg, Toasty.ERROR);
                }
            }
        });
        playlist.execute();
    }

    private void loadLogin(ItemUsersDB itemUsersDB) {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(UsersListActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
                    Toasty.makeText(UsersListActivity.this,true, getString(R.string.err_server_not_connected), Toasty.ERROR);
                    return;
                }

                spHelper.setLoginDetails(itemLoginUser, itemLoginServer);
                if (Boolean.TRUE.equals(itemUsersDB.getUserType().equals("xui"))){
                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                } else {
                    spHelper.setLoginType(Callback.TAG_LOGIN_STREAM);
                }
                spHelper.setUserId(itemUsersDB.getId());

                if (!allowedOutputFormats.isEmpty()){
                    if (allowedOutputFormats.contains("m3u8")){
                        spHelper.setLiveFormat(2);
                    } else {
                        spHelper.setLiveFormat(1);
                    }
                } else {
                    spHelper.setLiveFormat(0);
                }

                spHelper.setAnyName(itemUsersDB.getAnyName());
                spHelper.setIsFirst(false);
                spHelper.setIsLogged(true);
                spHelper.setIsAutoLogin(true);

                Toast.makeText(UsersListActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
                ApplicationUtil.openThemeActivity(UsersListActivity.this);
            }

        },itemUsersDB.getUserURL(), ApplicationUtil.getAPIRequestLogin(itemUsersDB.getUseName(),itemUsersDB.getUserPass()));
        login.execute();
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                rv.requestFocus();
            }
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_user_add).requestFocus();
            }

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_add_user, null);

            frameLayout.addView(myView);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            DialogUtil.exitDialog(UsersListActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
    }
}