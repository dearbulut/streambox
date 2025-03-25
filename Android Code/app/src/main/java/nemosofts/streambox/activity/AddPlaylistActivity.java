package nemosofts.streambox.activity;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.app.Activity;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.LoadPlaylist;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.MediaPath;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class AddPlaylistActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private DBHelper dbHelper;
    private JSHelper jsHelper;
    private EditText etAnyName;
    private EditText etUrl;
    private Boolean isFile = true;
    private String filePath = "";
    private TextView btnBrowse;
    private TextView tvBrowse;
    private ProgressDialog progressDialog;
    private static final String[] SUPPORTED_EXTENSIONS_PLAYLIST = new String[] {
            "audio/mpegurl",
            "audio/x-mpegurl",
            "application/x-mpegurl"
    };

    private ActivityResultLauncher<String> permissionLauncher;
    private ActivityResultLauncher<Intent> pickPlaylistLauncher;

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
                DialogUtil.exitDialog(AddPlaylistActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new ProgressDialog(AddPlaylistActivity.this, true);

        jsHelper = new JSHelper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        etAnyName = findViewById(R.id.et_any_name);
        etUrl = findViewById(R.id.et_url);
        btnBrowse = findViewById(R.id.btn_browse);
        tvBrowse = findViewById(R.id.tv_browse);

        setClickListener();

        RadioGroup rg =  findViewById(R.id.rg);
        if (ApplicationUtil.isTvBox(this)){
            etAnyName.requestFocus();
            rg.check(R.id.rd_2);
            setIsFile(false);
        } else {
            rg.check(R.id.rd_1);
            setIsFile(true);
        }

        // Initialize the permission launcher
        permissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted ->
                Toast.makeText(AddPlaylistActivity.this, Boolean.TRUE.equals(isGranted)
                        ? "Permission granted"
                        : getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show());

        // Initialize the ActivityResultLauncher to pick a playlist
        pickPlaylistLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), this::handlePickPlaylistResult);
    }

    private void setClickListener() {
        findViewById(R.id.rd_1).setOnClickListener(view -> setIsFile(true));
        findViewById(R.id.rd_2).setOnClickListener(view -> setIsFile(false));
        findViewById(R.id.ll_btn_add).setOnClickListener(v -> attemptLogin());
        findViewById(R.id.rl_list_users).setOnClickListener(view -> openUsersListActivity());

        btnBrowse.setOnClickListener(view -> {
            if (checkPermission()) {
                btnBrowse.setBackgroundResource(R.drawable.focused_btn_primary);
                pickPlaylist();
            }
        });
    }

    private void setIsFile(boolean file) {
        isFile = file;
        findViewById(R.id.ll_browse).setVisibility(file ? View.VISIBLE : View.GONE);
        findViewById(R.id.ll_url).setVisibility(file ? View.GONE : View.VISIBLE);
    }

    private void handlePickPlaylistResult(ActivityResult result) {
        if (result != null && result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
            Uri uri = result.getData().getData();
            if (uri == null) {
                return;
            }
            try {
                String pathAudio = MediaPath.getPathAudio(AddPlaylistActivity.this, uri);
                if (pathAudio != null && pathAudio.contains(".m3u")) {
                    filePath = String.valueOf(uri);
                    tvBrowse.setText(pathAudio);
                    btnBrowse.setBackgroundResource(R.drawable.focused_btn_success);
                    new Handler(Looper.getMainLooper()).postDelayed(() ->
                            Toasty.makeText(AddPlaylistActivity.this, true, getString(R.string.added_success), Toasty.SUCCESS), 0);
                } else {
                    errorData();
                }
            } catch (Exception e) {
                errorData();
            }
        }
    }

    private void openUsersListActivity() {
        Intent intent = new Intent(AddPlaylistActivity.this, UsersListActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void openPlaylistActivity() {
        Toast.makeText(AddPlaylistActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
        spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
        spHelper.setAnyName(etAnyName.getText().toString());
        Intent intent = new Intent(AddPlaylistActivity.this, PlaylistActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void pickPlaylist() {
        tvBrowse.setText("");
        // Create an intent with the specified MIME types
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("audio/*");  // General audio type, can filter within the file chooser
        intent.putExtra(Intent.EXTRA_MIME_TYPES, SUPPORTED_EXTENSIONS_PLAYLIST);
        // Launch the intent using ActivityResultLauncher
        pickPlaylistLauncher.launch(intent);
    }

    private void attemptLogin() {
        etAnyName.setError(null);
        String anyName = etAnyName.getText().toString();

        if (isAnyNameInvalid(anyName)) {
            return;
        }

        if (Boolean.TRUE.equals(isFile)) {
            if (filePath == null) {
                Toasty.makeText(AddPlaylistActivity.this, true, getString(R.string.err_file_invalid), Toasty.ERROR);
            } else {
                loadPlaylistData();
            }
        } else {
            etUrl.setError(null);
            String urlData = etUrl.getText().toString();

            if (TextUtils.isEmpty(urlData)) {
                etUrl.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
                etUrl.requestFocus();
            } else if (NetworkUtils.isConnected(this)) {
                loadPlaylistData();
            } else {
                Toasty.makeText(AddPlaylistActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            }
        }
    }

    private boolean isAnyNameInvalid(String anyName) {
        if (TextUtils.isEmpty(anyName)) {
            etAnyName.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            etAnyName.requestFocus();
            return true;
        }
        return false;
    }

    private void loadPlaylistData() {
        String finalUrl = Boolean.TRUE.equals(isFile) ? filePath : etUrl.getText().toString().trim();
        LoadPlaylist playlist = new LoadPlaylist(this, isFile, finalUrl, new LoadPlaylistListener() {
            @Override
            public void onStart() {
                setEnabled(false);
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, String msg , ArrayList<ItemPlaylist> arrayListPlaylist) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                handleLoadResult(success, msg, arrayListPlaylist);
            }
        });
        playlist.execute();
    }

    private void handleLoadResult(@NonNull String success, String msg, ArrayList<ItemPlaylist> arrayListPlaylist) {
        if (success.equals("1")) {
            if (arrayListPlaylist.isEmpty()){
                setEnabled(true);
                Toasty.makeText(AddPlaylistActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
            } else {
                jsHelper.addToPlaylistData(arrayListPlaylist);
                if (Boolean.FALSE.equals(isFile)){
                   String userId = dbHelper.addToUserDB(new ItemUsersDB("",
                            etAnyName.getText().toString(), etAnyName.getText().toString(),
                            etAnyName.getText().toString(), etUrl.getText().toString(),
                            "playlist"));
                   spHelper.setUserId(userId);
                }
                Toast.makeText(AddPlaylistActivity.this, "Add successfully.", Toast.LENGTH_SHORT).show();
                openPlaylistActivity();
            }
        }  else {
            setEnabled(true);
            Toasty.makeText(AddPlaylistActivity.this,true, msg, Toasty.ERROR);
        }
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
    public int setContentViewID() {
        return R.layout.activity_add_playlist;
    }

    @NonNull
    private Boolean checkPermission() {
        String permission;
        if (Build.VERSION.SDK_INT >= 33) {
            permission = READ_MEDIA_AUDIO;
        } else if (Build.VERSION.SDK_INT >= 29) {
            permission = READ_EXTERNAL_STORAGE;
        } else {
            permission = WRITE_EXTERNAL_STORAGE;
        }
        if (ContextCompat.checkSelfPermission(AddPlaylistActivity.this, permission) != PackageManager.PERMISSION_GRANTED) {
            permissionLauncher.launch(permission);  // Request permission using the new API
            return false;
        }
        return true;
    }

    private void errorData() {
        filePath = "";
        tvBrowse.setText("");
        btnBrowse.setBackgroundResource(R.drawable.focused_btn_danger);
        new Handler(Looper.getMainLooper()).postDelayed(() ->
                Toasty.makeText(AddPlaylistActivity.this,true, getString(R.string.err_file_invalid), Toasty.ERROR), 0);
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.exitDialog(AddPlaylistActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}