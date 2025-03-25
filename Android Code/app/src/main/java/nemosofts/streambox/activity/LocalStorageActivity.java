package nemosofts.streambox.activity;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.BaseColumns;
import android.provider.MediaStore;
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
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.core.content.ContextCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterVideo;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemVideo;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.advertising.AdManagerInterAdmob;
import nemosofts.streambox.util.advertising.GDPRChecker;
import nemosofts.streambox.util.advertising.RewardAdAdmob;
import nemosofts.streambox.util.helper.SPHelper;

public class LocalStorageActivity extends AppCompatActivity {

    private ProgressDialog progressDialog;
    private FrameLayout frameLayout;
    private RecyclerView rv;
    private AdapterVideo adapterVideo;
    private ArrayList<ItemVideo> itemVideoList;

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
                DialogUtil.exitDialog(LocalStorageActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new ProgressDialog(LocalStorageActivity.this, true);

        itemVideoList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        if(Boolean.TRUE.equals(checkPer())){
            loadDownloadVideo();
        }

        findViewById(R.id.iv_picker_video).setOnClickListener(v -> {
            if(Boolean.TRUE.equals(checkPer())) {
                openVideoPicker();
            }
        });
        findViewById(R.id.iv_exit).setOnClickListener(v -> openSelectPlayerActivity());

        setAds();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isFinishing()){
                return;
            }
            DialogUtil.popupAdsDialog(LocalStorageActivity.this);
        }, 600);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayerActivity() {
        new SPHelper(this).setLoginType(Callback.TAG_LOGIN);
        Intent intent = new Intent(LocalStorageActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    private void setAds() {
        if (ApplicationUtil.isTvBox(this)){
           return;
        }
        new GDPRChecker(LocalStorageActivity.this).check();
        if (Boolean.TRUE.equals(Callback.getRewardAdMovie() || Callback.getRewardAdEpisodes()
                || Callback.getRewardAdLive() || Callback.getRewardAdSingle() || Callback.getRewardAdLocal())) {
            RewardAdAdmob rewardAdAdmob = new RewardAdAdmob(getApplicationContext());
            rewardAdAdmob.createAd();
        }
        if (Boolean.TRUE.equals(Callback.getIsInterAd())) {
            AdManagerInterAdmob adManagerInterAdmob = new AdManagerInterAdmob(getApplicationContext());
            adManagerInterAdmob.createAd();
        }
    }

    // Initialize the permission launcher
    private final ActivityResultLauncher<String> permissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (Boolean.TRUE.equals(isGranted)){
            loadDownloadVideo();
            Toast.makeText(LocalStorageActivity.this, "Permission granted" , Toast.LENGTH_SHORT).show();
        } else {
            Toast.makeText(LocalStorageActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
        }
    });

    private void loadDownloadVideo() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected void onPreExecute() {
                rv.setVisibility(View.GONE);
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    Uri uri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                    String[] projection = {
                            BaseColumns._ID,
                            MediaStore.MediaColumns.TITLE,
                            MediaStore.MediaColumns.DATA
                    };
                    String selection = MediaStore.MediaColumns.MIME_TYPE + "=? OR " + MediaStore.MediaColumns.MIME_TYPE + "=?";
                    String[] selectionArgs = { "video/mp4", "video/x-msvideo" }; // MIME type for mp4 and avi videos

                    Cursor cursor = getContentResolver().query(uri, projection, selection, selectionArgs, null);
                    if (cursor != null) {
                        while (cursor.moveToNext()) {
                            String title = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.TITLE));
                            String path = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA));
                            itemVideoList.add(new ItemVideo(title, path));
                        }
                        cursor.close();
                    }
                    return "1";
                } catch (Exception e) {
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                if (!isFinishing()){
                    progressDialog.dismiss();
                    setAdapterToListview();
                }
            }
        }.execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToListview() {
        if (!itemVideoList.isEmpty()){
            adapterVideo = new AdapterVideo(itemVideoList, position -> {
                Intent intent = new Intent(LocalStorageActivity.this, PlayerLocalActivity.class);
                intent.putExtra("channel_title", itemVideoList.get(position).getTitle());
                intent.putExtra("channel_url", itemVideoList.get(position).getPath());
                startActivity(intent);
            });
            rv.setAdapter(adapterVideo);
            setupSearchFunctionality();
        } else {
            setEmpty();
        }
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
            if (adapterVideo != null) {
                adapterVideo.getFilter().filter(s.toString());
                adapterVideo.notifyDataSetChanged();
            }
        }

        @Override
        public void afterTextChanged(Editable s) {
            // this method is empty
        }
    };

    private void setEmpty() {
        if (!itemVideoList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                rv.requestFocus();
            }
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_local_storage;
    }

    @NonNull
    private Boolean checkPer() {
        String permission;
        if (Build.VERSION.SDK_INT >= 33) {
            permission = READ_MEDIA_VIDEO;
        } else if (Build.VERSION.SDK_INT >= 29) {
            permission = READ_EXTERNAL_STORAGE;
        } else {
            permission = WRITE_EXTERNAL_STORAGE;
        }
        if (ContextCompat.checkSelfPermission(LocalStorageActivity.this, permission) != PackageManager.PERMISSION_GRANTED) {
            permissionLauncher.launch(permission);  // Request permission using the new API
            return false;
        }
        return true;
    }

    private void openVideoPicker() {
        Intent intent = new Intent(Intent.ACTION_PICK, MediaStore.Video.Media.EXTERNAL_CONTENT_URI);
        pickPlaylistLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> pickPlaylistLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<>() {
        @OptIn(markerClass = UnstableApi.class)
        @Override
        public void onActivityResult(ActivityResult result) {
            if (isFinishing()){
                return;
            }
            if (result != null && result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                Uri videoUri = result.getData().getData();
                String videoPath = ApplicationUtil.getRealPathFromURI(LocalStorageActivity.this, videoUri);
                if (videoPath != null) {
                    Intent intent = new Intent(LocalStorageActivity.this, PlayerLocalActivity.class);
                    intent.putExtra("channel_title", "video");
                    intent.putExtra("channel_url", videoPath);
                    startActivity(intent);
                }
            }
        }
    });

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            DialogUtil.exitDialog(LocalStorageActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.getIsRecreate())) {
            Callback.setIsRecreate(false);
            recreate();
        }
        super.onResume();
    }
}