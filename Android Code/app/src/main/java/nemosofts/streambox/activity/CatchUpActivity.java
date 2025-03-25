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

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCategories;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;

public class CatchUpActivity extends AppCompatActivity {

    Helper helper;
    private JSHelper jsHelper;
    private RecyclerView rv;
    private ArrayList<ItemCat> arrayList;
    private AdapterCategories adapter;
    private FrameLayout frameLayout;
    private ProgressDialog progressDialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        progressDialog = new ProgressDialog(CatchUpActivity.this, true);

        jsHelper = new JSHelper(this);
        helper = new Helper(this, (position, type) -> openCatchUpLiveActivity(position));

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);

        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        getData();
    }

    private void openCatchUpLiveActivity(int position) {
        Intent intent = new Intent(CatchUpActivity.this, CatchUpLiveActivity.class);
        intent.putExtra("cat_id", arrayList.get(position).getId());
        intent.putExtra("cat_name", arrayList.get(position).getName());
        startActivity(intent);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_catch_up;
    }

    private void getData() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected void onPreExecute() {
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    arrayList.addAll(jsHelper.getCatchUpCategoryLive());
                    if (!arrayList.isEmpty() && Boolean.TRUE.equals(jsHelper.getIsCategoriesOrder())) {
                        Collections.reverse(arrayList);
                    }
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
                if (!arrayList.isEmpty()){
                    setAdapterToListview();
                } else {
                    setEmpty();
                }
            }
        }.execute();
    }

    public void setAdapterToListview() {
        adapter = new AdapterCategories(false,arrayList, position -> helper.showInterAd(position,""));
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

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                finish();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
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