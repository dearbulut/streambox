package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterChannelEpg;
import nemosofts.streambox.adapter.AdapterEpg;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadEpg;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.item.ItemPost;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class EPGOneActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private JSHelper jsHelper;
    private String catID = "0";
    private RecyclerView rvLive;
    private ArrayList<ItemChannel> arrayList;
    private final ArrayList<ItemPost> arrayListPost = new ArrayList<>();
    private AdapterChannelEpg adapter;
    private ProgressBar pb;
    private int pos = 0;
    private RecyclerView rvHome;
    private AdapterEpg adapterHome = null;

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

        catID = getIntent().getStringExtra("cat_id");

        jsHelper = new JSHelper(this);
        spHelper = new SPHelper(this);

        arrayList = new ArrayList<>();

        pb = findViewById(R.id.pb);
        rvLive = findViewById(R.id.rv_live);
        rvLive.setLayoutManager(new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false));
        rvLive.setNestedScrollingEnabled(false);

        rvHome = findViewById(R.id.rv_epg);
        rvHome.setLayoutManager(new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false));
        rvHome.setItemAnimator(new DefaultItemAnimator());

        getData();

        LinearLayout adView = findViewById(R.id.ll_adView);
        new Helper(this).showBannerAd(adView, Callback.getBannerEpg());
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_epg_one;
    }

    private void getData() {
        new AsyncTaskExecutor<String, String, String>() {

            final ArrayList<ItemChannel> itemChannels = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                pb.setVisibility(View.VISIBLE);
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    itemChannels.addAll(jsHelper.getLive(catID));
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
                pb.setVisibility(View.GONE);
                if (itemChannels.isEmpty()) {
                    findViewById(R.id.ll_epg).setVisibility(View.GONE);
                    findViewById(R.id.ll_epg_empty).setVisibility(View.VISIBLE);
                } else {
                    arrayList.addAll(itemChannels);
                    setAdapterToListview();
                }
            }
        }.execute();
    }

    public void setAdapterToListview() {
        adapter = new AdapterChannelEpg(this, arrayList, (itemCat, position) -> {
            pos = position;
            adapter.select(pos);
            setMediaSource();
        });
        rvLive.setAdapter(adapter);
        adapter.select(pos);
        setMediaSource();
    }

    private void setMediaSource() {
        ItemPost itemPost = new ItemPost("1","logo");
        ArrayList<ItemChannel> arrayListLive = new ArrayList<>();
        arrayListLive.add(arrayList.get(pos));
        itemPost.setArrayListLive(arrayListLive);
        arrayListPost.add(itemPost);
        getEpgData(pos);
    }

    private void getEpgData(int playPos) {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    pb.setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (isFinishing()){
                        return;
                    }
                    pb.setVisibility(View.GONE);
                    if (!epgArrayList.isEmpty()){
                        setEpg(epgArrayList);
                    } else {
                        setEpg(null);
                    }
                }
            }, ApplicationUtil.getAPIRequestID("get_simple_data_table","stream_id",
                    arrayList.get(playPos).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void setEpg(ArrayList<ItemEpg> arrayListEpg) {
        ItemPost itemPost = new ItemPost("1","listings");
        if (arrayListEpg != null && !arrayListEpg.isEmpty()){
            itemPost.setArrayListEpg(arrayListEpg);
        } else {
            ArrayList<ItemEpg> arrayListEp = new ArrayList<>();
            arrayListEp.add(new ItemEpg("","", ApplicationUtil.encodeBase64("No Data Found"),"",""));
            itemPost.setArrayListEpg(arrayListEp);
        }
        arrayListPost.add(itemPost);

        if (adapterHome == null){
            adapterHome = new AdapterEpg(this, spHelper.getIs12Format(), arrayListPost);
            rvHome.setAdapter(adapterHome);
        } else {
            adapter.notifyItemInserted(arrayList.size() - 1);
        }
        rvHome.scrollToPosition(arrayListPost.size() - 1);
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
}