package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCatchUpEpg;
import nemosofts.streambox.adapter.AdapterTabEpg;
import nemosofts.streambox.executor.LoadEpgFull;
import nemosofts.streambox.interfaces.EpgFullListener;
import nemosofts.streambox.item.ItemEpgFull;
import nemosofts.streambox.item.ItemSeasons;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.SPHelper;

public class CatchUpEpgActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private String streamID = "0";
    private ArrayList<ItemSeasons> arraySeasons;
    private ArrayList<ItemEpgFull> arrayList;
    private ArrayList<ItemEpgFull> arrayListFilter;
    private FrameLayout frameLayout;
    private FrameLayout frameLayoutEpg;
    private ProgressDialog progressDialog;
    private ProgressBar pb;
    private SimpleDateFormat inputFormat;

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

        streamID = getIntent().getStringExtra("stream_id");
        String streamName = getIntent().getStringExtra("stream_name");
        String streamIcon= getIntent().getStringExtra("stream_icon");

        TextView title = findViewById(R.id.tv_page_title);
        title.setText(streamName);

        ImageView logo = findViewById(R.id.iv_app_logo);
        if (streamIcon == null || streamIcon.isEmpty()){
            Picasso.get()
                    .load(streamIcon)
                    .placeholder(R.drawable.bg_card_item_load)
                    .error(R.drawable.bg_card_item_load)
                    .into(logo);
        }

        inputFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        progressDialog = new ProgressDialog(CatchUpEpgActivity.this, true);

        spHelper = new SPHelper(this);

        arrayList = new ArrayList<>();
        arraySeasons = new ArrayList<>();
        arrayListFilter = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        frameLayoutEpg = findViewById(R.id.fl_empty_epg);
        pb = findViewById(R.id.pb_epg);

        loadEpg();
    }

    private void loadEpg() {
        if (!NetworkUtils.isConnected(this)){
            setEmpty();
            return;
        }
        LoadEpgFull loadEpgFull = new LoadEpgFull(this, new EpgFullListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ArrayList<ItemEpgFull> epgArrayList) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (!epgArrayList.isEmpty()){
                    if (!arrayList.isEmpty()){
                        arrayList.clear();
                    }
                    arrayList.addAll(epgArrayList);
                    filterEpgList();
                } else {
                    setEmpty();
                }
            }
        }, ApplicationUtil.getAPIRequestID("get_simple_data_table","stream_id",
                streamID, spHelper.getUserName(), spHelper.getPassword()));
        loadEpgFull.execute();
    }

    private void filterEpgList() {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected void onPreExecute() {
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    SimpleDateFormat outputFormat = new SimpleDateFormat("MMM dd", Locale.getDefault());
                    SimpleDateFormat outputFormatFull = new SimpleDateFormat("yyyy-MM-dd", Locale.getDefault());
                    Set<String> seenDates = new HashSet<>();
                    for (int i = 0; i < arrayList.size(); i++) {
                        Date date = inputFormat.parse(arrayList.get(i).getStart());
                        if (date != null) {
                            String formattedDate = outputFormat.format(date);
                            String formattedDateFull = outputFormatFull.format(date);
                            if (!seenDates.contains(formattedDate)) {
                                seenDates.add(formattedDate);
                                arraySeasons.add(new ItemSeasons(formattedDate, formattedDateFull));
                            }
                        }
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
                setSeasonsAdapter();
            }
        }.execute();
    }

    private void setSeasonsAdapter() {
        if (arraySeasons.isEmpty()){
            setEmpty();
            return;
        }
        RecyclerView rvSeasons = findViewById(R.id.rv_tab);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rvSeasons.setLayoutManager(llm);
        rvSeasons.setNestedScrollingEnabled(false);
        AdapterTabEpg adapterTabEpg = new AdapterTabEpg(this, arraySeasons, (itemSeasons, position) -> filterDate(arraySeasons.get(position)));
        rvSeasons.setAdapter(adapterTabEpg);
        filterDate(arraySeasons.get(arraySeasons.size()-1));
        if (ApplicationUtil.isTvBox(this)){
            rvSeasons.requestFocus();
        }
    }

    private void filterDate(ItemSeasons itemSeasons) {
        new AsyncTaskExecutor<String, String, String>() {

            @Override
            protected void onPreExecute() {
                pb.setVisibility(View.VISIBLE);
                frameLayoutEpg.setVisibility(View.GONE);
                findViewById(R.id.rv_epg).setVisibility(View.GONE);
                if (!arrayListFilter.isEmpty()){
                    arrayListFilter.clear();
                }
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String strings) {
                try {
                    SimpleDateFormat outputFormat = new SimpleDateFormat("yyyy-MM-dd", Locale.getDefault());
                    for (int i = 0; i < arrayList.size(); i++) {
                        Date date = inputFormat.parse(arrayList.get(i).getStart());
                        if (date != null) {
                            String formattedDate = outputFormat.format(date);
                            if (itemSeasons.getSeasonNumber().equals(formattedDate)) {
                                arrayListFilter.add(arrayList.get(i));
                            }
                        }
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
                pb.setVisibility(View.GONE);
                setEpgAdapter();
            }
        }.execute();
    }

    private void setEpgAdapter() {
        if (arrayListFilter.isEmpty()){
            setEmptyEpg();
            return;
        }
        RecyclerView rv = findViewById(R.id.rv_epg);
        LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv.setLayoutManager(manager);
        rv.setNestedScrollingEnabled(false);
        AdapterCatchUpEpg adapterTabEpg = new AdapterCatchUpEpg(spHelper.getIs12Format(), arrayListFilter, (itemEpgFull, position) -> {
            if (arrayListFilter.get(position).getHasArchive() == 1){
                openPlayer(arrayListFilter.get(position));
            }
        });
        rv.setAdapter(adapterTabEpg);
        rv.setVisibility(View.VISIBLE);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openPlayer(ItemEpgFull item) {
        try {
            SimpleDateFormat inputFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
            SimpleDateFormat outputFormatData = new SimpleDateFormat("yyyy-MM-dd:HH:mm:ss", Locale.getDefault());
            SimpleDateFormat outputFormat = new SimpleDateFormat("HH:mm:ss", Locale.getDefault());
            Date dateStart = inputFormat2.parse(item.getStart());
            Date dateEnd = inputFormat2.parse(item.getEnd());
            if (dateStart != null && dateEnd != null) {
                String formattedDateStart = outputFormat.format(dateStart);
                String formattedDateEnd = outputFormat.format(dateEnd);
                String start = outputFormatData.format(dateStart);

                Date startTime = outputFormat.parse(formattedDateStart);
                Date endTime = outputFormat.parse(formattedDateEnd);
                if (startTime != null && endTime != null) {
                    long durationInMillis = endTime.getTime() - startTime.getTime();
                    long durationInMinutes = durationInMillis / (1000 * 60);
                    String duration = String.valueOf(durationInMinutes);
                    String channelUrl = spHelper.getServerURL()
                            + "streaming/timeshift.php?username="
                            + spHelper.getUserName()
                            + "&password="+spHelper.getPassword()
                            + "&stream="+streamID
                            + "&start="+start
                            + "&duration="+duration;
                    Intent intent = new Intent(CatchUpEpgActivity.this, PlayerSingleURLActivity.class);
                    intent.putExtra("channel_title", ApplicationUtil.decodeBase64(item.getTitle()));
                    intent.putExtra("channel_url", channelUrl);
                    startActivity(intent);
                }
            }
        } catch (Exception e) {
            Toasty.makeText(CatchUpEpgActivity.this,true, "Unexpected error", Toasty.ERROR);
        }
    }

    private void setEmptyEpg() {
        if (!arrayListFilter.isEmpty()) {
            frameLayoutEpg.setVisibility(View.GONE);
        } else {

            frameLayoutEpg.setVisibility(View.VISIBLE);

            frameLayoutEpg.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayoutEpg.addView(myView);
        }
    }

    private void setEmpty() {
        if (!arraySeasons.isEmpty()) {
            frameLayout.setVisibility(View.GONE);
        } else {

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
        return R.layout.activity_catch_up_epg;
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