package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.EpgFullListener;
import nemosofts.streambox.item.ItemEpgFull;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadEpgFull extends AsyncTaskExecutor<String, String, String> {

    private final SPHelper spHelper;
    private final RequestBody requestBody;
    private final EpgFullListener listener;
    private final ArrayList<ItemEpgFull> arrayList = new ArrayList<>();

    public LoadEpgFull(Context ctx, EpgFullListener listener, RequestBody requestBody) {
        this.listener = listener;
        this.requestBody = requestBody;
        spHelper = new SPHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {

            String json = ApplicationUtil.responsePost(spHelper.getAPI(), requestBody);
            JSONObject jsonObject = new JSONObject(json);

            if (jsonObject.has("epg_listings")) {

                JSONArray c =  jsonObject.getJSONArray("epg_listings");

                for (int i = 0; i < c.length(); i++) {
                    JSONObject jsonobject = c.getJSONObject(i);

                    String id = jsonobject.getString("id");
                    String start = jsonobject.getString("start");
                    String end = jsonobject.getString("end");
                    String title = jsonobject.getString("title");
                    String startTimestamp = jsonobject.getString("start_timestamp");
                    String stopTimestamp = jsonobject.getString("stop_timestamp");
                    int hasArchive = jsonobject.getInt("has_archive");
                    if (jsonobject.getInt("has_archive") == 1){
                        ItemEpgFull objItem = new ItemEpgFull(
                                id, start, end, title, startTimestamp, stopTimestamp, hasArchive
                        );
                        arrayList.add(objItem);
                    }
                }
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadEpgFull", "Error loading full epg data", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayList);
    }

}