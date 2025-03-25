package nemosofts.streambox.executor;

import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.interfaces.StatusListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import okhttp3.RequestBody;

public class LoadStatus extends AsyncTaskExecutor<String, String, String> {

    private final RequestBody requestBody;
    private final StatusListener listener;
    private String success = "0";
    private String message = "";

    public LoadStatus(StatusListener listener, RequestBody requestBody) {
        this.listener = listener;
        this.requestBody = requestBody;
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            String json = ApplicationUtil.responsePost(Callback.API_URL, requestBody);
            JSONObject mainJson = new JSONObject(json);
            JSONArray jsonArray = mainJson.getJSONArray(Callback.TAG_ROOT);
            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject c = jsonArray.getJSONObject(i);
                success = c.getString(Callback.TAG_SUCCESS);
                message = c.getString(Callback.TAG_MSG);
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadStatus", "Error loading status", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, success, message);
    }
}