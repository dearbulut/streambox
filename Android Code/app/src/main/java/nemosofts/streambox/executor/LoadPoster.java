package nemosofts.streambox.executor;

import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.interfaces.PosterListener;
import nemosofts.streambox.item.ItemPoster;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import okhttp3.RequestBody;

public class LoadPoster extends AsyncTaskExecutor<String, String, String> {

    private final RequestBody requestBody;
    private final PosterListener listener;
    private final ArrayList<ItemPoster> arrayList = new ArrayList<>();
    private String verifyStatus = "0";
    private String message = "";

    public LoadPoster(PosterListener listener, RequestBody requestBody) {
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
                JSONObject objJson = jsonArray.getJSONObject(i);
                if (!objJson.has(Callback.TAG_SUCCESS)) {
                    String image = objJson.getString("poster_image");
                    ItemPoster objItem = new ItemPoster(image);
                    arrayList.add(objItem);
                } else {
                    verifyStatus = objJson.getString(Callback.TAG_SUCCESS);
                    message = objJson.getString(Callback.TAG_MSG);
                }
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadPoster", "Error loading poster", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, verifyStatus, message, arrayList);
    }
}