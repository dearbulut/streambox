package nemosofts.streambox.executor;

import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.interfaces.UsersListener;
import nemosofts.streambox.item.ItemUsers;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import okhttp3.RequestBody;

public class LoadUsers extends AsyncTaskExecutor<String, String, String> {

    private final RequestBody requestBody;
    private final UsersListener listener;
    private final ArrayList<ItemUsers> arrayList = new ArrayList<>();
    private String verifyStatus = "0";
    private String message = "";

    public LoadUsers(UsersListener usersListener, RequestBody requestBody) {
        this.listener = usersListener;
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
                JSONObject obj = jsonArray.getJSONObject(i);

                if (!obj.has(Callback.TAG_SUCCESS)) {
                    String id = obj.getString("id");
                    String userType = obj.getString("user_type");
                    String userName = obj.getString("user_name");
                    String userPassword = obj.getString("user_password");
                    String base = obj.getString("dns_base");

                    String deviceID="";
                    if (obj.has("device_id")){
                        deviceID = obj.getString("device_id");
                    } else if (obj.has("activation_code")){
                        deviceID = obj.getString("activation_code");
                    }

                    ItemUsers objItem = new ItemUsers(
                            id, userType, userName, userPassword, base, deviceID
                    );
                    arrayList.add(objItem);
                } else {
                    verifyStatus = obj.getString(Callback.TAG_SUCCESS);
                    message = obj.getString(Callback.TAG_MSG);
                }
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadUsers", "Error loading user data", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, verifyStatus, message, arrayList);
    }
}