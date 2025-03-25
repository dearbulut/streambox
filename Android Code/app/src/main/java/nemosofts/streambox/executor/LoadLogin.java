package nemosofts.streambox.executor;

import android.util.Log;

import org.json.JSONObject;

import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import okhttp3.RequestBody;

public class LoadLogin extends AsyncTaskExecutor<String, String, String> {

    private final RequestBody requestBody;
    private final LoginListener listener;
    private ItemLoginUser itemUser;
    private ItemLoginServer itemServer;
    private final String apiUrl;
    private String allowedOutputFormats = "";

    public LoadLogin(LoginListener listener, String apiUrl , RequestBody requestBody) {
        this.listener = listener;
        this.apiUrl = apiUrl;
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
            String json = ApplicationUtil.responsePost(apiUrl+"/player_api.php", requestBody);

            JSONObject mainJson = new JSONObject(json);

            JSONObject userInfo = mainJson.getJSONObject("user_info");
            String username = userInfo.getString("username");
            String password = userInfo.getString("password");
            String message = userInfo.getString("message");
            String status = userInfo.getString("status");
            String expDate = userInfo.getString("exp_date");
            String activeCons = userInfo.getString("active_cons");
            String maxConnections = userInfo.getString("max_connections");

            if (userInfo.has("allowed_output_formats")){
               allowedOutputFormats = userInfo.getString("allowed_output_formats");
            }
            itemUser = new ItemLoginUser(
                    username, password, message, status, expDate, activeCons, maxConnections
            );

            // Server info
            JSONObject serverInfo = mainJson.getJSONObject("server_info");
            boolean xui = false;
            if (serverInfo.has("xui")){
                xui = serverInfo.getBoolean("xui");
            }
            String url = serverInfo.getString("url");
            String port = serverInfo.getString("port");
            String httpsPort = serverInfo.getString("https_port");
            String serverProtocol = serverInfo.getString("server_protocol");

            itemServer = new ItemLoginServer(xui, url, port, httpsPort, serverProtocol);
            return "1";
        } catch (Exception e) {
            Log.e("LoadLogin", "Error login", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, itemUser, itemServer, allowedOutputFormats);
    }

}