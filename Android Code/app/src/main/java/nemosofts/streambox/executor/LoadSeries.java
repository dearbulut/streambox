package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import org.json.JSONArray;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.HttpsTrustManager;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadSeries extends AsyncTaskExecutor<String, String, String> {

    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final LoadSuccessListener listener;
    private String msg = "";


    public LoadSeries(Context ctx, LoadSuccessListener listener) {
        this.listener = listener;
        jsHelper = new JSHelper(ctx);
        spHelper = new SPHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllSeries(); // Clear existing series data
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String string) {
        try {
            // Fetch series categories
            String jsonCategory = fetchData("get_series_categories");
            if (jsonCategory.isEmpty()) {
                msg = "No series categories found";
                return "3";
            }
            jsHelper.addToSeriesCatData(jsonCategory);

            // Fetch series data
            String jsonSeries = fetchData("get_series");
            if (jsonSeries.isEmpty()) {
                msg = "No series found";
                return "3";
            }
            jsHelper.setSeriesSize(new JSONArray(jsonSeries).length());
            jsHelper.addToSeriesData(jsonSeries);

            return "1";
        } catch (Exception e) {
            msg = "An error occurred. Please try again.";
            Log.e("LoadSeries", "Error loading series data", e);
            return "0";
        }
    }

    @NonNull
    private String fetchData(String action) {
        try {
            // Try fetching data via ApplicationUtil
            String response = ApplicationUtil.responsePost(spHelper.getAPI(),
                    ApplicationUtil.getAPIRequest(action, spHelper.getUserName(), spHelper.getPassword())
            );
            if (!response.isEmpty()) {
                return response;
            }

            // Fallback: fetch data using HTTP connection
            return performHttpRequest(action);
        } catch (Exception e) {
            return "";
        }
    }

    @NonNull
    private String performHttpRequest(String action) throws IOException {
        HttpsTrustManager.allowAllSSL();

        // Construct the URL with query parameters
        URL url = new URL(spHelper.getAPI() + "?username=" + spHelper.getUserName()
                + "&password=" + spHelper.getPassword() + "&action=" + action
        );
        HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
        urlConnection.setRequestMethod("GET");
        urlConnection.connect();

        // Read the response from InputStream
        try (InputStream inputStream = urlConnection.getInputStream();
             BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {

            if (inputStream == null) {
                return "";
            }

            StringBuilder stringBuilder = new StringBuilder();
            String line;

            while ((line = reader.readLine()) != null) {
                stringBuilder.append(line);
            }

            return stringBuilder.toString().isEmpty() ? "" : stringBuilder.toString();
        }
    }

    @Override
    protected void onPostExecute(String result) {
        listener.onEnd(result, msg);
    }
}
