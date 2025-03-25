package nemosofts.streambox.executor;

import android.content.Context;

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

public class LoadMovies extends AsyncTaskExecutor<String, String, String> {

    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final LoadSuccessListener listener;
    private String msg = "";

    public LoadMovies(Context ctx, LoadSuccessListener listener) {
        this.listener = listener;
        jsHelper = new JSHelper(ctx);
        spHelper = new SPHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllMovies(); // Clear existing movie data before loading new data
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String string) {
        try {
            // Fetch movie categories
            String jsonCategory = fetchDataFromApi("get_vod_categories");
            if (jsonCategory.isEmpty()) {
                msg = "No movie categories found";
                return "3";
            }
            jsHelper.addToMovieCatData(jsonCategory);

            // Fetch movie streams
            String jsonMovies = fetchDataFromApi("get_vod_streams");
            if (jsonMovies.isEmpty()) {
                msg = "No movie found";
                return "3";
            }
            jsHelper.setMovieSize(new JSONArray(jsonMovies).length());
            jsHelper.addToMovieData(jsonMovies);

            return "1";
        } catch (Exception e) {
            msg = "Please try again";
            return "0";
        }
    }

    @NonNull
    private String fetchDataFromApi(String action) {
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
        URL url = new URL(spHelper.getAPI() + "?username="
                + spHelper.getUserName() + "&password=" + spHelper.getPassword() + "&action=" + action
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
