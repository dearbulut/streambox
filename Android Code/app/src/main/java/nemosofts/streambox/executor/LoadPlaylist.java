package nemosofts.streambox.executor;

import android.content.Context;
import android.net.Uri;
import android.util.Log;

import androidx.annotation.NonNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nemosofts.streambox.R;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.HttpsTrustManager;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.logging.HttpLoggingInterceptor;

public class LoadPlaylist extends AsyncTaskExecutor<String, String, String> {

    private final Context ctx;
    private final LoadPlaylistListener listener;
    private final ArrayList<ItemPlaylist> playlist = new ArrayList<>();
    private final Boolean isFile;
    private final String filePath;
    private String msg = "";

    private static final String EXTINF_PREFIX = "#EXTINF:-1";
    private static final String TVG_NAME_PATTERN = "tvg-name=\"(.*?)\"";
    private static final String GROUP_TITLE_PATTERN = "group-title=\"([^\"]*)\",(.*?)$";
    private static final String TVG_LOGO_PATTERN = "tvg-logo=\"(.*?)\"";

    public LoadPlaylist(Context ctx, Boolean isFile, String filePath, LoadPlaylistListener listener) {
        this.ctx = ctx;
        this.listener = listener;
        this.isFile = isFile;
        this.filePath = filePath;
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            if (Boolean.TRUE.equals(isFile)) {
                return processFileFromUri();
            } else {
                return processHttpRequest();
            }
        } catch (Exception e) {
            msg = ctx.getString(R.string.err_server_not_connected);
            return "0";
        }
    }

    @NonNull
    private String processFileFromUri() {
        try (InputStream inputStream = ctx.getContentResolver().openInputStream(Uri.parse(filePath));
             BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {

            if (inputStream == null) {
                msg = "File not found or unable to open";
                return "0";
            }

            return processFile(reader);
        } catch (IOException e) {
            msg = "Error reading file";
            Log.e("LoadPlaylist", "Error loading playlist data", e);
            return "0";
        }
    }

    @NonNull
    private String processHttpRequest() throws IOException {
        HttpsTrustManager.allowAllSSL();

        OkHttpClient client = createHttpClient();
        Request request = new Request.Builder().url(filePath).build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                msg = "HTTP request failed";
                return "0";
            }

            if (response.body() == null) {
                msg = "Response body is empty";
                return "0";
            }

            try (BufferedReader reader = new BufferedReader(new InputStreamReader(response.body().byteStream()))) {
                return processFile(reader);
            }
        }
    }

    @NonNull
    private OkHttpClient createHttpClient() {
        HttpLoggingInterceptor logging = new HttpLoggingInterceptor();
        logging.setLevel(HttpLoggingInterceptor.Level.BODY);
        return new OkHttpClient.Builder()
                .connectTimeout(15, TimeUnit.SECONDS)
                .writeTimeout(20, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .addInterceptor(logging)
                .cache(null)
                .build();
    }

    @NonNull
    private String processFile(BufferedReader reader) throws IOException {
        String line;
        String name = null;
        String logo = null;
        String group = null;
        while ((line = reader.readLine()) != null) {
            if (line.startsWith(EXTINF_PREFIX)) {
                String data = line.substring(EXTINF_PREFIX.length()).trim();
                name = extractData(data, TVG_NAME_PATTERN);
                if (name.isEmpty()) {
                    // Extract name from remaining data if tvg-name is missing
                    String[] parts = data.split(",", 2);
                    name = parts.length > 1 ? parts[1].trim() : "Unknown";
                }
                logo = extractData(data, TVG_LOGO_PATTERN);
                group = extractData(data, GROUP_TITLE_PATTERN);
            } else if ((line.startsWith("http") || line.startsWith("https")) && name != null) {
                addPlaylistItem(line, name, logo, group);
                name = null;
                logo = null;
                group = null;
            }
        }
        msg = "Successfully loaded playlist";
        return "1";
    }

    private void addPlaylistItem(String line, String name, String logo, String group) {
        playlist.add(new ItemPlaylist(
                name,
                logo != null ? logo : "null", // Use empty string if logo is null
                group != null ? group : "Uncategorized", // Default group if missing
                line
        ));
    }

    private String extractData(String data, String pattern) {
        try {
            Pattern compiledPattern = Pattern.compile(pattern);
            Matcher matcher = compiledPattern.matcher(data);
            if (matcher.find()) {
                return matcher.group(1);
            }
        } catch (Exception e) {
            return "";
        }
        return "";
    }

    @Override
    protected void onPostExecute(String result) {
        listener.onEnd(result, msg, playlist);
    }
}