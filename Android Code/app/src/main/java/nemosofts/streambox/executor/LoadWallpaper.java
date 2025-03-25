package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.WallpaperListener;
import nemosofts.streambox.item.ItemWallpaper;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadWallpaper extends AsyncTaskExecutor<String, String, String> {

    private final SPHelper spHelper;
    private final WallpaperListener listener;
    private final String movieId;
    private final ArrayList<ItemWallpaper> arrayList = new ArrayList<>();

    public LoadWallpaper(Context context, String movieId, WallpaperListener listener) {
        this.movieId = movieId;
        this.listener = listener;
        spHelper = new SPHelper(context);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            String json = ApplicationUtil.getMovieImages(movieId, spHelper.getTmdbKEY());
            JSONObject jsonObject = new JSONObject(json);

            // Parse backdrops
            JSONArray backdrops = jsonObject.getJSONArray("backdrops");
            for (int i = 0; i < backdrops.length(); i++) {
                JSONObject backdrop = backdrops.getJSONObject(i);
                String filePath = backdrop.getString("file_path");
                int width = backdrop.getInt("width");
                int height = backdrop.getInt("height");

                ItemWallpaper itemWallpaper = new ItemWallpaper(height, width, filePath);
                arrayList.add(itemWallpaper);
            }

            // Parse posters
            JSONArray posters = jsonObject.getJSONArray("posters");
            for (int i = 0; i < posters.length(); i++) {
                JSONObject poster = posters.getJSONObject(i);
                String filePath = poster.getString("file_path");
                int width = poster.getInt("width");
                int height = poster.getInt("height");
                ItemWallpaper itemWallpaper = new ItemWallpaper(height, width, filePath);
                arrayList.add(itemWallpaper);
            }

            return "1";
        } catch (Exception e) {
            Log.e("LoadStatus", "Error loading status", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayList);
    }
}