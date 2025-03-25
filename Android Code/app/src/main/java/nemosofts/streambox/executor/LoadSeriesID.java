package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.SeriesIDListener;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemInfoSeasons;
import nemosofts.streambox.item.ItemSeasons;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadSeriesID extends AsyncTaskExecutor<String, String, String> {

    private final SPHelper spHelper;
    private final SeriesIDListener listener;
    private final RequestBody requestBody;
    private ItemInfoSeasons arrayListInfo;
    private final ArrayList<ItemSeasons> arrayListSeries = new ArrayList<>();
    private final ArrayList<ItemEpisodes> arrayListEpisodes = new ArrayList<>();

    public LoadSeriesID(Context ctx, SeriesIDListener listener, RequestBody requestBody) {
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

            if (jsonObject.has("info")) {

                JSONObject c =  jsonObject.getJSONObject("info");

                String name = c.optString("name","");
                String cover = c.optString("cover","null").replace(" ", "%20");
                String plot = c.optString("plot","");
                String director = c.optString("director","");
                String genre = c.optString("genre","");
                String releaseDate = c.optString("releaseDate","");
                String rating = c.optString("rating","");
                String rating5 = c.optString("rating_5based","");
                String youtubeTrailer = c.optString("youtube_trailer","");

                arrayListInfo = new ItemInfoSeasons(
                        name, cover, plot, director, genre, releaseDate, rating, rating5, youtubeTrailer
                );
            }

            if (jsonObject.has("episodes")) {

                JSONObject cx =  jsonObject.getJSONObject("episodes");

                for (int h = 0; h < 30; h++) {

                    if (cx.has(String.valueOf(h))) {
                        arrayListSeries.add(new ItemSeasons("Seasons "+ h, String.valueOf(h)));

                        JSONArray cm = cx.getJSONArray(String.valueOf(h));
                        for (int i = 0; i < cm.length(); i++) {
                            JSONObject object = cm.getJSONObject(i);

                            String id = object.getString("id");
                            String title = object.getString("title");
                            String containerExtension = object.getString("container_extension");
                            String season = object.getString("season");

                            // info
                            JSONObject object2 = object.getJSONObject("info");

                            String plot = object2.optString("plot","");
                            String duration = object2.optString("duration","0");
                            String movieImage = object2.optString("movie_image","null").replace(" ", "%20");
                            String rating = object2.optString("rating","0");

                            ItemEpisodes episodes = new ItemEpisodes(
                                    id, title,
                                    containerExtension, season, plot, duration, rating, movieImage
                            );
                            arrayListEpisodes.add(episodes);
                        }
                    }
                }
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadSeriesID", "Error loading series id data", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayListInfo, arrayListSeries, arrayListEpisodes);
    }
}