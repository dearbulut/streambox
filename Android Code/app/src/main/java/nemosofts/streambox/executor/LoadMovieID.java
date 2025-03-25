package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.MovieIDListener;
import nemosofts.streambox.item.ItemInfoMovies;
import nemosofts.streambox.item.ItemMoviesData;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadMovieID extends AsyncTaskExecutor<String, String, String> {

    private final SPHelper spHelper;
    private final MovieIDListener listener;
    private final RequestBody requestBody;
    private final ArrayList<ItemInfoMovies> arrayListInfo = new ArrayList<>();
    private final ArrayList<ItemMoviesData> arrayListData = new ArrayList<>();

    public LoadMovieID(Context ctx, MovieIDListener listener, RequestBody requestBody) {
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

                String tmdbID = c.optString("tmdb_id","");
                String name = c.optString("name","");
                String movieImage = c.optString("movie_image","null").replace(" ", "%20");
                String releaseDate = c.optString("release_date","");
                String episodeRunTime = c.optString("episode_run_time","");
                String youtUrl = c.optString("youtube_trailer","");
                String director = c.optString("director","");
                String cast = c.optString("cast","");
                String plot = c.optString("plot","");
                String genre = c.optString("genre","");
                String rating = c.optString("rating","");

                ItemInfoMovies objItem1 = new ItemInfoMovies(tmdbID, name, movieImage,
                        releaseDate, episodeRunTime, youtUrl, director, cast, plot, genre, rating);
                arrayListInfo.add(objItem1);
            }

            if (jsonObject.has("movie_data")) {

                JSONObject c =  jsonObject.getJSONObject("movie_data");

                String streamID = c.getString("stream_id");
                String name = c.getString("name");
                String container = c.getString("container_extension");

                ItemMoviesData objItem2 = new ItemMoviesData(streamID, name, container);
                arrayListData.add(objItem2);
            }
            return "1";
        } catch (Exception e) {
            Log.e("LoadMovieID", "Error loading movie data", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayListInfo, arrayListData);
    }
}