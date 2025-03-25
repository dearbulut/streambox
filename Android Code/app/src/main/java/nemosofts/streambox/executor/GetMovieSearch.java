package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;

public class GetMovieSearch extends AsyncTaskExecutor<String, String, String> {

    private final JSHelper jsHelper;
    private final GetMovieListener listener;
    private final ArrayList<ItemMovies> itemMovies = new ArrayList<>();
    private final String searchText;
    private final Boolean isPlaylist;
    private static final int MAX_RESULTS = 20;

    public GetMovieSearch(Context ctx, Boolean isPlaylist, String searchText, GetMovieListener listener) {
        this.listener = listener;
        this.isPlaylist = isPlaylist;
        this.searchText = searchText;
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            ArrayList<ItemMovies> moviesList = new ArrayList<>();
            if (Boolean.TRUE.equals(isPlaylist)) {
                moviesList.addAll(jsHelper.getMoviesPlaylist());
            } else {
                moviesList.addAll(jsHelper.getMoviesSearch(searchText));
            }

            if (moviesList.isEmpty()){
                return "0";
            }

            int limit = Math.min(MAX_RESULTS, moviesList.size());
            for (int i = 0; i < limit; i++) {
                ItemMovies movie = moviesList.get(i);
                if (movie.getName().toLowerCase().contains(searchText)) {
                    itemMovies.add(movie);
                }
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetMovieSearch", "Error fetching movies search", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemMovies);
    }
}