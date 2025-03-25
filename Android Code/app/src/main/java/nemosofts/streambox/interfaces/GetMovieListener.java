package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemMovies;

public interface GetMovieListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemMovies> arrayListMovies);
}