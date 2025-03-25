package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemInfoMovies;
import nemosofts.streambox.item.ItemMoviesData;

public interface MovieIDListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemInfoMovies> arrayListInfo ,
               ArrayList<ItemMoviesData> arrayListMoviesData
    );
}