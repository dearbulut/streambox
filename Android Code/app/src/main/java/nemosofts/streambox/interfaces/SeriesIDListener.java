package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemInfoSeasons;
import nemosofts.streambox.item.ItemSeasons;

public interface SeriesIDListener {
    void onStart();
    void onEnd(String success, ItemInfoSeasons infoSeasons,
               ArrayList<ItemSeasons> arrayListSeasons, ArrayList<ItemEpisodes> arrayListEpisodes
    );
}