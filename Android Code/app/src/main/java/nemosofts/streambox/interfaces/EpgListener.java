package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemEpg;

public interface EpgListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemEpg> epgArrayList);
}