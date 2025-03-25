package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemEpgFull;

public interface EpgFullListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemEpgFull> epgArrayList);
}