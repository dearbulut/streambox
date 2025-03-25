package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemPoster;

public interface PosterListener {
    void onStart();
    void onEnd(String success, String verifyStatus, String message, ArrayList<ItemPoster> arrayList);
}