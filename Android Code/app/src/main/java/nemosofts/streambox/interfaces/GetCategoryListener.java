package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemCat;

public interface GetCategoryListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemCat> arrayListCat);
}