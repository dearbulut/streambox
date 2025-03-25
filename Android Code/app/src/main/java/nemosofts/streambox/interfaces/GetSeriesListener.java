package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemSeries;

public interface GetSeriesListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemSeries> arrayListSeries);
}