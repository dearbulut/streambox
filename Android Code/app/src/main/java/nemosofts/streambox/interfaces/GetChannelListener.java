package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemChannel;

public interface GetChannelListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemChannel> arrayListLive);
}