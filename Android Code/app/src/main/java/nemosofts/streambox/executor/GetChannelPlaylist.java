package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetChannelListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;

public class GetChannelPlaylist extends AsyncTaskExecutor<String, String, String> {

    private final JSHelper jsHelper;
    private final GetChannelListener listener;
    private final ArrayList<ItemChannel> itemChannels = new ArrayList<>();
    private final String categoryName;
    private final int page;
    private static final int ITEMS_PER_PAGE = 10;

    public GetChannelPlaylist(Context ctx, int page, String categoryName, GetChannelListener listener) {
        this.listener = listener;
        this.categoryName = categoryName;
        this.page = page;
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
            // Retrieve the entire live playlist
            ArrayList<ItemChannel> allItems = new ArrayList<>(jsHelper.getLivePlaylist());

            // Filter the items by the specified category
            ArrayList<ItemChannel> filteredItems = new ArrayList<>();
            for (ItemChannel item : allItems) {
                if (item.getCatName().equals(categoryName)) {
                    filteredItems.add(item);
                }
            }

            // Reverse the order if required
            if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder())) {
                Collections.reverse(filteredItems);
            }

            // Apply pagination
            int startIndex = (page - 1) * ITEMS_PER_PAGE;
            int endIndex = Math.min(startIndex + ITEMS_PER_PAGE, filteredItems.size());
            for (int i = startIndex; i < endIndex; i++) {
                itemChannels.add(filteredItems.get(i));
            }

            return "1";
        } catch (Exception e) {
            Log.e("GetChannelPlaylist", "Error fetching Channels Playlist", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, itemChannels);
    }
}