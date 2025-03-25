package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.GetChannelListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;

public class GetChannelSearch extends AsyncTaskExecutor<String, String, String> {

    Context ctx;
    private final JSHelper jsHelper;
    private final GetChannelListener listener;
    private final ArrayList<ItemChannel> itemChannels = new ArrayList<>();
    private final String searchText;
    private final Boolean isPlaylist;
    private static final int MAX_RESULTS = 20;

    public GetChannelSearch(Context ctx,
                            Boolean isPlaylist, String searchText, GetChannelListener listener) {
        this.ctx = ctx;
        this.listener = listener;
        this.isPlaylist = isPlaylist;
        this.searchText = searchText;
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
            ArrayList<ItemChannel> resultList = new ArrayList<>();

            if (Boolean.TRUE.equals(isPlaylist)) {
                ArrayList<ItemChannel> allItems = new ArrayList<>(jsHelper.getLivePlaylist());
                if (allItems.isEmpty()){
                    return "0";
                }

                for (ItemChannel item : allItems) {
                    if (item.getName().toLowerCase().contains(searchText.toLowerCase())) {
                        resultList.add(item);
                    }
                }
            } else {
                resultList.addAll(jsHelper.getLivesSearch(searchText));
            }

            if (resultList.isEmpty()){
                return "0";
            }

            int limit = Math.min(MAX_RESULTS, resultList.size());
            for (int i = 0; i < limit; i++) {
                itemChannels.add(resultList.get(i));
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetChannelSearch", "Error fetching Channels Search", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, itemChannels);
    }
}