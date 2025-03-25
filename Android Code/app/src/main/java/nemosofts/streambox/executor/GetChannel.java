package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetChannelListener;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;

public class GetChannel extends AsyncTaskExecutor<String, String, String> {

    private static final int PAGE_TYPE_FAV = 1;
    private static final int PAGE_TYPE_RECENT = 2;
    private static final int PAGE_TYPE_RECENT_ADD = 3;
    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetChannelListener listener;
    private final ArrayList<ItemChannel> itemChannels = new ArrayList<>();
    private final int isPage;
    private final String catID;
    private final int page;
    int itemsPerPage = 15;

    public GetChannel(Context ctx, int page, String catID, int isPage, GetChannelListener listener) {
        this.listener = listener;
        this.isPage = isPage;
        this.catID = catID;
        this.page = page;
        jsHelper = new JSHelper(ctx);
        dbHelper = new DBHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            switch (isPage) {
                case PAGE_TYPE_FAV:
                    itemChannels.addAll(dbHelper.getLive(DBHelper.TABLE_FAV_LIVE, jsHelper.getIsLiveOrder()));
                    break;
                case PAGE_TYPE_RECENT:
                    itemChannels.addAll(dbHelper.getLive(DBHelper.TABLE_RECENT_LIVE, jsHelper.getIsLiveOrder()));
                    break;
                case PAGE_TYPE_RECENT_ADD:
                    final ArrayList<ItemChannel> arrayListRe = new ArrayList<>(jsHelper.getLiveRe());
                    if (arrayListRe.isEmpty()){
                        return "0";
                    }
                    fetchRecentAdditions(arrayListRe);
                    break;
                default:
                    final ArrayList<ItemChannel> arrayList = new ArrayList<>(jsHelper.getLive(catID));
                    if (arrayList.isEmpty()){
                        return "0";
                    }

                    if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder())){
                        Collections.reverse(arrayList);
                    }
                    if (!arrayList.isEmpty()){
                        int startIndex = (page - 1) * itemsPerPage;
                        int endIndex = Math.min(startIndex + itemsPerPage, arrayList.size());
                        for (int i = startIndex; i < endIndex; i++) {
                            itemChannels.add(arrayList.get(i));
                        }
                    }
                    break;
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetChannel", "Error fetching Channels", e);
            return "0";
        }
    }

    private void fetchRecentAdditions(ArrayList<ItemChannel> arrayListRe) {
        Collections.sort(arrayListRe, (o1, o2) -> Integer.compare(Integer.parseInt(o1.getStreamID()), Integer.parseInt(o2.getStreamID())));
        Collections.reverse(arrayListRe);
        for (int i = 0; i < arrayListRe.size(); i++) {
            itemChannels.add(arrayListRe.get(i));
            if (i == 49){
                break;
            }
        }
        if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder()) && !itemChannels.isEmpty()){
            Collections.reverse(itemChannels);
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, itemChannels);
    }
}