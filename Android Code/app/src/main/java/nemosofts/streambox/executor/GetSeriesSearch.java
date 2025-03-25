package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;

public class GetSeriesSearch extends AsyncTaskExecutor<String, String, String> {

    private final JSHelper jsHelper;
    private final GetSeriesListener listener;
    private final ArrayList<ItemSeries> itemSeries = new ArrayList<>();
    private final String searchText;
    private static final int MAX_RESULTS = 20;

    public GetSeriesSearch(Context ctx, String searchText, GetSeriesListener listener) {
        this.listener = listener;
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
            ArrayList<ItemSeries> searchResults = new ArrayList<>(jsHelper.getSeriesSearch(searchText));
            if (searchResults.isEmpty()){
                return "0";
            }

            Collections.reverse(searchResults);

            int limit = Math.min(MAX_RESULTS, searchResults.size());
            for (int j = 0; j < limit; j++) {
                itemSeries.add(searchResults.get(j));
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetSeriesSearch", "Error fetching series search", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemSeries);
    }
}