package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;

public class GetSeries extends AsyncTaskExecutor<String, String, String> {

    private static final int PAGE_TYPE_FAV = 1;
    private static final int PAGE_TYPE_RECENT = 2;
    private static final int PAGE_TYPE_RECENT_ADD = 3;

    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetSeriesListener listener;
    private final ArrayList<ItemSeries> itemSeries = new ArrayList<>();
    private final int isPage;
    private final String catId;
    private final int page;
    private static final int ITEMS_PER_PAGE = 15;

    public GetSeries(Context ctx, int page, String catId, int isPage, GetSeriesListener listener) {
        this.listener = listener;
        this.isPage = isPage;
        this.catId = catId;
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
                    itemSeries.addAll(dbHelper.getSeries(DBHelper.TABLE_FAV_SERIES, jsHelper.getIsSeriesOrder()));
                    break;
                case PAGE_TYPE_RECENT:
                    itemSeries.addAll(dbHelper.getSeries(DBHelper.TABLE_RECENT_SERIES, jsHelper.getIsSeriesOrder()));
                    break;
                case PAGE_TYPE_RECENT_ADD:
                    ArrayList<ItemSeries> recommendedSeries = new ArrayList<>(jsHelper.getSeriesRe());
                    if (recommendedSeries.isEmpty()){
                        return "0";
                    }
                    Collections.sort(recommendedSeries, (o1, o2) -> Integer.compare(Integer.parseInt(o1.getSeriesID()), Integer.parseInt(o2.getSeriesID())));
                    Collections.reverse(recommendedSeries);
                    itemSeries.addAll(recommendedSeries.subList(0, Math.min(50, recommendedSeries.size())));
                    if (Boolean.TRUE.equals(jsHelper.getIsSeriesOrder()) && !itemSeries.isEmpty()) {
                        Collections.reverse(itemSeries);
                    }
                    break;
                default:
                    ArrayList<ItemSeries> categorySeries = new ArrayList<>(jsHelper.getSeries(catId));
                    if (categorySeries.isEmpty()){
                        return "0";
                    }
                    if (Boolean.TRUE.equals(jsHelper.getIsSeriesOrder())) {
                        Collections.reverse(categorySeries);
                    }
                    int startIndex = (page - 1) * ITEMS_PER_PAGE;
                    int endIndex = Math.min(startIndex + ITEMS_PER_PAGE, categorySeries.size());
                    itemSeries.addAll(categorySeries.subList(startIndex, endIndex));
                    break;
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetSeries", "Error fetching series", e);
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemSeries);
    }
}