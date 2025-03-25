package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;

public class GetCategory extends AsyncTaskExecutor<String, String, String> {

    private static final int PAGE_TYPE_LIVE = 1;
    private static final int PAGE_TYPE_MOVIE = 2;
    private static final int PAGE_TYPE_SERIES = 3;
    private static final int PAGE_TYPE_PLAYLIST_4 = 4;
    private static final int PAGE_TYPE_PLAYLIST_5 = 5;

    private final JSHelper jsHelper;
    private final GetCategoryListener listener;
    private final ArrayList<ItemCat> itemCat = new ArrayList<>();
    private final int pageType;

    public GetCategory(Context ctx, int pageType, GetCategoryListener listener) {
        this.listener = listener;
        this.pageType = pageType;
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String s) {
        try {
            switch (pageType) {
                case PAGE_TYPE_LIVE -> itemCat.addAll(jsHelper.getCategoryLive());
                case PAGE_TYPE_MOVIE -> itemCat.addAll(jsHelper.getCategoryMovie());
                case PAGE_TYPE_SERIES -> itemCat.addAll(jsHelper.getCategorySeries());
                case PAGE_TYPE_PLAYLIST_4, PAGE_TYPE_PLAYLIST_5 -> {
                    ArrayList<ItemCat> arrayList = new ArrayList<>(jsHelper.getCategoryPlaylist(pageType));
                    for (int i = 0; i < arrayList.size(); i++) {
                        addOrUpdateItem(itemCat, String.valueOf(i), arrayList.get(i).getName());
                    }
                }
                default -> {
                    return "0";
                }
            }
            if (!itemCat.isEmpty() && Boolean.TRUE.equals(jsHelper.getIsCategoriesOrder())) {
                Collections.reverse(itemCat);
            }
            return "1";
        } catch (Exception e) {
            Log.e("GetCategory", "Error fetching categories", e);
            return "0";
        }
    }

    protected void addOrUpdateItem(@NonNull ArrayList<ItemCat> itemList, String id, String title) {
        boolean idExists = false;
        for (ItemCat item : itemList) {
            if (item.getName().equals(title)) {
                idExists = true;
                break;
            }
        }
        if (!idExists) {
            itemList.add(new ItemCat(id,title,""));
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, itemCat);
    }
}