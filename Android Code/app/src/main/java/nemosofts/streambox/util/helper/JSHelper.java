package nemosofts.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.nemosofts.BuildConfig;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.json.JSONArray;
import org.json.JSONObject;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.ApplicationUtil;

public class JSHelper {

    private static final String TAG = "JSHelper";
    private final SharedPreferences sp;
    private final SharedPreferences.Editor ed;

    private static final String TAG_JSON_LIVE = "json_live";
    private static final String TAG_JSON_MOVIE = "json_movie";
    private static final String TAG_JSON_SERIES = "json_series";
    private static final String TAG_JSON_LIVE_CAT = "json_live_cat";
    private static final String TAG_JSON_MOVIE_CAT = "json_movie_cat";
    private static final String TAG_JSON_SERIES_CAT = "json_series_cat";
    private static final String TAG_ORDER_LIVE = "live_order";
    private static final String TAG_ORDER_MOVIE = "movie_order";
    private static final String TAG_ORDER_SERIES = "series_order";
    private static final String TAG_UPDATE_DATE = "update_date";
    private static final String TAG_JSON_PLAYLIST = "json_playlist";
    private static final String TAG_SIZE_LIVE = "live_size_all";
    private static final String TAG_SIZE_MOVIE = "movie_size_all";
    private static final String TAG_SIZE_SERIES = "series_size_all";
    private static final String TAG_ORDER_CAT = "is_categories_order";

    private static final String TAG_CAT_ID = "category_id";
    private static final String TAG_CAT_NAME = "category_name";
    private static final String TAG_NAME = "name";
    private static final String TAG_STREAM_ID = "stream_id";
    private static final String TAG_STREAM_ICON = "stream_icon";
    private static final String TAG_STREAM_TYPE = "stream_type";
    private static final String TAG_RATING = "rating";
    private static final String TAG_SERIES_ID = "series_id";
    private static final String TAG_COVER = "cover";
    private static final String TAG_EMPTY = "";

    private static final String TAG_LIVE = "live";
    private static final String TAG_CREATED_LIVE = "created_live";
    private static final String TAG_RADIO_STREAMS = "radio_streams";
    private static final String TAG_LOGO = "logo";
    private static final String TAG_URL = "url";
    private static final String TAG_GROUP = "group";
    private static final String TAG_PLAYLIST_NAME = "playlistName";

    public JSHelper(Context ctx) {
        sp = ctx.getApplicationContext().getSharedPreferences(BuildConfig.APPLICATION_ID + "_" + "json", Context.MODE_PRIVATE);
        ed = sp.edit();
    }

    // CatchUp -------------------------------------------------------------------------------------
    public List<ItemCat> getCatchUpCategoryLive() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            Set<String> seenDates = new HashSet<>();
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (jsonobject.getInt("tv_archive") == 1 && !seenDates.contains(jsonobject.getString(TAG_CAT_ID))) {
                    seenDates.add(jsonobject.getString(TAG_CAT_ID));
                    arrayList.add(categoryIdList(jsonobject.getString(TAG_CAT_ID)));
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in CatchUpCategoryLive", e);
        }
        return arrayList;
    }

    @Nullable
    private ItemCat categoryIdList(String catID) {
        if (catID == null || catID.isEmpty()){
            return null;
        }
        try {
            String json = getString(TAG_JSON_LIVE_CAT, null);
            if (json == null) {
                return null;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);
                if (objectCategory.getString(TAG_CAT_ID).equals(catID)){
                    String id = objectCategory.getString(TAG_CAT_ID);
                    String name = objectCategory.getString(TAG_CAT_NAME);
                    return new ItemCat(id, name, TAG_EMPTY);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in categoryIdList", e);
        }
        return null;
    }

    public List<ItemChannel> getLiveCatchUpLive(String catId) {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        if (catId == null || catId.isEmpty()){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                ItemChannel objItem = new ItemChannel(name, streamID, streamIcon, TAG_EMPTY);
                if (jsonobject.getInt("tv_archive") == 1 && jsonobject.getString(TAG_CAT_ID).equals(catId) && jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_LIVE)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLiveCatchUpLive", e);
        }
        return arrayList;
    }

    // Live ----------------------------------------------------------------------------------------
    public List<ItemChannel> getLive(String catId) {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        if (catId == null || catId.isEmpty()){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                ItemChannel objItem = new ItemChannel(name, streamID, streamIcon, TAG_EMPTY);
                if (jsonobject.getString(TAG_CAT_ID).equals(catId) && jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_LIVE)){
                    arrayList.add(objItem);
                } else if (jsonobject.getString(TAG_CAT_ID).equals(catId) && jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_CREATED_LIVE)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLive", e);
        }
        return arrayList;
    }

    public List<ItemChannel> getLiveRadio() {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                ItemChannel objItem = new ItemChannel(name, streamID, streamIcon, TAG_EMPTY);
                if (jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_RADIO_STREAMS)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLiveRadio", e);
        }
        return arrayList;
    }

    public List<ItemChannel> getLiveRe() {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.isAdultsCount(jsonobject.getString(TAG_NAME))){
                    String name = jsonobject.getString(TAG_NAME);
                    String streamID = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    ItemChannel objItem = new ItemChannel(name,streamID,streamIcon,TAG_EMPTY);
                    if (jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_LIVE)){
                        arrayList.add(objItem);
                    } else if (jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_CREATED_LIVE)){
                        arrayList.add(objItem);
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLiveRe", e);
        }
        return arrayList;
    }

    public List<ItemChannel> getLivesSearch(String searchText) {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        if (searchText == null || searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                ItemChannel objItem = new ItemChannel(name,streamID,streamIcon,TAG_EMPTY);
                if (name.toLowerCase().contains(searchText.toLowerCase())
                        || name.toUpperCase().contains(searchText.toUpperCase())
                        && jsonobject.getString(TAG_STREAM_TYPE).equals(TAG_LIVE)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLivesSearch", e);
        }
        return arrayList;
    }

    public void addToLiveData(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_LIVE, json);
    }

    public List<ItemCat> getCategoryLive() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_LIVE_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);
                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);
                ItemCat objItem = new ItemCat(id, name, TAG_EMPTY);
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getCategoryLive", e);
        }
        return arrayList;
    }

    public void addToCatLiveList(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_LIVE_CAT, json);
    }

    // Movies --------------------------------------------------------------------------------------
    public List<ItemMovies> getMovies(String catId) {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        if (catId == null || catId.isEmpty()){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String name = jsonobject.getString(TAG_NAME);
                String streamId = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                String rating = jsonobject.getString(TAG_RATING);
                ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);
                if (jsonobject.getString(TAG_CAT_ID).equals(catId)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getMovies", e);
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesRe() {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.isAdultsCount(jsonobject.getString(TAG_NAME))){

                    String name = jsonobject.getString(TAG_NAME);
                    String streamId = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getMoviesRe", e);
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesSearch(String searchText) {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        if (searchText == null || searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (jsonobject.getString(TAG_NAME).toLowerCase().contains(searchText.toLowerCase())
                        || jsonobject.getString(TAG_NAME).toUpperCase().contains(searchText.toUpperCase())){
                    String name = jsonobject.getString(TAG_NAME);
                    String streamId = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getMoviesSearch", e);
        }
        return arrayList;
    }

    public void addToMovieData(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_MOVIE, json);
    }

    public List<ItemCat> getCategoryMovie() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_MOVIE_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);

                ItemCat objItem = new ItemCat(id, name,"");
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getCategoryMovie", e);
        }
        return arrayList;
    }

    public void addToMovieCatData(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_MOVIE_CAT, json);
    }

    // Series --------------------------------------------------------------------------------------
    public List<ItemSeries> getSeries(String catId) {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        if (catId == null || catId.isEmpty()){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String name= jsonobject.getString(TAG_NAME);
                String seriesId= jsonobject.getString(TAG_SERIES_ID);
                String cover= jsonobject.getString(TAG_COVER);
                String rating= jsonobject.getString(TAG_RATING);
                ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);
                if (jsonobject.getString(TAG_CAT_ID).equals(catId)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getSeries", e);
        }
        return arrayList;
    }

    public List<ItemSeries> getSeriesRe() {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.isAdultsCount(jsonobject.getString(TAG_NAME))){
                    String name = jsonobject.getString(TAG_NAME);
                    String seriesId= jsonobject.getString(TAG_SERIES_ID);
                    String cover= jsonobject.getString(TAG_COVER);
                    String rating= jsonobject.getString(TAG_RATING);

                    ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getSeriesRe", e);
        }
        return arrayList;
    }

    public List<ItemSeries> getSeriesSearch(String searchText) {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        if (searchText == null || searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
            return arrayList;
        }
        try {
            String json = getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (jsonobject.getString(TAG_NAME).toLowerCase().contains(searchText.toLowerCase())
                        || jsonobject.getString(TAG_NAME).toUpperCase().contains(searchText.toUpperCase())){
                    String name = jsonobject.getString(TAG_NAME);
                    String seriesId = jsonobject.getString(TAG_SERIES_ID);
                    String cover = jsonobject.getString(TAG_COVER);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getSeriesSearch", e);
        }
        return arrayList;
    }

    public void addToSeriesData(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_SERIES, json);
    }

    public List<ItemCat> getCategorySeries() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_SERIES_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);

                ItemCat objItem = new ItemCat(id, name, TAG_EMPTY);
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getCategorySeries", e);
        }
        return arrayList;
    }

    public void addToSeriesCatData(String json) {
        if (json == null) {
            return;
        }
        putString(TAG_JSON_SERIES_CAT, json);
    }

    // Playlist ------------------------------------------------------------------------------------
    public void addToPlaylistData(List<ItemPlaylist> arrayListPlaylist) {
        if (arrayListPlaylist == null) {
            return;
        }
        Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd' 'HH:mm:ss").create();
        String jsonString = gson.toJson(arrayListPlaylist);
        putString(TAG_JSON_PLAYLIST, jsonString);
    }

    public List<ItemCat> getCategoryPlaylist(int pageType) {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String group = objectCategory.getString(TAG_GROUP);
                String url = objectCategory.getString(TAG_URL);
                if (pageType == 4){
                    if (!url.contains(".mp4") || !url.contains(".mkv") || !url.contains(".avi")
                            || !url.contains(".mov") || !url.contains(".flv") || url.contains(".ts")
                            || url.contains("/ts") || url.contains(".m3u8") || url.contains("/m3u8")){
                        ItemCat objItem = new ItemCat(TAG_EMPTY ,group, TAG_EMPTY);
                        arrayList.add(objItem);
                    }
                } else {
                    if (url.contains(".mp4") || url.contains(".mkv") || url.contains(".avi")
                            || url.contains(".mov") || url.contains(".flv")){
                        ItemCat objItem = new ItemCat(TAG_EMPTY ,group, TAG_EMPTY);
                        arrayList.add(objItem);
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getCategoryPlaylist", e);
        }
        return arrayList;
    }

    public List<ItemChannel> getLivePlaylist() {
        ArrayList<ItemChannel> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String url = jsonobject.getString(TAG_URL);
                if (!url.contains(".mp4") || !url.contains(".mkv") || !url.contains(".avi")
                        || !url.contains(".webm") || !url.contains(".mov") || !url.contains(".flv")
                        || url.contains(".ts") || url.contains("/ts") || url.contains(".m3u8") || url.contains("/m3u8")){

                    String name = jsonobject.optString(TAG_PLAYLIST_NAME, "Default Name");
                    String logo = jsonobject.optString(TAG_LOGO,"null");
                    String group = jsonobject.optString(TAG_GROUP,"Default group");

                    ItemChannel objItem = new ItemChannel(name, url, logo, group);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getLivePlaylist", e);
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesPlaylist() {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            String json = getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String url = jsonobject.getString(TAG_URL);
                if (url.contains(".mp4") || url.contains(".mkv") || url.contains(".avi")
                        || url.contains(".webm") || url.contains(".mov") || url.contains(".flv")){

                    String name = jsonobject.optString(TAG_PLAYLIST_NAME, "Default Name");
                    String logo = jsonobject.optString(TAG_LOGO,"null");
                    String group = jsonobject.optString(TAG_GROUP,"Default group");

                    ItemMovies objItem = new ItemMovies(name, url, logo, TAG_EMPTY, group);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error parsing JSON in getMoviesPlaylist", e);
        }
        return arrayList;
    }

    // Example method to put a value ---------------------------------------------------------------
    public void putString(String key, String value) {
        ed.putString(key, value).apply();
    }

    public void putBoolean(String key, Boolean value) {
        ed.putBoolean(key, value).apply();
    }

    public void putInt(String key, int value) {
        ed.putInt(key, value).apply();
    }

    // Example method to get a value ---------------------------------------------------------------
    public String getString(String key, String defaultValue) {
        try {
            return sp.getString(key, defaultValue);
        } catch (Exception e) {
            if (key.equals(TAG_UPDATE_DATE)){
                return TAG_EMPTY;
            } else {
                return null;
            }
        }
    }

    public Boolean getBoolean(String key, Boolean defaultValue) {
        try {
            return sp.getBoolean(key, defaultValue);
        } catch (Exception e) {
            return false;
        }
    }

    public int getInt(String key, int defaultValue) {
        try {
            return sp.getInt(key, defaultValue);
        } catch (Exception e) {
            return 0;
        }
    }

    // Remove --------------------------------------------------------------------------------------
    public void removeAllPlaylist() {
        try {
            ed.putString(TAG_JSON_PLAYLIST, null);
            ed.apply();
        } catch (Exception e) {
            Log.e(TAG, "Error clearing removeAllPlaylist", e);
        }
    }
    
    public void removeAllData() {
        try {
            ed.putInt(TAG_SIZE_LIVE, 0);
            ed.putString(TAG_JSON_LIVE, null);
            ed.putString(TAG_JSON_LIVE_CAT, null);

            ed.putInt(TAG_SIZE_MOVIE, 0);
            ed.putString(TAG_JSON_MOVIE, null);
            ed.putString(TAG_JSON_MOVIE_CAT, null);

            ed.putInt(TAG_SIZE_SERIES, 0);
            ed.putString(TAG_JSON_SERIES, null);
            ed.putString(TAG_JSON_SERIES_CAT, null);

            ed.apply();
        } catch (Exception e) {
            Log.e(TAG, "Error clearing removeAllData", e);
        }
    }

    public void removeAllSeries() {
        try {
            ed.putInt(TAG_SIZE_SERIES, 0);
            ed.putString(TAG_JSON_SERIES, null);
            ed.putString(TAG_JSON_SERIES_CAT, null);
            ed.apply();
        } catch (Exception e) {
            Log.e(TAG, "Error clearing removeAllSeries", e);
        }
    }

    public void removeAllMovies() {
        try {
            ed.putInt(TAG_SIZE_MOVIE, 0);
            ed.putString(TAG_JSON_MOVIE, null);
            ed.putString(TAG_JSON_MOVIE_CAT, null);
            ed.apply();
        } catch (Exception e) {
            Log.e(TAG, "Error clearing removeAllMovies", e);
        }
    }

    public void removeAllLive() {
        try {
            ed.putInt(TAG_SIZE_LIVE, 0);
            ed.putString(TAG_JSON_LIVE, null);
            ed.putString(TAG_JSON_LIVE_CAT, null);
            ed.apply();
        } catch (Exception e) {
            Log.e(TAG, "Error clearing removeAllLive", e);
        }
    }

    // ---------------------------------------------------------------------------------------------
    public Boolean getIsLiveOrder() {
        return getBoolean(TAG_ORDER_LIVE, false);
    }
    public void setIsLiveOrder(Boolean flag) {
        putBoolean(TAG_ORDER_LIVE, flag);
    }

    public Boolean getIsMovieOrder() {
        return getBoolean(TAG_ORDER_MOVIE, false);
    }
    public void setIsMovieOrder(Boolean flag) {
        putBoolean(TAG_ORDER_MOVIE, flag);
    }

    public Boolean getIsSeriesOrder() {
        return getBoolean(TAG_ORDER_SERIES, false);
    }
    public void setIsSeriesOrder(Boolean flag) {
        putBoolean(TAG_ORDER_SERIES, flag);
    }

    public boolean getIsCategoriesOrder() {
        return getBoolean(TAG_ORDER_CAT, false);
    }
    public void setIsCategoriesOrder(Boolean flag){
        putBoolean(TAG_ORDER_CAT, flag);
    }

    //Size------------------------------------------------------------------------------------------
    public int getLiveSize() {
        return getInt(TAG_SIZE_LIVE, 0);
    }
    public void setLiveSize(int size) {
        putInt(TAG_SIZE_LIVE, size);
    }

    public int getMoviesSize() {
        return getInt(TAG_SIZE_MOVIE, 0);
    }
    public void setMovieSize(int size) {
        putInt(TAG_SIZE_MOVIE, size);
    }

    public int getSeriesSize() {
        return getInt(TAG_SIZE_SERIES, 0);
    }
    public void setSeriesSize(int size) {
        putInt(TAG_SIZE_SERIES, size);
    }

    //----------------------------------------------------------------------------------------------
    @SuppressLint("SimpleDateFormat")
    public void setUpdateDate(){
        Calendar calendar = Calendar.getInstance();
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String currentDateTime = simpleDateFormat.format(calendar.getTime());
        putString(TAG_UPDATE_DATE, currentDateTime);
    }

    public String getUpdateDate() {
        return getString(TAG_UPDATE_DATE, TAG_EMPTY);
    }
}