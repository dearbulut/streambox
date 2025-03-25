package nemosofts.streambox.executor;

import android.content.Context;
import android.util.Log;

import org.json.JSONArray;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.interfaces.DataListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadData extends AsyncTaskExecutor<String, String, String> {

    private static final String TAG = "LoadData";
    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final DataListener listener;

    public LoadData(Context ctx, DataListener listener) {
        this.listener = listener;
        spHelper = new SPHelper(ctx);
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
            if (!isUpdateNeeded()){
                updateSeriesData();
                updateMoviesData();
                updateLiveStreams();
                return "1";
            } else {
                return "2";
            }
        } catch (Exception e) {
            return "0";
        }
    }

    private boolean isUpdateNeeded() {
        if (jsHelper.getUpdateDate().isEmpty()) {
            jsHelper.setUpdateDate();
            return true;
        }
        // Hours Check
        return Boolean.TRUE.equals(ApplicationUtil.calculateUpdateHours(jsHelper.getUpdateDate(), spHelper.getAutoUpdate()));
    }

    private void updateLiveStreams() {
        try {
            String currentLive = spHelper.getCurrent(Callback.TAG_TV);
            if (spHelper.getIsUpdateLive() && !currentLive.isEmpty()) {
                String jsonLive = ApplicationUtil.responsePost(spHelper.getAPI(),
                        ApplicationUtil.getAPIRequest("get_live_streams",
                                spHelper.getUserName(), spHelper.getPassword())
                );
                if (!jsonLive.isEmpty()) {
                    JSONArray arrayLive = new JSONArray(jsonLive);
                    if (arrayLive.length() != 0 && arrayLive.length() != jsHelper.getLiveSize()) {
                        jsHelper.setLiveSize(arrayLive.length());
                        jsHelper.addToLiveData(jsonLive);
                        Callback.setSuccessLive("1");
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "get_live_streams", e);
        }
    }

    private void updateMoviesData() {
        try {
            String currentMovies = spHelper.getCurrent(Callback.TAG_MOVIE);
            if (spHelper.getIsUpdateMovies() && !currentMovies.isEmpty()) {
                String jsonMovies = ApplicationUtil.responsePost(spHelper.getAPI(),
                        ApplicationUtil.getAPIRequest("get_vod_streams",
                                spHelper.getUserName(), spHelper.getPassword())
                );
                if (!jsonMovies.isEmpty()) {
                    JSONArray arrayMovies = new JSONArray(jsonMovies);
                    if (arrayMovies.length() != 0 && arrayMovies.length() != jsHelper.getMoviesSize()) {
                        jsHelper.setMovieSize(arrayMovies.length());
                        jsHelper.addToMovieData(jsonMovies);
                        Callback.setSuccessMovies("1");
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "get_vod_streams", e);
        }
    }

    private void updateSeriesData() {
        try {
            String currentSeries = spHelper.getCurrent(Callback.TAG_SERIES);
            if (spHelper.getIsUpdateSeries() && !currentSeries.isEmpty()) {
                String jsonSeries = ApplicationUtil.responsePost(spHelper.getAPI(),
                        ApplicationUtil.getAPIRequest("get_series",
                                spHelper.getUserName(), spHelper.getPassword())
                );
                if (!jsonSeries.isEmpty()) {
                    JSONArray arraySeries = new JSONArray(jsonSeries);
                    if (arraySeries.length() != 0 && arraySeries.length() != jsHelper.getSeriesSize()) {
                        jsHelper.setSeriesSize(arraySeries.length());
                        jsHelper.addToSeriesData(jsonSeries);
                        Callback.setSuccessSeries("1");
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "get_series", e);
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s);
        this.shutDown();
    }
}