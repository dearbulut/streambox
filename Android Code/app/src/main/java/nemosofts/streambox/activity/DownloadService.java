package nemosofts.streambox.activity;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.widget.RemoteViews;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.nemosofts.coreprogress.ProgressHelper;
import androidx.nemosofts.coreprogress.ProgressUIListener;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.encrypter.Encrypt;
import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;

public class DownloadService extends Service {

    private static final String TAG = "DownloadService";
    NotificationCompat.Builder myNotify;
    RemoteViews rv;
    OkHttpClient client;
    public static final String ACTION_STOP = "com.mydownload.action.STOP";
    public static final String ACTION_START = "com.mydownload.action.START";
    public static final String ACTION_ADD = "com.mydownload.action.ADD";
    private static final String CANCEL_TAG = "c_tag";
    NotificationManager mNotificationManager;
    private static DownloadService downloadService;

    Encrypt enc;
    Boolean isDownloaded = false;
    Thread thread;
    Call call;
    private int count = 0;
    private static final List<String> arrayListName = new ArrayList<>();
    private static final List<String> arrayListContainer = new ArrayList<>();
    private static final List<String> arrayListFilePath = new ArrayList<>();
    private static final List<String> arrayListURL = new ArrayList<>();
    private static final List<ItemVideoDownload> arrayListVideo = new ArrayList<>();
    static final int MY_NOTIFICATION_ID = 1002;

    private final Handler mHandler = new Handler(Looper.getMainLooper(), msg -> {
        switch (msg.what) {
            case 1 :
                int progress = Integer.parseInt(msg.obj.toString());
                arrayListVideo.get(0).setProgress(progress);
                rv.setProgressBar(R.id.progress, 100, progress, false);
                mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                break;
            case 0 :
                rv.setTextViewText(R.id.nf_title, arrayListVideo.get(0).getName());
                rv.setTextViewText(R.id.nf_percentage, count - (arrayListURL.size() - 1) + "/" + count + " " + getString(R.string.downloading));
                mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                break;
            case 2 :
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    // Restore the interrupted status
                    Thread.currentThread().interrupt();
                    // Optionally, log the exception or handle it as needed
                    Log.e("Thread", "Thread was interrupted", e);
                }
                rv.setProgressBar(R.id.progress, 100, 100, false);
                rv.setTextViewText(R.id.nf_percentage, count + "/" + count + " " + getString(R.string.downloaded));
                mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                count = 0;
                break;
            default:
                break;
        }
        return false;
    });

    public static DownloadService getInstance() {
        if (downloadService == null) {
            downloadService = new DownloadService();
        }
        return downloadService;
    }

    public static Boolean isDownloading() {
        return !arrayListFilePath.isEmpty();
    }

    public static List<ItemVideoDownload> getArrayListVideo() {
        return arrayListVideo;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();

        enc = Encrypt.getInstance();
        enc.init(this);

        mNotificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        String downloadNotification = "download_ch_1";
        myNotify = new NotificationCompat.Builder(this, downloadNotification);
        myNotify.setChannelId(downloadNotification);
        myNotify.setSmallIcon(R.drawable.ic_file_download_not);
        myNotify.setTicker(getResources().getString(R.string.downloading));
        myNotify.setWhen(System.currentTimeMillis());
        myNotify.setOnlyAlertOnce(true);

        rv = new RemoteViews(getPackageName(), R.layout.row_custom_notification);
        rv.setTextViewText(R.id.nf_title, getString(R.string.app_name));
        rv.setProgressBar(R.id.progress, 100, 0, false);
        rv.setTextViewText(R.id.nf_percentage, getResources().getString(R.string.downloading) + " " + "(0%)");

        Intent closeIntent = new Intent(this, DownloadService.class);
        closeIntent.setAction(ACTION_STOP);
        PendingIntent pcloseIntent = PendingIntent.getService(this, 0,
                closeIntent, PendingIntent.FLAG_IMMUTABLE);
        rv.setOnClickPendingIntent(R.id.iv_stop_download, pcloseIntent);

        myNotify.setCustomContentView(rv);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Online Channel download";// The user-visible name of the channel.
            NotificationChannel mChannel = new NotificationChannel(downloadNotification, name, NotificationManager.IMPORTANCE_LOW);
            mNotificationManager.createNotificationChannel(mChannel);
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(MY_NOTIFICATION_ID, myNotify.build(), ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(MY_NOTIFICATION_ID, myNotify.build());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        try {
            stopForeground(true);
            stop(null);
        } catch (Exception e) {
            Log.e(TAG, "Error stopping service", e); // Log the error instead of printing the stack trace
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        super.onStartCommand(intent, flags, startId);
        if (intent != null && intent.getAction() != null) {
            return switch (intent.getAction()) {
                case ACTION_START -> {
                    handleStartAction(intent);
                    yield START_NOT_STICKY;
                }
                case ACTION_STOP -> {
                    stop(intent);
                    yield START_STICKY;
                }
                case ACTION_ADD -> {
                    handleAddAction(intent);
                    yield START_REDELIVER_INTENT;
                }
                default -> START_STICKY;  // Default behavior for unrecognized actions
            };
        }

        return START_STICKY;  // Default if the intent or action is null
    }


    private void handleStartAction(@NonNull Intent intent) {
        arrayListURL.add(intent.getStringExtra("downloadUrl"));
        arrayListFilePath.add(intent.getStringExtra("file_path"));
        arrayListName.add(intent.getStringExtra("file_name"));
        arrayListContainer.add(intent.getStringExtra("file_container"));
        arrayListVideo.add((ItemVideoDownload) intent.getSerializableExtra("item"));
        count++;
        init();
    }

    private void handleAddAction(@NonNull Intent intent) {
        ItemVideoDownload itemVideoDownload = (ItemVideoDownload) intent.getSerializableExtra("item");
        if (itemVideoDownload != null && !isVideoAlreadyAdded(itemVideoDownload)) {
            count++;
            arrayListURL.add(intent.getStringExtra("downloadUrl"));
            arrayListFilePath.add(intent.getStringExtra("file_path"));
            arrayListName.add(intent.getStringExtra("file_name"));
            arrayListContainer.add(intent.getStringExtra("file_container"));
            arrayListVideo.add(itemVideoDownload);

            Message msg = mHandler.obtainMessage();
            msg.what = 0;
            mHandler.sendMessage(msg);
        }
    }

    private boolean isVideoAlreadyAdded(ItemVideoDownload itemVideoDownload) {
        for (ItemVideoDownload video : arrayListVideo) {
            if (video.getStreamID().equals(itemVideoDownload.getStreamID())) {
                return true;
            }
        }
        return false;
    }

    private void stop(Intent intent) {
        try {
            count = 0;
            if (client != null) {
                for (Call call1 : client.dispatcher().runningCalls()) {
                    if (Objects.equals(call1.request().tag(), CANCEL_TAG))
                        call1.cancel();
                }
            }
            if (thread != null) {
                thread.interrupt();
                thread = null;
            }
            deleteFirstFile(arrayListFilePath, arrayListName, arrayListContainer);

            arrayListVideo.clear();
            arrayListName.clear();
            arrayListContainer.clear();
            arrayListURL.clear();
            arrayListFilePath.clear();
            stopForeground(true);
            if (intent != null) {
                stopService(intent);
            } else {
                stopSelf();
            }
        } catch (Exception e) {
            Log.e(TAG, "Error stopping service", e);
        }
    }

    public void deleteFirstFile(List<String> filePaths, List<String> names, List<String> containers) {
        if (filePaths == null || filePaths.isEmpty() || names.isEmpty() || containers.isEmpty()) {
            Log.e(TAG, "One or more lists are empty. Cannot delete file.");
            return;
        }

        try {
            String filePath = filePaths.get(0) + "/" + names.get(0).replace(containers.get(0), "");
            File file = new File(filePath);

            // Attempt to delete using the `File` API
            if (file.delete()) {
                Log.d(TAG, "File deleted successfully: " + filePath);
            } else {
                Log.e(TAG, "Failed to delete file: " + filePath);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to delete file");
        }
    }

    public void init() {
        thread = new Thread(() -> {
            isDownloaded = false;

            client = new OkHttpClient();
            Request.Builder builder = new Request.Builder()
                    .url(arrayListURL.get(0))
                    .addHeader("Accept-Encoding", "identity")
                    .get()
                    .tag(CANCEL_TAG);

            call = client.newCall(builder.build());
            call.enqueue(new Callback() {
                @Override
                public void onFailure(@NonNull Call call, @NonNull IOException e) {
                    Log.d(TAG, "Error in download" ,e);
                }

                @Override
                public void onResponse(@NonNull Call call, @NonNull Response response) {
                    handleResponse(response);
                }
            });
        });
        thread.start();
    }

    private void handleResponse(Response response) {
        ResponseBody responseBody = ProgressHelper.withProgress(response.body(), new ProgressUIListener() {

            //if you don't need this method, don't override this methd. It isn't an abstract method, just an empty method.
            @Override
            public void onUIProgressStart(long totalBytes) {
                super.onUIProgressStart(totalBytes);
                Message msg = mHandler.obtainMessage();
                msg.what = 0;
                mHandler.sendMessage(msg);
            }

            @Override
            public void onUIProgressChanged(long numBytes, long totalBytes, float percent, float speed) {
                if (Boolean.FALSE.equals(isDownloaded)) {
                    Message msg = mHandler.obtainMessage();
                    msg.what = 1;
                    msg.obj = (int) (100 * percent) + "";
                    mHandler.sendMessage(msg);
                }
            }
        });

        try {
            BufferedSource source = responseBody.source();
            enc.encrypt(arrayListFilePath.get(0) + "/" + arrayListName.get(0), source, arrayListVideo.get(0));
        } catch (Exception e) {
            Log.d(TAG, "Error encrypt",e);
        }

        if (!arrayListURL.isEmpty()) {
            arrayListVideo.remove(0);
            arrayListName.remove(0);
            arrayListContainer.remove(0);
            arrayListFilePath.remove(0);
            arrayListURL.remove(0);
            if (!call.isCanceled() && !arrayListURL.isEmpty()) {
                init();
            } else {
                Message msg = mHandler.obtainMessage();
                msg.what = 2;
                msg.obj = 0 + "";
                mHandler.sendMessage(msg);
                isDownloaded = true;
            }
        }
    }
}