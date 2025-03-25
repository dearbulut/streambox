package nemosofts.streambox.util;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import androidx.annotation.NonNull;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public abstract class AsyncTaskExecutor<P, B, R> {

    private final ExecutorService executor;
    private Handler handler;

    protected AsyncTaskExecutor() {
        executor = new ThreadPoolExecutor(
                2, // corePoolSize
                100, // maximumPoolSize
                60L, TimeUnit.SECONDS, // keepAliveTime
                new LinkedBlockingQueue<>(50), // queue capacity
                Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.CallerRunsPolicy()
        );
    }

    public ExecutorService getExecutor() {
        return executor;
    }

    public Handler getHandler() {
        if (handler == null) {
            synchronized (AsyncTaskExecutor.class) {
                handler = new Handler(Looper.getMainLooper());
            }
        }
        return handler;
    }

    protected void onPreExecute() {
        // Override this method wherever you want to perform task before background execution get started
    }

    protected abstract R doInBackground(P params);

    protected abstract void onPostExecute(R result);

    protected void onProgressUpdate(@NonNull B value) {
        // Override this method wherever you want update a progress result
    }

    // used for push progress report to UI ---------------------------------------------------------
    public void publishProgress(@NonNull B value) {
        getHandler().post(() -> onProgressUpdate(value));
    }

    public void execute() {
        execute(null);
    }

    public void execute(P params) {
        getHandler().post(() -> {
            onPreExecute();
            executor.execute(() -> {
                try {
                    R result = doInBackground(params);
                    getHandler().post(() -> onPostExecute(result));
                } catch (Exception e) {
                    // Handle exceptions that might occur during background task
                    Log.e("AsyncTaskExecutor", "Error executing task", e);
                }
            });
        });
    }

    // Cancel the task -----------------------------------------------------------------------------
    public void shutDown() {
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }

    // Shutdown the executor if no more tasks are pending ------------------------------------------
    public void checkAndShutDown() {
        if (executor != null && executor.isTerminated()) {
            shutDown();
        }
    }

    // Check if the task is running ----------------------------------------------------------------
    public boolean isRunning() {
        return executor != null && !executor.isShutdown() && !executor.isTerminated();
    }

    // Check if the task is cancelled --------------------------------------------------------------
    public boolean isCancelled() {
        return executor == null || executor.isTerminated() || executor.isShutdown();
    }
}