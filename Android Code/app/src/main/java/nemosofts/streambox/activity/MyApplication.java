package nemosofts.streambox.activity;

import android.content.Context;
import android.os.StrictMode;
import android.util.Log;

import androidx.multidex.MultiDex;
import androidx.nemosofts.Application;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.onesignal.OneSignal;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;

public class MyApplication extends Application {

    DBHelper dbHelper;

    @Override
    public void onCreate() {
        super.onCreate();

        // Analytics Initialization
        FirebaseAnalytics.getInstance(getApplicationContext());

        StrictMode.VmPolicy.Builder builder = new StrictMode.VmPolicy.Builder();
        StrictMode.setVmPolicy(builder.build());

        try {
            dbHelper = new DBHelper(getApplicationContext());
            dbHelper.onCreate(dbHelper.getWritableDatabase());
        } catch (Exception e) {
            Log.e("MyApplication", "Error DB", e);
        }

        // OneSignal Initialization
        OneSignal.initWithContext(this, getString(R.string.onesignal_app_id));

        new Helper(getApplicationContext()).initializeAds();
    }

    @Override
    public String setProductID() {
        return "52621164";
    }

    @Override
    public String setApplicationID() {
        return BuildConfig.APPLICATION_ID;
    }

    @Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        MultiDex.install(this);
    }
}
