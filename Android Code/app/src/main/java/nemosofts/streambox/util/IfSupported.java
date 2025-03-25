package nemosofts.streambox.util;

import android.app.Activity;
import android.os.Build;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowInsetsController;
import android.view.WindowManager;

import androidx.annotation.OptIn;
import androidx.core.content.ContextCompat;
import androidx.core.view.WindowCompat;
import androidx.media3.common.util.UnstableApi;

import nemosofts.streambox.R;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.CustomPlayerView;

public class IfSupported {

    private static final String TAG = "IfSupported";

    private IfSupported() {
        throw new IllegalStateException("Utility class");
    }

    public static void isRTL(Activity activity) {
        if (activity == null) {
            Log.e(TAG, "Activity context is null in isRTL");
            return;
        }
        try {
            if (Boolean.TRUE.equals(new SPHelper(activity).getIsRTL())) {
                Window window = activity.getWindow();
                WindowCompat.setDecorFitsSystemWindows(window, false);
                window.getDecorView().setLayoutDirection(View.LAYOUT_DIRECTION_RTL);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to apply RTL layout direction", e);
        }
    }

    public static void isScreenshot(Activity mContext) {
        if (mContext == null) {
            Log.e(TAG, "Activity context is null isScreenshot");
            return;
        }
        try {
            if (Boolean.TRUE.equals(new SPHelper(mContext).getIsScreenshot())) {
                Window window = mContext.getWindow();
                window.setFlags(WindowManager.LayoutParams.FLAG_SECURE, WindowManager.LayoutParams.FLAG_SECURE);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to isScreenshot", e);
        }
    }

    public static void keepScreenOn(Activity mContext) {
        if (mContext == null) {
            Log.e(TAG, "Activity context is null keepScreenOn");
            return;
        }
        try {
            Window window = mContext.getWindow();
            window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        } catch (Exception e) {
            Log.e(TAG, "Failed to keep screen on", e);
        }
    }

    public static void hideStatusBar(Activity mContext) {
        if (mContext == null) {
            Log.e(TAG, "Activity context is null hideStatusBar");
            return;
        }
        try {
            Window window = mContext.getWindow();
            hideStatusBarDialog(window);
        } catch (Exception e) {
            Log.e(TAG, "Failed to hide status bar", e);
        }
    }

    public static void hideBottomBar(Activity activity) {
        if (activity == null) {
            Log.e(TAG, "Activity context is null in hideBottomBar");
            return;
        }
        try {
            Window window = activity.getWindow();
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                WindowInsetsController controller = window.getInsetsController();
                if (controller != null) {
                    controller.hide(WindowInsets.Type.navigationBars());
                    controller.setSystemBarsBehavior(WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
                }
            } else {
                View decorView = window.getDecorView();
                decorView.setSystemUiVisibility(View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                        | View.SYSTEM_UI_FLAG_FULLSCREEN);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to hide bottom bar", e);
        }
    }

    public static void hideStatusBarDialog(Window window) {
        if (window == null) {
            Log.e(TAG, "Window is null");
            return;
        }
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                WindowCompat.setDecorFitsSystemWindows(window, false);
                WindowInsetsController controller = window.getDecorView().getWindowInsetsController();
                if (controller != null) {
                    controller.hide(WindowInsets.Type.statusBars());
                    controller.hide(WindowInsets.Type.navigationBars());
                    controller.setSystemBarsBehavior(WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
                }
            } else {
                View decorView = window.getDecorView();
                int uiOptions = View.SYSTEM_UI_FLAG_FULLSCREEN;
                decorView.setSystemUiVisibility(uiOptions);
            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to hide status bar dialog", e);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void toggleSystemUi(final Activity activity, final CustomPlayerView playerView, final boolean show) {
        if (Build.VERSION.SDK_INT >= 31) {
            Window window = activity.getWindow();
            if (window != null) {
                WindowInsetsController windowInsetsController = window.getInsetsController();
                if (windowInsetsController != null) {
                    if (show) {
                        windowInsetsController.show(WindowInsets.Type.systemBars());
                        windowInsetsController.show(WindowInsets.Type.navigationBars());
                    } else {
                        windowInsetsController.hide(WindowInsets.Type.systemBars());
                        windowInsetsController.hide(WindowInsets.Type.navigationBars());
                    }
                }
            }
        } else {
            if (show) {
                playerView.setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
            } else {
                playerView.setSystemUiVisibility(View.SYSTEM_UI_FLAG_LOW_PROFILE
                        | View.SYSTEM_UI_FLAG_FULLSCREEN
                        | View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION);
            }
        }
    }

    public static void statusBarBlackColor(Activity activity) {
        if (activity == null) {
            Log.e(TAG, "Activity context is null in statusBarBlackColor");
            return;
        }
        try {
            Window window = activity.getWindow();
            WindowCompat.setDecorFitsSystemWindows(window, true);
            window.setStatusBarColor(ContextCompat.getColor(activity, R.color.black));
        } catch (Exception e) {
            Log.e(TAG, "Failed to set status bar color to black", e);
        }
    }

    public static void setNavigationBarColor(Activity activity) {
        if (activity == null) {
            Log.e(TAG, "Activity context is null statusBarBlackColor");
            return;
        }

        try {
            Window window = activity.getWindow();
            window.setNavigationBarColor(ContextCompat.getColor(activity, R.color.black));
        } catch (Exception e) {
            Log.e(TAG, "Failed to set navigation bar color to black", e);
        }
    }
}