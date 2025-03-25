package nemosofts.streambox.util.player;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
import android.media.AudioManager;
import android.util.AttributeSet;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.view.GestureDetectorCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.ui.PlayerView;

import nemosofts.streambox.R;

@UnstableApi
public class CustomPlayerView extends PlayerView implements GestureDetector.OnGestureListener{

    private float gestureScrollY = 0f;
    private float gestureScrollX = 0f;
    private boolean handleTouch;

    public static final int MESSAGE_TIMEOUT_TOUCH = 400;
    public static final int MESSAGE_TIMEOUT_KEY = 800;

    private final AudioManager mAudioManager;
    private BrightnessVolumeControl brightnessControl;

    private final TextView exoErrorMessage;

    private final GestureDetectorCompat mDetector;

    private static int volumeUpsInRow = 0;
    public static int getVolumeUpsInRow() {
        return volumeUpsInRow;
    }
    public static void setVolumeUpsInRow(int volume) {
        volumeUpsInRow = volume;
    }
    public static void incrementVolumeUpsInRow() {
        volumeUpsInRow++;
    }

    public final Runnable textClearRunnable = () -> {
        setCustomErrorMessage(null);
        clearIcon();
    };

    public CustomPlayerView(Context context) {
        this(context, null);
    }

    public CustomPlayerView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CustomPlayerView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mDetector = new GestureDetectorCompat(context,this);
        mAudioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
        exoErrorMessage = findViewById(androidx.media3.ui.R.id.exo_error_message);
    }

    public void setHighlight(boolean active) {
        if (active)
            exoErrorMessage.getBackground().setTint(Color.RED);
        else
            exoErrorMessage.getBackground().setTintList(null);
    }

    public void setIconBrightness() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_brightness_medium, 0, 0, 0);
    }

    public void setIconBrightnessAuto() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_brightness_auto, 0, 0, 0);
    }

    public void setIconVolumeUp() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_up, 0, 0, 0);
    }

    public void setIconVolumeDown() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_down, 0, 0, 0);
    }

    public void setIconVolumeError() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_off, 0, 0, 0);
    }

    public void setIconVolume(boolean volumeActive) {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(volumeActive ? R.drawable.ic_volume_up : R.drawable.ic_volume_off, 0, 0, 0);
    }

    public void setBrightnessControl(BrightnessVolumeControl brightnessControl) {
        this.brightnessControl = brightnessControl;
    }

    public void clearIcon() {
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(0, 0, 0, 0);
        setHighlight(false);
    }

    @Override
    public boolean onDown(@NonNull MotionEvent motionEvent) {
        gestureScrollY = 0;
        gestureScrollX = 0;
        return false;
    }

    @Override
    public void onShowPress(@NonNull MotionEvent e) {
        // Add comment explaining why this method is empty or implement functionality if necessary
    }

    @Override
    public boolean onSingleTapUp(@NonNull MotionEvent e) {
        // Implement logic if necessary or return true/false based on your requirements
        return false;
    }


    @SuppressLint("UnsafeOptInUsageError")
    @Override
    public boolean onScroll(MotionEvent motionEvent, @NonNull MotionEvent motionEvent1, float distanceX, float distanceY) {
        if (gestureScrollY == 0 || gestureScrollX == 0) {
            gestureScrollY = 0.0001f;
            gestureScrollX = 0.0001f;
            return false;
        }

        // LEFT = Brightness  |  RIGHT = Volume
        gestureScrollY += distanceY;
        if (Math.abs(gestureScrollY) > dpToPx(16)) {
            if (motionEvent.getX() < ((float) getWidth() / 2)) {
                brightnessControl.changeBrightness(this, gestureScrollY > 0, false);
            } else {
                brightnessControl.adjustVolume(mAudioManager, this, gestureScrollY > 0);
            }
            gestureScrollY = 0.0001f;
        }
        return true;
    }

    @Override
    public void onLongPress(@NonNull MotionEvent e) {
        // Add comment explaining why this method is empty or implement functionality if necessary
    }

    @Override
    public boolean onFling(MotionEvent e1, @NonNull MotionEvent e2, float velocityX, float velocityY) {
        // Implement logic if necessary or return true/false based on your requirements
        return false;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event != null){
            if (event.getActionMasked() == MotionEvent.ACTION_DOWN){
                removeCallbacks(textClearRunnable);
                handleTouch = true;
            } else if (event.getActionMasked() == MotionEvent.ACTION_UP || event.getActionMasked() == MotionEvent.ACTION_CANCEL && (handleTouch)){
                postDelayed(textClearRunnable, MESSAGE_TIMEOUT_TOUCH);
                setControllerAutoShow(true);
            }
            if (handleTouch){
                mDetector.onTouchEvent(event);
            }
        }
        return super.onTouchEvent(event);
    }

    public static int dpToPx(int dp) {
        return (int) (dp * Resources.getSystem().getDisplayMetrics().density);
    }
}
