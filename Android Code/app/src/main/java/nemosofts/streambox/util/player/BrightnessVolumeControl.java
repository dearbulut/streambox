package nemosofts.streambox.util.player;

import android.app.Activity;
import android.media.AudioManager;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;

@UnstableApi
public class BrightnessVolumeControl {

    private final Activity activity;
    private int currentBrightnessLevel = -1;

    public BrightnessVolumeControl(Activity activity) {
        this.activity = activity;
    }

    // Gets the current screen brightness
    public float getScreenBrightness() {
        return activity.getWindow().getAttributes().screenBrightness;
    }

    // Sets the screen brightness
    public void setScreenBrightness(final float brightness) {
        WindowManager.LayoutParams lp = activity.getWindow().getAttributes();
        lp.screenBrightness = brightness;
        activity.getWindow().setAttributes(lp);
    }

    // Converts a brightness level to a screen brightness value
    public float levelToBrightness(final float level) {
        final double d = 0.064 + 0.936 / 30 * level;
        return (float) (d * d);
    }

    // Changes the screen brightness based on user input
    public void changeBrightness(final CustomPlayerView playerView, final boolean increase, final boolean canSetAuto) {
        int newBrightnessLevel = increase ? currentBrightnessLevel + 1 : currentBrightnessLevel - 1;

        if (canSetAuto && newBrightnessLevel < 0)
            currentBrightnessLevel = -1;
        else if (newBrightnessLevel >= 0 && newBrightnessLevel <= 30)
            currentBrightnessLevel = newBrightnessLevel;

        if (currentBrightnessLevel == -1 && canSetAuto)
            setScreenBrightness(WindowManager.LayoutParams.BRIGHTNESS_OVERRIDE_NONE);
        else if (currentBrightnessLevel != -1)
            setScreenBrightness(levelToBrightness(currentBrightnessLevel));

        playerView.setHighlight(false);
        if (currentBrightnessLevel == -1 && canSetAuto) {
            playerView.setIconBrightnessAuto();
            playerView.setCustomErrorMessage("");
        } else {
            playerView.setIconBrightness();
            playerView.setCustomErrorMessage(" " + currentBrightnessLevel);
        }
    }

    // Adjusts the volume level based on user input
    public void adjustVolume(AudioManager audioManager, final CustomPlayerView playerView, final boolean increase) {
        if (audioManager == null) return;

        int volumeMax = audioManager.getStreamMaxVolume(AudioManager.STREAM_MUSIC);
        int currentVolumeLevel = audioManager.getStreamVolume(AudioManager.STREAM_MUSIC);
        int newVolumeLevel = calculateNewVolume(currentVolumeLevel, volumeMax, increase);

        audioManager.setStreamVolume(AudioManager.STREAM_MUSIC, Math.max(newVolumeLevel, 0), 0);
        updatePlayerViewIcon(playerView, newVolumeLevel);
        playerView.setCustomErrorMessage(" " + Math.max(0, newVolumeLevel));
    }

    // Calculates the new volume level based on the increase or decrease input
    private int calculateNewVolume(int currentVolume, int volumeMax, boolean increase) {
        int adjustedVolume = increase ? currentVolume + 1 : currentVolume - 1;
        return Math.min(Math.max(adjustedVolume, 0), volumeMax);
    }

    // Updates the player view's icon based on the volume level
    private void updatePlayerViewIcon(@NonNull CustomPlayerView playerView, int volumeLevel) {
        playerView.setHighlight(false);

        if (volumeLevel <= 0) {
            playerView.setIconVolumeError();
        } else if (volumeLevel < 10) {
            playerView.setIconVolumeDown();
        } else {
            playerView.setIconVolumeUp();
        }
    }
}