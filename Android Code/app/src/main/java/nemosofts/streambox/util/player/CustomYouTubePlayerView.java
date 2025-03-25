package nemosofts.streambox.util.player;

import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.customui.utils.FadeViewHelper;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.PlayerConstants;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.utils.YouTubePlayerTracker;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;

import nemosofts.streambox.R;

public class CustomYouTubePlayerView extends AbstractYouTubePlayerListener {

    private final YouTubePlayerTracker playerTracker;
    private final YouTubePlayer youTubePlayer;
    private final YouTubePlayerView youTubePlayerView;

    public CustomYouTubePlayerView(View controlsUi,
                                   @NonNull YouTubePlayer youTubePlayer, YouTubePlayerView youTubePlayerView) {
        this.youTubePlayer = youTubePlayer;
        this.youTubePlayerView = youTubePlayerView;
        playerTracker = new YouTubePlayerTracker();
        youTubePlayer.addListener(playerTracker);
        initViews(controlsUi);
    }

    private void initViews(@NonNull View view) {
        View container = view.findViewById(R.id.container);
        RelativeLayout relativeLayout = view.findViewById(R.id.root);
        ImageView pausePlay = view.findViewById(R.id.pausePlay);
        pausePlay.setOnClickListener(view13 -> {
            if (playerTracker.getState() == PlayerConstants.PlayerState.PLAYING) {
                pausePlay.setImageResource(R.drawable.ic_play);
                youTubePlayer.pause();
            } else {
                pausePlay.setImageResource(R.drawable.ic_pause);
                youTubePlayer.play();
            }
        });

        youTubePlayerView.matchParent();

        FadeViewHelper fadeViewHelper = new FadeViewHelper(container);
        fadeViewHelper.setAnimationDuration(FadeViewHelper.DEFAULT_ANIMATION_DURATION);
        fadeViewHelper.setFadeOutDelay(FadeViewHelper.DEFAULT_FADE_OUT_DELAY);
        youTubePlayer.addListener(fadeViewHelper);

        relativeLayout.setOnClickListener(view12 -> fadeViewHelper.toggleVisibility());
        container.setOnClickListener(view1 -> fadeViewHelper.toggleVisibility());
    }
}
