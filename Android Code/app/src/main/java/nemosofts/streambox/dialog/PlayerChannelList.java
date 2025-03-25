package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;

import android.app.Activity;
import android.app.Dialog;
import android.view.Window;

import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterChannelPlayer;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.util.IfSupported;

public class PlayerChannelList {

    private Dialog dialog;
    private final Activity ctx;
    private final PlayerLiveListListener listener;

    public PlayerChannelList(Activity ctx, PlayerLiveListListener listener) {
        this.ctx = ctx;
        this.listener = listener;
    }

    public void showDialog() {
        dialog = new Dialog(ctx);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_player_list);
        dialog.findViewById(R.id.iv_close_vw).setOnClickListener(v -> dismissDialog());
        RecyclerView rv = dialog.findViewById(R.id.rv_dialog);
        LinearLayoutManager trending = new LinearLayoutManager(ctx, LinearLayoutManager.VERTICAL, false);
        rv.setLayoutManager(trending);
        rv.setItemAnimator(new DefaultItemAnimator());
        AdapterChannelPlayer adapter = new AdapterChannelPlayer(ctx, Callback.getArrayListLive(), (item, position) -> listener.onSubmit(position));
        rv.setAdapter(adapter);
        rv.scrollToPosition(Callback.getPlayPosLive());
        adapter.select(Callback.getPlayPosLive());
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, MATCH_PARENT);
    }

    public void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

    public interface PlayerLiveListListener {
        void onSubmit(int position);
    }

}
