package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.view.KeyEvent;
import android.view.View;
import android.view.Window;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RadioGroup;
import android.widget.TextView;

import androidx.annotation.OptIn;
import androidx.core.app.ActivityCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.WebActivity;
import nemosofts.streambox.adapter.AdapterRadioButton;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemRadioButton;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class DialogUtil {

    private static Dialog dialog;
    
    private static Boolean flag = false;
    private static Boolean getIsFlag() {
        return flag;
    }
    private static void setIsFlag(Boolean isFlag) {
        flag = isFlag;
    }

    private DialogUtil() {
        throw new IllegalStateException("Utility class");
    }

    // Dialog --------------------------------------------------------------------------------------
    public static void exitDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }

        if (dialog != null){
            dialog = null;
        }
        boolean isTvBox = ApplicationUtil.isTvBox(activity);
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);
            dialog.findViewById(R.id.tv_do_cancel).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_do_yes).setOnClickListener(view -> {
                dismissDialog();
                activity.finish();
            });
            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {
            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.exit);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_exit);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            dialog.findViewById(R.id.tv_dialog_no).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_dialog_yes).setOnClickListener(view -> {
                dismissDialog();
                activity.finish();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void maintenanceDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.maintenance);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.we_are_performing_scheduled);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.temporarily_down_for_maintenance);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void upgradeDialog(Activity activity, CancelListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.upgrade);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.its_time_to_upgrade);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.upgrade);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            listener.onCancel();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            listener.onCancel();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setText(R.string.do_it_now);
        yes.setOnClickListener(view -> {
            dismissDialog();
            if (!Callback.getAppRedirectUrl().isEmpty()){
                activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(Callback.getAppRedirectUrl())));
            }
        });

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void dModeDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.developer_mode);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.turn_off_developer_mode);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.developer_mode);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.try_again_later);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void vpnDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.sniffing_detected);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.turn_off_all_sniffers_tools);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.sniffing_detected);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void logoutDialog(Activity activity, LogoutListener logoutListener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        boolean isTvBox = ApplicationUtil.isTvBox(activity);
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_logout);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                logoutListener.onLogout();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.logout);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_logout);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                logoutListener.onLogout();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void deleteDialog(Context context, DeleteListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        boolean isTvBox = ApplicationUtil.isTvBox(context);
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_trash);

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_delete);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.delete);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDelete();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.cancel);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();

        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_trash);

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.delete);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_delete);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.cancel);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.delete);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDelete();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void downloadDataDialog(Activity activity, String type, DownloadListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        boolean isTvBox = ApplicationUtil.isTvBox(activity);
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_reset);

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_reload_data);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload(type);
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_reset);

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.reload_data);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_reload_data);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload(type);
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void dialogPlayerInfo(Activity activity, ExoPlayer exoPlayer, boolean isLive) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (exoPlayer != null){
            if (dialog != null){
                dialog = null;
            }
            dialog = new Dialog(activity);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_media_info);
            dialog.findViewById(R.id.iv_close_vw).setOnClickListener(v -> dismissDialog());
            dialog.findViewById(R.id.iv_back_player_info).setOnClickListener(v -> dismissDialog());

            String infoVideo = ApplicationUtil.getInfoVideo(exoPlayer, isLive);
            TextView mediaVideo = dialog.findViewById(R.id.tv_info_video);
            mediaVideo.setText(infoVideo);

            String infoAudio = ApplicationUtil.getInfoAudio(exoPlayer);
            TextView mediaAudio = dialog.findViewById(R.id.tv_info_audio);
            mediaAudio.setText(infoAudio);

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            if (window != null){
                IfSupported.hideStatusBarDialog(window);
                window.setLayout(MATCH_PARENT, WRAP_CONTENT);
            }
        }
    }

    public static void screenDialog(Activity activity, ScreenDialogListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_screen);
        dialog.setCancelable(false);
        dialog.findViewById(R.id.iv_screen_one).setOnClickListener(v -> {
            listener.onSubmit(1);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_two).setOnClickListener(v -> {
            listener.onSubmit(2);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_three).setOnClickListener(v -> {
            listener.onSubmit(3);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_four).setOnClickListener(v -> {
            listener.onSubmit(4);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_five).setOnClickListener(v -> {
            listener.onSubmit(5);
            dismissDialog();
        });
        dialog.setOnKeyListener((dialog, keyCode, event) -> {
            if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
                listener.onCancel();
                dialog.dismiss();
                return true;
            }
            return false;
        });
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void liveDownloadDialog(Context context, LiveDownloadListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        boolean isTvBox = ApplicationUtil.isTvBox(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_file_download);

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.want_to_download);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                listener.onDownload();
                dismissDialog();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();

        } else {
            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_file_download);

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.live_not_downloaded);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.want_to_download);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void radioBtnDialog(Context context, List<ItemRadioButton> arrayList, int position,
                                      String pageTitle, RadioBtnListener radioBtnListener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_radio_btn);

        TextView title = dialog.findViewById(R.id.tv_page_title_dil);
        title.setText(pageTitle);

        RecyclerView rv = dialog.findViewById(R.id.rv_radio_btn);
        GridLayoutManager grid = new GridLayoutManager(context, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        AdapterRadioButton adapter = new AdapterRadioButton(context, arrayList, position);
        rv.setAdapter(adapter);

        dialog.findViewById(R.id.tv_submit).setOnClickListener(view -> {
            radioBtnListener.onSetLimit(adapter.getData());
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_close).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel).setOnClickListener(view -> dismissDialog());


        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void popupAdsDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        if (!Callback.getAdsImage().isEmpty() && !Callback.getAdsRedirectURL().isEmpty()){
            dialog = new Dialog(activity);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_popup_ads);

            ImageHelperView loadAds = dialog.findViewById(R.id.iv_ads);
            ProgressBar pb  = dialog.findViewById(R.id.pb_ads);
            dialog.findViewById(R.id.vw_ads).setOnClickListener(v -> {
                if (Callback.getAdsRedirectType().equals("external")){
                    Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(Callback.getAdsRedirectURL()));
                    activity.startActivity(browserIntent);
                } else {
                    Intent intent = new Intent(activity, WebActivity.class);
                    intent.putExtra("web_url", Callback.getAdsRedirectURL());
                    intent.putExtra("page_title", Callback.getAdsTitle());
                    ActivityCompat.startActivity(activity, intent, null);
                }
            });
            Picasso.get()
                    .load(Callback.getAdsImage().replace(" ", "%20"))
                    .placeholder(R.color.white)
                    .error(R.color.white)
                    .into(loadAds, new com.squareup.picasso.Callback() {
                        @Override
                        public void onSuccess() {
                            pb.setVisibility(View.GONE);
                        }

                        @Override
                        public void onError(Exception e) {
                            pb.setVisibility(View.GONE);
                        }
                    });

            dialog.findViewById(R.id.iv_back_ads).setOnClickListener(view -> {
                dismissDialog();
                Callback.setAdsImage("");
                Callback.setAdsRedirectURL("");
            });

            boolean isTvBox = ApplicationUtil.isTvBox(activity);
            if (isTvBox){
                dialog.findViewById(R.id.iv_back_ads).requestFocus();
            }

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            if (window != null){
                IfSupported.hideStatusBarDialog(window);
                window.setLayout(MATCH_PARENT, WRAP_CONTENT);
            }
        }
    }

    public static void childCountDialog(Context context, int pos,  ChildCountListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        SPHelper spHelper = new SPHelper(context);
        if (spHelper.getAdultPassword().isEmpty()){
            listener.onUnLock(pos);
        } else {
            dialog = new Dialog(context);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_child_count);
            dialog.findViewById(R.id.iv_close_adult).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_cancel_adult).setOnClickListener(view -> dismissDialog());
            EditText password = dialog.findViewById(R.id.et_password_adult);
            dialog.findViewById(R.id.tv_unlock_adult).setOnClickListener(view -> {
                if(password.getText().toString().trim().isEmpty()) {
                    password.setError(context.getString(R.string.err_cannot_empty));
                    password.requestFocus();
                } else {
                    if (spHelper.getAdultPassword().equals(password.getText().toString())){
                        listener.onUnLock(pos);
                        dismissDialog();
                    } else {
                        password.setError(context.getString(R.string.err_password));
                        password.requestFocus();
                    }
                }
            });
            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void filterDialog(Context context, int pageType, FilterDialogListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        JSHelper jsHelper = new JSHelper(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);

        dialog.setContentView(R.layout.dialog_filter);
        dialog.findViewById(R.id.iv_close_btn).setOnClickListener(view -> dismissDialog());

        setIsFlag(false);
        if (Boolean.TRUE.equals(pageType == 1)){
            flag = jsHelper.getIsLiveOrder();
        } else if (Boolean.TRUE.equals(pageType == 2)){
            flag = jsHelper.getIsMovieOrder();
        }  else if (Boolean.TRUE.equals(pageType == 3)){
            flag = jsHelper.getIsSeriesOrder();
        }

        RadioGroup rg = dialog.findViewById(R.id.rg);
        if (Boolean.TRUE.equals(flag)){
            rg.check(R.id.rd_1);
        } else {
            rg.check(R.id.rd_2);
        }

        dialog.findViewById(R.id.rd_1).setOnClickListener(view -> setIsFlag(true));
        dialog.findViewById(R.id.rd_2).setOnClickListener(view -> setIsFlag(false));

        dialog.findViewById(R.id.btn_cancel_filter).setOnClickListener(view -> {
            if (Boolean.TRUE.equals(pageType == 1)){
                jsHelper.setIsLiveOrder(false);
            } else if (Boolean.TRUE.equals(pageType == 2)){
                jsHelper.setIsMovieOrder(false);
            }  else if (Boolean.TRUE.equals(pageType == 3)){
                jsHelper.setIsSeriesOrder(false);
            }
            listener.onSubmit();
            dismissDialog();
        });
        dialog.findViewById(R.id.btn_submit_filter).setOnClickListener(view -> {
            if (Boolean.TRUE.equals(pageType == 1)){
                jsHelper.setIsLiveOrder(getIsFlag());
            } else if (Boolean.TRUE.equals(pageType == 2)){
                jsHelper.setIsMovieOrder(getIsFlag());
            }  else if (Boolean.TRUE.equals(pageType == 3)){
                jsHelper.setIsSeriesOrder(getIsFlag());
            }
            listener.onSubmit();
            dismissDialog();
        });

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    public static void wallpaperDialog(Activity activity, String filePath) {
        if (activity == null || activity.isFinishing() || filePath.isEmpty()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_wallpaper);

        ImageHelperView loadAds = dialog.findViewById(R.id.iv_wallpaper);
        ProgressBar pb  = dialog.findViewById(R.id.pb);
        Picasso.get()
                .load("https://image.tmdb.org/t/p/original"+filePath)
                .into(loadAds, new com.squareup.picasso.Callback() {
                    @Override
                    public void onSuccess() {
                        pb.setVisibility(View.GONE);
                    }

                    @Override
                    public void onError(Exception e) {
                        pb.setVisibility(View.GONE);
                        dismissDialog();
                    }
                });

        dialog.findViewById(R.id.iv_back_wallpaper).setOnClickListener(v -> dismissDialog());
        dialog.findViewById(R.id.rl_root_dialog).setOnClickListener(v -> dismissDialog());
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, MATCH_PARENT);
        }
    }

    // Dismiss -------------------------------------------------------------------------------------
    public static void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    // isShowing -----------------------------------------------------------------------------------
    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

    // Listener ------------------------------------------------------------------------------------
    public interface CancelListener {
        void onCancel();
    }

    public interface LogoutListener {
        void onLogout();
    }

    public interface DeleteListener {
        void onDelete();
    }

    public interface DownloadListener {
        void onDownload(String type);
    }

    public interface LiveDownloadListener {
        void onDownload();
    }

    public interface ScreenDialogListener {
        void onSubmit(int screen);
        void onCancel();
    }

    public interface RadioBtnListener {
        void onSetLimit(int update);
    }

    public interface ChildCountListener {
        void onUnLock(int pos);
    }

    public interface FilterDialogListener {
        void onSubmit();
    }
}
