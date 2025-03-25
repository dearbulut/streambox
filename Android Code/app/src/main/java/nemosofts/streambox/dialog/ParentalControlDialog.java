package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Dialog;
import android.content.Context;
import android.view.View;
import android.view.Window;
import android.widget.EditText;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class ParentalControlDialog {

    private final SPHelper spHelper;
    private final Dialog dialog;
    private final Context context;

    public ParentalControlDialog(Context context) {
        this.context = context;
        spHelper = new SPHelper(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_parental_control);

        dialog.findViewById(R.id.iv_close_adult).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel_adult).setOnClickListener(view -> dismissDialog());

        EditText etPasswordOld = dialog.findViewById(R.id.et_password_1);
        EditText etPassword = dialog.findViewById(R.id.et_password_2);
        EditText etConfirmPassword = dialog.findViewById(R.id.et_password_3);

        if (spHelper.getAdultPassword().isEmpty()) {
            etPasswordOld.setVisibility(View.GONE);
        } else {
            etPasswordOld.setVisibility(View.VISIBLE);
        }

        dialog.findViewById(R.id.tv_submit_adult).setOnClickListener(view -> onSubmit(etPasswordOld, etPassword, etConfirmPassword));

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();

        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    private void onSubmit(EditText etPasswordOld, EditText etPassword, EditText etConfirmPassword) {
        boolean hasOldPassword = etPasswordOld.getVisibility() == View.VISIBLE;

        if (hasOldPassword && !validateOldPassword(etPasswordOld)) return;
        if (!validateNewPasswords(etPassword, etConfirmPassword)) return;

        spHelper.setAdultPassword(etPassword.getText().toString());
        dismissDialog();
    }

    private boolean validateOldPassword(EditText etPasswordOld) {
        String oldPassword = etPasswordOld.getText().toString().trim();

        if (oldPassword.isEmpty()) return setErrorAndFocus(etPasswordOld, R.string.err_cannot_empty);
        if (oldPassword.endsWith(" ")) return setErrorAndFocus(etPasswordOld, R.string.error_pass_end_space);
        if (!spHelper.getAdultPassword().equals(oldPassword)) return setErrorAndFocus(etPasswordOld, R.string.error_old_pass_not_match);

        return true;
    }

    private boolean validateNewPasswords(EditText etPassword, EditText etConfirmPassword) {
        String password = etPassword.getText().toString().trim();
        String confirmPassword = etConfirmPassword.getText().toString().trim();

        if (password.isEmpty()) return setErrorAndFocus(etPassword, R.string.err_cannot_empty);
        if (password.endsWith(" ")) return setErrorAndFocus(etPassword, R.string.error_pass_end_space);
        if (confirmPassword.isEmpty()) return setErrorAndFocus(etConfirmPassword, R.string.err_cannot_empty);
        if (confirmPassword.endsWith(" ")) return setErrorAndFocus( etConfirmPassword, R.string.error_pass_end_space);
        if (!confirmPassword.equals(password)) return setErrorAndFocus(etConfirmPassword, R.string.error_pass_not_match);

        return true;
    }

    private boolean setErrorAndFocus(EditText editText, int errorResId) {
        editText.setError(context.getResources().getString(errorResId));
        editText.requestFocus();
        return false;
    }

    public void dismissDialog() {
        if (dialog != null && dialog.isShowing()) {
            dialog.dismiss();
        }
    }

    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }
}

