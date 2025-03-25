package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;


public class AboutUsActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private TextView author;
    private TextView email;
    private TextView website;
    private TextView contact;
    private TextView description;
    private TextView version;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));
        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        author = findViewById(R.id.tv_company);
        email = findViewById(R.id.tv_email);
        website = findViewById(R.id.tv_website);
        contact = findViewById(R.id.tv_contact);
        description = findViewById(R.id.tv_app_des);
        version = findViewById(R.id.tv_version);

        setAboutUs();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_about_us;
    }

    private void setAboutUs() {
        author.setText(!spHelper.getAppAuthor().trim().isEmpty() ? spHelper.getAppAuthor() : "");
        email.setText(!spHelper.getAppEmail().trim().isEmpty() ? spHelper.getAppEmail() : "");
        website.setText(!spHelper.getAppWebsite().trim().isEmpty() ? spHelper.getAppWebsite() : "");
        contact.setText(!spHelper.getAppContact().trim().isEmpty() ? spHelper.getAppContact() : "");
        description.setText(!spHelper.getAppDescription().trim().isEmpty() ? spHelper.getAppDescription() : "");
        version.setText(BuildConfig.VERSION_NAME);
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                finish();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }
}