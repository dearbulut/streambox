package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.LinearInterpolator;
import android.view.animation.RotateAnimation;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import com.example.internet_speed_testing.InternetSpeedBuilder;
import com.example.internet_speed_testing.ProgressionModel;

import java.text.DecimalFormat;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;

public class NetworkSpeedActivity extends AppCompatActivity {

    private int position = 0;
    private int lastPosition = 0;
    private TextView downloadSpeed;
    private TextView totalSpeed;
    private ImageView barImage;
    private LinearLayout btnSpeed;

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

        btnSpeed = findViewById(R.id.ll_btn_speed);

        barImage =  findViewById(R.id.barImageView);
        downloadSpeed = findViewById(R.id.download);
        totalSpeed = findViewById(R.id.total_speed);

        btnSpeed.setOnClickListener(v -> runSpeedTest());
        if (ApplicationUtil.isTvBox(this)){
            btnSpeed.requestFocus();
        }
    }

    @SuppressLint("SetTextI18n")
    private void runSpeedTest() {
        findViewById(R.id.tv_speed).setVisibility(View.GONE);
        findViewById(R.id.pb_speed).setVisibility(View.VISIBLE);
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isFinishing()){
                return;
            }
            if (!NetworkUtils.isConnected(this)){
                Toasty.makeText(NetworkSpeedActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
                return;
            }
            btnSpeed.setVisibility(View.GONE);
            downloadSpeed.setVisibility(View.VISIBLE);
            totalSpeed.setVisibility(View.VISIBLE);

            InternetSpeedBuilder builder = new InternetSpeedBuilder(NetworkSpeedActivity.this);
            builder.setOnEventInternetSpeedListener(new InternetSpeedBuilder.OnEventInternetSpeedListener() {
                @Override
                public void onDownloadProgress(int count, @NonNull final ProgressionModel progressModel) {
                    double downloadSpeedValue = progressModel.getDownloadSpeed().doubleValue() / 1_000_000; // Convert to Mbps
                    position = getPositionByRate((float) downloadSpeedValue);

                    runOnUiThread(() -> {
                        RotateAnimation rotateAnimation = new RotateAnimation(
                                lastPosition, position, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
                        rotateAnimation.setInterpolator(new LinearInterpolator());
                        rotateAnimation.setDuration(500);
                        barImage.startAnimation(rotateAnimation);
                        downloadSpeed.setText("Download Speed: " + formatFileSize(downloadSpeedValue));
                    });

                    lastPosition = position;
                }

                @Override
                public void onUploadProgress(int count, @NonNull final ProgressionModel progressModel) {
                    // Handle upload progress if needed
                }

                @Override
                public void onTotalProgress(int count, @NonNull final ProgressionModel progressModel) {
                    double downloadSpeedValue = progressModel.getDownloadSpeed().doubleValue();
                    double uploadSpeedValue = progressModel.getUploadSpeed().doubleValue();
                    double totalSpeedValue = (downloadSpeedValue + uploadSpeedValue) / 2;

                    float totalAssumedSpeed = (float) ((downloadSpeedValue + uploadSpeedValue) / 2 / 1_000_000); // Convert to Mbps
                    position = getPositionByRate(totalAssumedSpeed);

                    runOnUiThread(() -> {
                        barImage.setRotation(position);
                        totalSpeed.setText("Total Speed: " + formatFileSize(totalSpeedValue));
                    });
                    lastPosition = position;
                }
            });
            builder.start(BuildConfig.BASE_URL+"uploads/1Mo.dat", 1);
        }, 500);
    }

    @NonNull
    public static String formatFileSize(double size) {
        if (size <= 0) {
            return "0 B/s";
        }
        DecimalFormat dec = new DecimalFormat("0.00");
        if (size >= 1_000_000_000_000L) {
            return dec.format(size / 1_000_000_000_000L) + " TB/s";
        } else if (size >= 1_000_000_000L) {
            return dec.format(size / 1_000_000_000L) + " GB/s";
        } else if (size >= 1_000_000L) {
            return dec.format(size / 1_000_000L) + " MB/s";
        } else if (size >= 1_000L) {
            return dec.format(size / 1_000L) + " KB/s";
        } else {
            return dec.format(size) + " B/s";
        }
    }

    public int getPositionByRate(float rate) {
        if (rate <= 0) {
            return 0;
        }

        if (rate <= 1) {
            return (int) (rate * 30);
        } else if (rate <= 10) {
            return (int) (rate * 6) + 30;
        } else if (rate <= 30) {
            return (int) ((rate - 10) * 3) + 90;
        } else if (rate <= 50) {
            return (int) ((rate - 30) * 1.5) + 150;
        } else if (rate <= 100) {
            return (int) ((rate - 50) * 1.2) + 180;
        }
        return 0;
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_network_speed;
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