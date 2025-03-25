package nemosofts.streambox.util.advertising;

import android.app.Activity;
import android.content.Context;
import android.util.Log;

import com.google.android.ump.ConsentInformation;
import com.google.android.ump.ConsentRequestParameters;
import com.google.android.ump.UserMessagingPlatform;

public class GDPRChecker {

    private static final String TAG = "GDPRChecker";
    private Activity activity;
    private ConsentInformation consentInformation;
    Context context;

    public GDPRChecker() {
        throw new IllegalStateException("Utility class");
    }

    public GDPRChecker(Activity activity) {
        this.activity = activity;
        this.consentInformation = UserMessagingPlatform.getConsentInformation(activity);
    }

    public GDPRChecker(Context context) {
        this.context = context;
    }

    public void check() {
        initGDPR();
    }

    public boolean canLoadAd() {
        int status = UserMessagingPlatform.getConsentInformation(context).getConsentStatus();
        return (status == ConsentInformation.ConsentStatus.OBTAINED
                || status == ConsentInformation.ConsentStatus.NOT_REQUIRED
                || status == ConsentInformation.ConsentStatus.UNKNOWN
        );
    }

    private void initGDPR() {
        ConsentRequestParameters parameters = new ConsentRequestParameters.Builder()
                .setTagForUnderAgeOfConsent(false)
                .build();

        consentInformation.requestConsentInfoUpdate(activity, parameters, () -> {
            if (consentInformation.isConsentFormAvailable()) {
                loadForm();
            }
        }, formError -> Log.e(TAG, "onFailedToUpdateConsentInfo: " + formError.getMessage()));
    }

    public void loadForm() {
        UserMessagingPlatform.loadConsentForm(activity, consentForm -> {
            if (consentInformation.getConsentStatus() == ConsentInformation.ConsentStatus.REQUIRED
                    || consentInformation.getConsentStatus() == ConsentInformation.ConsentStatus.UNKNOWN) {
                consentForm.show(activity, formError -> {
                    if (consentInformation.getConsentStatus() != ConsentInformation.ConsentStatus.OBTAINED) {
                        // App can start requesting ads.
                        loadForm();
                    }
                });
            }
        }, formError -> loadForm());
    }
}
