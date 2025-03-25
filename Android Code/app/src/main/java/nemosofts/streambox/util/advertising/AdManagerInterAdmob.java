package nemosofts.streambox.util.advertising;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.NonNull;

import com.google.ads.mediation.admob.AdMobAdapter;
import com.google.android.gms.ads.AdRequest;
import com.google.android.gms.ads.interstitial.InterstitialAd;
import com.google.android.gms.ads.interstitial.InterstitialAdLoadCallback;

import nemosofts.streambox.callback.Callback;


public class AdManagerInterAdmob {

    private static  InterstitialAd interAd;
    private final Context ctx;

    public AdManagerInterAdmob(Context ctx) {
        this.ctx = ctx;
    }

    public void createAd() {
        AdRequest adRequest;
        Bundle extras = new Bundle();
        extras.putString("npa", "1");
        adRequest = new AdRequest.Builder()
                .addNetworkExtrasBundle(AdMobAdapter.class, extras)
                .build();
        InterstitialAd.load(ctx, Callback.getAdmobInterstitialAdID(), adRequest, new InterstitialAdLoadCallback() {
            @Override
            public void onAdLoaded(@NonNull InterstitialAd interstitialAd) {
                AdManagerInterAdmob.setAd(interstitialAd);


            }
        });
    }

    public InterstitialAd getAd() {
        return interAd;
    }

    public static void setAd(InterstitialAd interstitialAd) {
        interAd = interstitialAd;
    }
}