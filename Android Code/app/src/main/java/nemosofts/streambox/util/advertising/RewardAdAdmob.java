package nemosofts.streambox.util.advertising;

import android.content.Context;

import androidx.annotation.NonNull;

import com.google.android.gms.ads.AdRequest;
import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.rewarded.RewardedAd;
import com.google.android.gms.ads.rewarded.RewardedAdLoadCallback;

import nemosofts.streambox.callback.Callback;


public class RewardAdAdmob {

    static RewardedAd rewardedAd;
    private final Context ctx;

    public RewardAdAdmob(Context ctx) {
        this.ctx = ctx;
    }

    public void createAd() {
        AdRequest adRequest = new AdRequest.Builder().build();
        RewardedAd.load(ctx, Callback.getAdmobRewardAdID(), adRequest, new RewardedAdLoadCallback() {
            @Override
            public void onAdFailedToLoad(@NonNull LoadAdError loadAdError) {
                RewardAdAdmob.setAd(null);
            }

            @Override
            public void onAdLoaded(@NonNull RewardedAd ad) {
                RewardAdAdmob.setAd(ad);
            }
        });
    }

    public RewardedAd getAd() {
        return rewardedAd;
    }

    public static void setAd(RewardedAd rewardAd) {
        rewardedAd = rewardAd;
    }
}