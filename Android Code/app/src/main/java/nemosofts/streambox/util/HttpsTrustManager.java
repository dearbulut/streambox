package nemosofts.streambox.util;

import android.annotation.SuppressLint;
import android.util.Log;

import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

@SuppressLint("CustomX509TrustManager")
public class HttpsTrustManager implements X509TrustManager {

    private static TrustManager[] trustManagers;
    private static final X509Certificate[] _AcceptedIssuers = new X509Certificate[]{};

    @Override
    public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        // Implement proper client certificate verification or leave to default if not needed
        throw new CertificateException("Client certificates are not trusted.");
    }

    @Override
    public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        // Implement proper server certificate verification or leave to default if not needed
        throw new CertificateException("Server certificates are not trusted.");
    }

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return _AcceptedIssuers;
    }

    public static void allowAllSSL() {
        try {
            SSLContext context = SSLContext.getInstance("TLS");
            if (trustManagers == null) {
                // Use the default trust manager, or customize as needed
                TrustManagerFactory factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                factory.init((KeyStore) null);
                trustManagers = factory.getTrustManagers();
            }
            context.init(null, trustManagers, new SecureRandom());
            HttpsURLConnection.setDefaultSSLSocketFactory(context.getSocketFactory());

            // Enforce default hostname verification
            HttpsURLConnection.setDefaultHostnameVerifier(HttpsURLConnection.getDefaultHostnameVerifier());
        } catch (NoSuchAlgorithmException | KeyManagementException | KeyStoreException e) {
           Log.e("HttpsTrustManager", "Error enabling SSL", e);
        }
    }
}