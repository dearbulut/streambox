package nemosofts.streambox.activity;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.widget.EditText;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.nemosofts.AppCompatActivity;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import nemosofts.streambox.R;

public class OpenVPNActivity extends AppCompatActivity {

    private static final String TAG = "OpenVPNActivity";

    // File picker launcher
    private ActivityResultLauncher<Intent> pickOvpnFileLauncher;

    // Example EditText for entering URL (should be in your layout file)
    private EditText urlEditText;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Initialize the EditText for URL
        urlEditText = findViewById(R.id.et_url);

        // Initialize the file picker launcher
        pickOvpnFileLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (result.getResultCode() == RESULT_OK && result.getData() != null) {
                Uri fileUri = result.getData().getData();
                Log.d(TAG, "File selected: " + fileUri);

                try {
                    // Read the .ovpn file content
                    String ovpnConfig = readOvpnFile(fileUri);
                    Log.d(TAG, "OVPN Config: " + ovpnConfig);

                    // Start VPN
                    startVpn(ovpnConfig, "your_username", "your_password");

                } catch (Exception e) {
                    Log.e(TAG, "Failed to read OVPN file", e);
                }
            }
        });

        // Example: Trigger file picker
        pickOvpnFile();

        // Example: Trigger URL fetching
        fetchOvpnFromUrl("https://example.com/your_vpn_file.ovpn");
    }

    // Launch file picker to choose .ovpn file
    private void pickOvpnFile() {
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*"); // Refine with "*.ovpn" if needed
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        pickOvpnFileLauncher.launch(Intent.createChooser(intent, "Select VPN Configuration File"));
    }

    // Start the VPN
    private void startVpn(String ovpnConfig, String username, String password) {
        try {
//            OpenVpnApi.startVpn(this, ovpnConfig, username, password);
            Log.d(TAG, "VPN started successfully.");
        } catch (Exception e) {
            Log.e(TAG, "Failed to start VPN", e);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_open_vpn;
    }

    // Fetch .ovpn file from a URL
    private void fetchOvpnFromUrl(String ovpnUrl) {
        new Thread(() -> {
            try {
                URL url = new URL(ovpnUrl);
                HttpURLConnection connection = (HttpURLConnection) url.openConnection();
                connection.setRequestMethod("GET");
                connection.connect();

                if (connection.getResponseCode() == HttpURLConnection.HTTP_OK) {
                    InputStream inputStream = connection.getInputStream();
                    String ovpnConfig = readInputStream(inputStream);
                    runOnUiThread(() -> {
                        Log.d(TAG, "Downloaded OVPN Config: " + ovpnConfig);
                        // Start VPN
                        startVpn(ovpnConfig, "your_username", "your_password");
                    });
                } else {
                    Log.e(TAG, "Failed to download file. Response code: " + connection.getResponseCode());
                }
                connection.disconnect();
            } catch (Exception e) {
                Log.e(TAG, "Error fetching OVPN file", e);
            }
        }).start();
    }

    // Read the content of the selected file
    private String readOvpnFile(Uri fileUri) throws Exception {
        StringBuilder config = new StringBuilder();
        InputStream inputStream = getContentResolver().openInputStream(fileUri);
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
        String line;
        while ((line = reader.readLine()) != null) {
            config.append(line).append("\n");
        }
        reader.close();
        return config.toString();
    }

    // Read InputStream into String
    private String readInputStream(InputStream inputStream) throws Exception {
        StringBuilder config = new StringBuilder();
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
        String line;
        while ((line = reader.readLine()) != null) {
            config.append(line).append("\n");
        }
        reader.close();
        return config.toString();
    }
}