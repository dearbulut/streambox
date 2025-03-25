package nemosofts.streambox.util.encrypter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Log;

import androidx.annotation.NonNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.HttpsURLConnection;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.AsyncTaskExecutor;
import nemosofts.streambox.util.helper.DBHelper;
import okio.BufferedSource;

public class Encrypt {

    private DBHelper dbHelper;
    private Context context;
    private Boolean isEncrypt = false;
    @SuppressLint("StaticFieldLeak")
    private static Encrypt instance = null;

    private Encrypt() {
        isEncrypt = false;
    }

    public static Encrypt getInstance() {
        if (instance == null) {
            instance = new Encrypt();
        }
        return instance;
    }

    public void init(Context context) {
        this.context = context;
        isEncrypt = true;
        dbHelper = new DBHelper(context);
    }

    public String getEditedFileName(@NonNull File file, String token) {
        String path = file.getAbsolutePath();
        String name;
        int i = path.lastIndexOf('.');
        if (i > 0) {
            name = path.substring(0, i) + token;
        } else {
            name = path + token;
        }
        return name;
    }

    public void encrypt(String fileName, BufferedSource bufferedSource, final ItemVideoDownload itemDownload) {
        try {
            final long a = System.currentTimeMillis();

            File fileEncrypt = new File(getEditedFileName(new File(fileName.concat(ApplicationUtil.containerExtension(itemDownload.getContainerExtension()))), ""));
            final String fileSavedName = fileEncrypt.getName().replace(ApplicationUtil.containerExtension(itemDownload.getContainerExtension()), "");
            itemDownload.setTempName(fileSavedName);

            Cipher encryptionCipher = Cipher.getInstance("AES/CTR/NoPadding");

            byte[] secretKey = BuildConfig.ENC_KEY.getBytes();
            byte[] initialIv = BuildConfig.IV.getBytes();

            SecretKeySpec secretKeySpec = new SecretKeySpec(secretKey, "AES");
            encryptionCipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, new IvParameterSpec(initialIv));

            try (InputStream fis = bufferedSource.inputStream();
                 OutputStream fileStream = new BufferedOutputStream(new FileOutputStream(fileEncrypt));
                 OutputStream outputStream = new CipherOutputStream(fileStream, encryptionCipher)) {

                byte[] buffer = new byte[2048];
                int len;
                while ((len = fis.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, len);
                }
            }

            new AsyncTaskExecutor<String, String, String>() {
                String imageName;

                @Override
                protected String doInBackground(String strings) {
                    imageName = getBitmapFromURL(itemDownload.getStreamIcon(), fileSavedName);
                    if (!imageName.equals("0")) {
                        return "1";
                    } else {
                        return "0";
                    }
                }

                @Override
                protected void onPostExecute(String s) {
                    if (!s.equals("1")) {
                        imageName = "null";
                    }
                    itemDownload.setStreamIcon(imageName);
                    itemDownload.setTempName(fileSavedName);
                    dbHelper.addToDownloads(DBHelper.TABLE_DOWNLOAD_MOVIES, itemDownload);
                }
            }.execute();
        } catch (Exception e) {
            Log.e("Encrypt", "Error encrypting file",e);
        }
    }

    public String getBitmapFromURL(String src, String name) {
        if (src != null && src.isEmpty()) {
            src = "null";
        }

        try (InputStream input = getInputStream(src)) {
            Bitmap myBitmap = BitmapFactory.decodeStream(input);

            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            myBitmap.compress(Bitmap.CompressFormat.JPEG, 80, bytes);

            // Define the file path and create necessary directories
            File root = new File(context.getExternalFilesDir("").getAbsolutePath() + File.separator + "/tempim/");
            if (!root.exists()) {
                root.mkdirs();
            }

            // Create the file
            File f = new File(root, name + ".jpg");
            if (!f.exists()) {
                f.createNewFile();
            }

            // Write the byte array to the file
            try (FileOutputStream fo = new FileOutputStream(f)) {
                fo.write(bytes.toByteArray());
            }

            return f.getAbsolutePath();
        } catch (IOException e) {
            return "0"; // Handle error by returning "0" as before
        }
    }

    private static InputStream getInputStream(String src) throws IOException {
        URL url = new URL(src);
        InputStream input;
        if(src.contains("https://")) {
            HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.connect();
            input = connection.getInputStream();
        } else {
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.connect();
            input = connection.getInputStream();
        }
        return input;
    }
}
