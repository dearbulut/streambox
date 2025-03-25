package nemosofts.streambox.util.encrypter;


import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.TransferListener;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

@UnstableApi
public class EncryptedFileDataSourceFactory implements DataSource.Factory {

    private final Cipher mCipher;
    private final SecretKeySpec mSecretKeySpec;
    private final IvParameterSpec mIvParameterSpec;
    private final TransferListener mTransferListener;

    public EncryptedFileDataSourceFactory(Cipher cipher, SecretKeySpec secretKeySpec,
                                          IvParameterSpec ivParameterSpec, TransferListener listener) {
        mCipher = cipher;
        mSecretKeySpec = secretKeySpec;
        mIvParameterSpec = ivParameterSpec;
        mTransferListener = listener;
    }

    @NonNull
    @Override
    public EncryptedFileDataSource createDataSource() {
        return new EncryptedFileDataSource(mCipher, mSecretKeySpec, mIvParameterSpec, mTransferListener);
    }
}