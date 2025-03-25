package nemosofts.streambox.util;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Environment;
import android.provider.BaseColumns;
import android.provider.DocumentsContract;
import android.provider.MediaStore;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class MediaPath {

    private MediaPath() {
        throw new IllegalStateException("Utility class");
    }

    public static String getPathAudio(Context ctx, Uri uri) {
        if (uri == null) {
            return null;
        }

        // Handle the document Uri (API level 19 and above)
        if (DocumentsContract.isDocumentUri(ctx, uri)) {
            // External storage document
            if (isExternalStorageDocument(uri)) {
                String docId = DocumentsContract.getDocumentId(uri);
                String[] split = docId.split(":");
                String type = split[0];

                if ("primary".equalsIgnoreCase(type)) {
                    return Environment.getExternalStorageDirectory() + "/" + split[1];
                }

                // Handle non-primary volumes
                // Extend this logic if needed.
            }
            // Downloads folder
            else if (isDownloadsDocument(uri)) {
                String id = DocumentsContract.getDocumentId(uri);
                Uri contentUri = ContentUris.withAppendedId(
                        Uri.parse("content://downloads/public_downloads"), Long.parseLong(id));

                return getDataColumn(ctx, contentUri, null, null);
            }
            // Media provider (audio)
            else if (isMediaDocument(uri)) {
                String docId = DocumentsContract.getDocumentId(uri);
                String[] split = docId.split(":");
                String type = split[0];
                Uri contentUri = null;
                if ("audio".equals(type)) {
                    contentUri = MediaStore.Audio.Media.EXTERNAL_CONTENT_URI;
                }

                String selection = BaseColumns._ID + "=?";
                String[] selectionArgs = new String[]{split[1]};

                return getDataColumn(ctx, contentUri, selection, selectionArgs);
            }
        }
        // Handle content URIs (audio from gallery, file manager)
        else if ("content".equalsIgnoreCase(uri.getScheme())) {
            return getDataColumn(ctx, uri, null, null);
        }
        // Handle file URIs
        else if ("file".equalsIgnoreCase(uri.getScheme())) {
            return uri.getPath();
        }

        return null;
    }

    @Nullable
    private static String getDataColumn(@NonNull Context context, Uri uri, String selection, String[] selectionArgs) {
        Cursor cursor = null;

        final String column = MediaStore.MediaColumns.DATA;
        final String[] projection = {column};

        try {
            cursor = context.getContentResolver().query(uri, projection, selection, selectionArgs, null);
            if (cursor != null && cursor.moveToFirst()) {
                int index = cursor.getColumnIndexOrThrow(column);
                return cursor.getString(index);
            }
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
        return null;
    }

    private static boolean isExternalStorageDocument(@NonNull Uri uri) {
        return "com.android.externalstorage.documents".equals(uri.getAuthority());
    }

    private static boolean isDownloadsDocument(@NonNull Uri uri) {
        return "com.android.providers.downloads.documents".equals(uri.getAuthority());
    }

    private static boolean isMediaDocument(@NonNull Uri uri) {
        return "com.android.providers.media.documents".equals(uri.getAuthority());
    }
}
