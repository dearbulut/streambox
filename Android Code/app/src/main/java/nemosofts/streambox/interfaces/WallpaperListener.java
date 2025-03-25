package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemWallpaper;

public interface WallpaperListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemWallpaper> arrayList);
}