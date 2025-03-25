package nemosofts.streambox.interfaces;

import nemosofts.streambox.item.ItemLoginServer;
import nemosofts.streambox.item.ItemLoginUser;

public interface LoginListener {
    void onStart();
    void onEnd(String success, ItemLoginUser itemLoginUser,
               ItemLoginServer itemLoginServer, String allowedOutputFormats
    );
}