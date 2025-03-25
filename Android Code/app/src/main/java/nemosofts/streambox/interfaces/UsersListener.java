package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemUsers;

public interface UsersListener {
    void onStart();
    void onEnd(String success,
               String verifyStatus, String message, ArrayList<ItemUsers> arrayListUsers
    );
}