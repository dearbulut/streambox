package nemosofts.streambox.interfaces;

public interface StatusListener {
    void onStart();
    void onEnd(String success, String registerSuccess, String message);
}