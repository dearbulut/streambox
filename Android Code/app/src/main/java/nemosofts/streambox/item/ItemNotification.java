package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemNotification implements Serializable {

    private final String id;
    private final String title;
    private final String msg;
    private final String description;
    private final String date;

    public ItemNotification(String id, String title, String msg, String description, String date) {
        this.id = id;
        this.title = title;
        this.msg = msg;
        this.description = description;
        this.date = date;
    }

    public String getId() {
        return id;
    }

    public String getTitle() {
        return title;
    }

    public String getMsg() {
        return msg;
    }

    public String getDescription() {
        return description;
    }

    public String getDate() {
        return date;
    }
}
