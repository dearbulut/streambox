package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemEpgFull implements Serializable {

	private final String id;
	private final String start;
	private final String end;
	private final String title;
	private final String startTimestamp;
	private final String stopTimestamp;
	private final int hasArchive;

    public ItemEpgFull(String id, String start, String end, String title,
					   String startTimestamp, String stopTimestamp, int hasArchive) {
        this.id = id;
        this.start = start;
        this.end = end;
        this.title = title;
		this.startTimestamp = startTimestamp;
		this.stopTimestamp = stopTimestamp;
		this.hasArchive = hasArchive;
    }

	public String getId() {
		return id;
	}

	public String getStart() {
		return start;
	}

	public String getEnd() {
		return end;
	}

	public String getTitle() {
		return title;
	}

	public String getStartTimestamp() {
		return startTimestamp;
	}

	public String getStopTimestamp() {
		return stopTimestamp;
	}

	public int getHasArchive() {
		return hasArchive;
	}
}
