package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemEpg implements Serializable {

	private final String start;
	private final String end;
	private final String title;
	private final String startTimestamp;
	private final String stopTimestamp;

	public ItemEpg(String start,
				   String end, String title, String startTimestamp, String stopTimestamp) {
		this.start = start;
		this.end = end;
		this.title = title;
		this.startTimestamp = startTimestamp;
		this.stopTimestamp = stopTimestamp;
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
}
