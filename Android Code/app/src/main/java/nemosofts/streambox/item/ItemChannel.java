package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemChannel implements Serializable {

	private final String name;
	private final String streamID;
	private final String streamIcon;
	private final String catName;

	public ItemChannel(String name, String streamID, String streamIcon, String catName) {
		this.name = name;
		this.streamID = streamID;
		this.streamIcon = streamIcon;
		this.catName = catName;
	}

	public String getName() {
		return name;
	}

	public String getStreamID() {
		return streamID;
	}

	public String getStreamIcon() {
		return streamIcon;
	}

	public String getCatName() {
		return catName;
	}
}
