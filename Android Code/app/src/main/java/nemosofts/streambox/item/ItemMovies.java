package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemMovies implements Serializable {

	private final String name;
	private final String streamID;
	private final String streamIcon;
	private final String rating;
	private final String catName;

	public ItemMovies(String name, String streamID, String streamIcon, String rating, String catName) {
		this.name = name;
		this.streamID = streamID;
		this.streamIcon = streamIcon;
		this.rating = rating;
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

	public String getRating() {
		return rating;
	}

	public String getCatName() {
		return catName;
	}
}
