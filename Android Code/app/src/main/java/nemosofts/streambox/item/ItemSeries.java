package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemSeries implements Serializable {

	private final String name;
	private final String seriesID;
	private final String cover;
	private final String rating;

	public ItemSeries(String name, String seriesID, String cover, String rating) {
		this.name = name;
		this.seriesID = seriesID;
		this.cover = cover;
		this.rating = rating;
	}

	public String getName() {
		return name;
	}

	public String getSeriesID() {
		return seriesID;
	}

	public String getCover() {
		return cover;
	}

	public String getRating() {
		return rating;
	}
}
