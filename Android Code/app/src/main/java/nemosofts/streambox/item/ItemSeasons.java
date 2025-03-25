package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemSeasons implements Serializable {

	private final String name;
	private final String seasonNumber;

	public ItemSeasons(String name, String seasonNumber) {
		this.name = name;
		this.seasonNumber = seasonNumber;
	}

	public String getName() {
		return name;
	}

	public String getSeasonNumber() {
		return seasonNumber;
	}
}
