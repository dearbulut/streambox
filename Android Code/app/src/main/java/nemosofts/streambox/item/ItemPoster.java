package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemPoster implements Serializable {

	private final String posterData;

	public ItemPoster(String poster) {
		this.posterData = poster;
	}

	public String getPoster() {
		return posterData;
	}
}
