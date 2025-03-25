package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemPlaylist implements Serializable {

	private final String playlistName;
	private final String logo;
	private final String group;
	private final String url;

	public ItemPlaylist(String name, String logo, String group, String url) {
		this.playlistName = name;
		this.logo = logo;
		this.group = group;
		this.url = url;
	}

	public String getName() {
		return playlistName;
	}

	public String getLogo() {
		return logo;
	}

	public String getGroup() {
		return group;
	}

	public String getUrl() {
		return url;
	}
}
