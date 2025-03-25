package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemCast implements Serializable {

	private final String castID;
	private final String name;
	private final String profile;

	public ItemCast(String id, String name, String profile) {
		this.castID = id;
		this.name = name;
		this.profile = profile;
	}

	public String getId() {
		return castID;
	}

	public String getName() {
		return name;
	}

	public String getProfile() {
		return profile;
	}
}
