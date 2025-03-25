package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemSingleURL implements Serializable {

	private final String id;
	private final String anyName;
	private final String singleUrl;

	public ItemSingleURL(String id, String anyName, String singleUrl) {
		this.id = id;
		this.anyName = anyName;
		this.singleUrl = singleUrl;
	}

	public String getId() {
		return id;
	}

	public String getAnyName() {
		return anyName;
	}

	public String getSingleURL() {
		return singleUrl;
	}
}
