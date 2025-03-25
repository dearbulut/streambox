package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemDns implements Serializable {

	private final String title;
	private final String baseURL;

	public ItemDns(String title, String base) {
		this.title = title;
		this.baseURL = base;
	}

	public String getTitle() {
		return title;
	}

	public String getBase() {
		return baseURL;
	}
}
