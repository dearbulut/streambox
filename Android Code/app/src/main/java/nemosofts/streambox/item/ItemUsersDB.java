package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemUsersDB implements Serializable {

	private final String id;
	private final String anyName;
	private final String userName;
	private final String userPass;
	private final String userUrl;
	private final String userType;

	public ItemUsersDB(String id, String anyName, String userName, String userPass,
					   String userUrl, String userType) {
		this.id = id;
		this.anyName = anyName;
		this.userName = userName;
		this.userPass = userPass;
		this.userUrl = userUrl;
		this.userType = userType;
	}

	public String getId() {
		return id;
	}

	public String getAnyName() {
		return anyName;
	}

	public String getUseName() {
		return userName;
	}

	public String getUserPass() {
		return userPass;
	}

	public String getUserURL() {
		return userUrl;
	}

	public String getUserType() {
		return userType;
	}
}
