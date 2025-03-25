package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemLoginUser implements Serializable {

	private final String username;
	private final String password;
	private final String message;
	private final String status;
	private final String expDate;
	private final String activeCons;
	private final String maxConnections;

    public ItemLoginUser(String username, String password, String message, String status,
						 String expDate, String activeCons, String maxConnections) {
        this.username = username;
        this.password = password;
        this.message = message;
        this.status = status;
        this.expDate = expDate;
        this.activeCons = activeCons;
        this.maxConnections = maxConnections;
    }

	public String getUsername() {
		return username;
	}

	public String getPassword() {
		return password;
	}

	public String getMessage() {
		return message;
	}

	public String getStatus() {
		return status;
	}

	public String getExpDate() {
		return expDate;
	}

	public String getActiveCons() {
		return activeCons;
	}

	public String getMaxConnections() {
		return maxConnections;
	}
}
