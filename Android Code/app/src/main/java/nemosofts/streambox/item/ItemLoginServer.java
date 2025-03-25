package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemLoginServer implements Serializable {

	private final Boolean xui;
	private final String url;
	private final String port;
	private final String httpsPort;
	private final String serverProtocol;

    public ItemLoginServer(Boolean xui, String url, String port, String httpsPort, String serverProtocol) {
        this.xui = xui;
        this.url = url;
        this.port = port;
        this.httpsPort = httpsPort;
        this.serverProtocol = serverProtocol;
    }

	public Boolean getXui() {
		return xui;
	}

	public String getUrl() {
		return url;
	}

	public String getPort() {
		return port;
	}

	public String getHttpsPort() {
		return httpsPort;
	}

	public String getServerProtocol() {
		return serverProtocol;
	}
}
