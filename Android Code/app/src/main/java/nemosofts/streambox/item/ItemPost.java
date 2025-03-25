package nemosofts.streambox.item;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ItemPost implements Serializable{

	private final String id;
	private final String type;
	private List<ItemChannel> arrayListLive = new ArrayList<>();
	private List<ItemEpg> arrayListEpg = new ArrayList<>();

	public ItemPost(String id, String type) {
		this.id = id;
		this.type = type;
	}

	public String getId() {
		return id;
	}

	public String getType() {
		return type;
	}

	public List<ItemChannel> getArrayListLive() {
		return arrayListLive;
	}
	public void setArrayListLive(List<ItemChannel> arrayListLive) {
		this.arrayListLive = arrayListLive;
	}

	public List<ItemEpg> getArrayListEpg() {
		return arrayListEpg;
	}
	public void setArrayListEpg(List<ItemEpg> arrayListEpg) {
		this.arrayListEpg = arrayListEpg;
	}
}