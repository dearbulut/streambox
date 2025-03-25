package nemosofts.streambox.item;

import androidx.annotation.DrawableRes;

import java.io.Serializable;

public class ItemSetting implements Serializable {

	private final String title;
	@DrawableRes
	public final int drawableResId;

	public ItemSetting(String title, @DrawableRes int drawableResId) {
		this.title = title;
		this.drawableResId = drawableResId;
	}

	public String getName() {
		return title;
	}

	public int getDrawableData() {
		return drawableResId;
	}
}
