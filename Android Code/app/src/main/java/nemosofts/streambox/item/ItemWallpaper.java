package nemosofts.streambox.item;

public class ItemWallpaper {

	private final int height;
	private final int width;
	private final String filePath;

    public ItemWallpaper(int height, int width, String filePath) {
        this.height = height;
        this.width = width;
        this.filePath = filePath;
    }

	public int getHeight() {
		return height;
	}

	public int getWidth() {
		return width;
	}

	public String getFilePath() {
		return filePath;
	}
}
