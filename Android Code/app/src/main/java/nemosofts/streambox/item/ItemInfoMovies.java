package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemInfoMovies implements Serializable {

	private final String tmdbID;
	private final String name;
	private final String movieImage;
	private final String releaseDate;
	private final String episodeRunTime;
	private final String youtubeTrailer;
	private final String director;
	private final String cast;
	private final String plot;
	private final String genre;
	private final String rating;

	public ItemInfoMovies(String tmdbID, String name, String movieImage, String releaseDate,
						  String episodeRunTime, String youtubeTrailer, String director, String cast,
						  String plot, String genre, String rating) {
		this.tmdbID = tmdbID;
		this.name = name;
		this.movieImage = movieImage;
		this.releaseDate = releaseDate;
		this.episodeRunTime = episodeRunTime;
		this.youtubeTrailer = youtubeTrailer;
		this.director = director;
		this.cast = cast;
		this.plot = plot;
		this.genre = genre;
		this.rating = rating;
	}

	public String getTmdbID() {
		return tmdbID;
	}

	public String getName() {
		return name;
	}

	public String getMovieImage() {
		return movieImage;
	}

	public String getReleaseDate() {
		return releaseDate;
	}

	public String getEpisodeRunTime() {
		return episodeRunTime;
	}

	public String getYoutubeTrailer() {
		return youtubeTrailer;
	}

	public String getDirector() {
		return director;
	}

	public String getCast() {
		return cast;
	}

	public String getPlot() {
		return plot;
	}

	public String getGenre() {
		return genre;
	}

	public String getRating() {
		return rating;
	}
}
