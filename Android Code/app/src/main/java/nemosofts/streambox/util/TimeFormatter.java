package nemosofts.streambox.util;

import androidx.annotation.NonNull;

import org.jetbrains.annotations.Contract;

public class TimeFormatter {

    private TimeFormatter() {
        throw new IllegalStateException("Utility class");
    }

    private static final int MINUTE = 60;
    private static final int HOUR = 3600;
    private static final int DAY = 86400;
    private static final int WEEK = 604800;
    private static final int MONTH = 2629743; // Approximation for average month
    private static final int YEAR = 31556926; // Approximation for average year

    @NonNull
    @Contract(pure = true)
    public static String formatTimeSpan(long seconds) {
        if (seconds <= 1) {
            return " just now";
        }

        if (seconds < MINUTE) {
            return formatSeconds(seconds);
        } else if (seconds < HOUR) {
            return formatMinutes(seconds);
        } else if (seconds < DAY) {
            return formatHours(seconds);
        } else if (seconds < WEEK) {
            return formatDays(seconds);
        } else if (seconds < MONTH) {
            return formatWeeks(seconds);
        } else if (seconds < YEAR) {
            return formatMonths(seconds);
        } else {
            return formatYears(seconds);
        }
    }

    @NonNull
    @Contract(pure = true)
    private static String formatSeconds(long seconds) {
        return seconds + (seconds == 1 ? " sec ago" : " secs ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatMinutes(long seconds) {
        int min = (int) (seconds / MINUTE);
        return min + (min == 1 ? " min ago" : " mins ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatHours(long seconds) {
        int hours = (int) (seconds / HOUR);
        return hours + (hours == 1 ? " hour ago" : " hours ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatDays(long seconds) {
        int days = (int) (seconds / DAY);
        return days + (days == 1 ? " day ago" : " days ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatWeeks(long seconds) {
        int weeks = (int) (seconds / WEEK);
        return weeks + (weeks == 1 ? " week ago" : " weeks ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatMonths(long seconds) {
        int months = (int) (seconds / MONTH);
        return months + (months == 1 ? " month ago" : " months ago");
    }

    @NonNull
    @Contract(pure = true)
    private static String formatYears(long seconds) {
        int years = (int) (seconds / YEAR);
        return years + (years == 1 ? " year ago" : " years ago");
    }
}
