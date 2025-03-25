package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemWallpaper;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterWallpaper extends RecyclerView.Adapter<AdapterWallpaper.VideoViewHolder> {

    private final RecyclerItemClickListener listener;
    private final List<ItemWallpaper> arrayList;

    public AdapterWallpaper(List<ItemWallpaper> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public VideoViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.layout_wallpaper,parent, false);
        return new VideoViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull VideoViewHolder holder, int position) {

        int width;
        int height;
        if (ApplicationUtil.isLandscapeWallpaper(arrayList.get(position).getWidth(), arrayList.get(position).getHeight())){
            width = 300;
            height = 170;
        } else {
            width = 300;
            height = 525;
        }

        Picasso.get()
                .load("https://image.tmdb.org/t/p/original"+arrayList.get(position).getFilePath())
                .resize(width, height)
                .centerCrop()
                .placeholder(R.color.bg_color_load)
                .error(R.color.bg_color_load)
                .into(holder.wallpaper);

        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(holder.getAbsoluteAdapterPosition()));
    }

    public static class VideoViewHolder extends RecyclerView.ViewHolder {

        ImageHelperView wallpaper;
        RelativeLayout relativeLayout;

        public VideoViewHolder(@NonNull View itemView) {
            super(itemView);

            wallpaper = itemView.findViewById(R.id.iv_wallpaper);
            relativeLayout = itemView.findViewById(R.id.rl_wallpaper);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(int position);
    }
}