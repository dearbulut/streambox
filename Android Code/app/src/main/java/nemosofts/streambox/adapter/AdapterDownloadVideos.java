package nemosofts.streambox.adapter;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.io.File;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterDownloadVideos extends RecyclerView.Adapter<AdapterDownloadVideos.MyViewHolder> {

    private final DBHelper dbHelper;
    private final Context context;
    private final List<ItemVideoDownload> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;
    private final int columnHeight;
    private final Boolean isTvBox;

    public AdapterDownloadVideos(Context context, List<ItemVideoDownload> arrayList, RecyclerItemClickListener listener) {
        this.context = context;
        this.arrayList = arrayList;
        this.listener = listener;
        dbHelper = new DBHelper(context);
        columnWidth = ApplicationUtil.getColumnWidth(context,6, 0);
        columnHeight = (int) (columnWidth * 1.15);
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_movie, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.title.setText(arrayList.get(position).getName());
        holder.rating.setVisibility(View.GONE);

        holder.poster.setScaleType(ImageView.ScaleType.CENTER_CROP);
        holder.poster.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnHeight));
        holder.vw.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnHeight));

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(Boolean.TRUE.equals(isTvBox) ? columnWidth : 250, Boolean.TRUE.equals(isTvBox) ? columnHeight : 350)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.poster);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.vw.setOnLongClickListener(v -> {
            DialogUtil.deleteDialog(context, () -> {
                try {
                    dbHelper.removeFromDownload(DBHelper.TABLE_DOWNLOAD_MOVIES, arrayList.get(holder.getAbsoluteAdapterPosition()).getStreamID());
                    deleteFiles(arrayList.get(position).getVideoURL());
                    deleteFiles(arrayList.get(position).getStreamIcon());
                    arrayList.remove(holder.getAbsoluteAdapterPosition());
                    notifyItemRemoved(holder.getAbsoluteAdapterPosition());
                    listener.onDelete();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            return false;
        });
        holder.vw.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View vw;
        private final ImageHelperView poster;
        private final TextView rating;
        private final TextView title;

        public MyViewHolder(View view) {
            super(view);
            vw = view.findViewById(R.id.fd_movie_card);
            poster = view.findViewById(R.id.iv_movie);
            rating = view.findViewById(R.id.tv_movie_rating);
            title = view.findViewById(R.id.tv_movie_title);
        }
    }

    private void deleteFiles(String path) {
        try {
            File file = new File(path);
            if (file.exists() && file.delete()) {
                Toast.makeText(context, context.getString(R.string.file_deleted), Toast.LENGTH_SHORT).show();
            }
        } catch (Exception e) {
            Log.e("AdapterDownload", "deleteFiles",e);
        }
    }


    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemVideoDownload itemVideo, int position);
        void onDelete();
    }
}