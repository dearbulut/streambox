package nemosofts.streambox.adapter;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.theme.ThemeEngine;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.SPHelper;

public class AdapterMovie extends RecyclerView.Adapter<AdapterMovie.MyViewHolder> {

    private final List<ItemMovies> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;
    private final int columnHeight;
    private final Boolean isTvBox;
    private final Boolean isTitle;
    private int theme;

    public AdapterMovie(Context context, List<ItemMovies> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
        isTitle = new SPHelper(context).getUICardTitle();
        columnWidth = ApplicationUtil.getColumnWidth(context, Boolean.TRUE.equals(isTvBox) ? 8 : 7, 0);
        columnHeight = (int) (columnWidth * 1.15);
        try {
            switch (new ThemeEngine(context).getThemePage()){
                case 1 :
                    theme = R.color.ns_dark_bg_sub;
                    break;
                case 2 :
                    theme = R.color.ns_grey_bg_sub;
                    break;
                case 3 :
                    theme = R.color.ns_blue_bg_sub;
                    break;
                default :
                    theme = R.color.ns_classic_bg_sub;
                    break;
            }
        } catch (Exception e) {
            theme = R.color.bg_color_load;
        }
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_movie, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.title.setVisibility(Boolean.TRUE.equals(isTitle) ? View.VISIBLE : View.GONE);
        holder.title.setText(arrayList.get(position).getName());

        if (!arrayList.get(position).getRating().isEmpty() && arrayList.get(position).getRating().equals("0")){
            holder.llStar.setVisibility(View.GONE);
        } else {
            holder.rating.setText(arrayList.get(position).getRating());
        }

        holder.poster.setScaleType(ImageView.ScaleType.CENTER_CROP);
        holder.poster.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnHeight));
        holder.vw.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnHeight));

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(Boolean.TRUE.equals(isTvBox) ? columnWidth : 200, Boolean.TRUE.equals(isTvBox) ? columnHeight : 300)
                    .centerCrop()
                    .placeholder(theme)
                    .error(theme)
                    .into(holder.poster);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.vw.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View vw;
        private final ImageHelperView poster;
        private final TextView rating;
        private final TextView title;
        private final LinearLayout llStar;

        public MyViewHolder(View view) {
            super(view);
            vw = view.findViewById(R.id.fd_movie_card);
            poster = view.findViewById(R.id.iv_movie);

            llStar = view.findViewById(R.id.ll_card_star);
            rating = view.findViewById(R.id.tv_movie_rating);
            title = view.findViewById(R.id.tv_movie_title);
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
        void onClickListener(ItemMovies itemMovies, int position);
    }
}
