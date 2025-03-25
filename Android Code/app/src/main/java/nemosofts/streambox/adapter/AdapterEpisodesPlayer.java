package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterEpisodesPlayer extends RecyclerView.Adapter<AdapterEpisodesPlayer.ViewHolder> {

    private final List<ItemEpisodes> arrayList;
    private final Context context;
    private final RecyclerItemClickListener listener;
    private int rowIndex = 0;
    private final Boolean isTvBox;

    public AdapterEpisodesPlayer(Context context, List<ItemEpisodes> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.context = context;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_player_epi,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        try{
            Picasso.get()
                    .load(arrayList.get(position).getCoverBig().isEmpty() ? "null" : arrayList.get(position).getCoverBig())
                    .resize(300, 300)
                    .centerCrop()
                    .noFade()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.logo);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        int step = position + 1;
        holder.num.setText(String.valueOf(step));
        holder.title.setText(arrayList.get(position).getTitle());
        holder.linearLayout.setOnClickListener(v -> {
            try {
                listener.onClickListener(arrayList.get(position), holder.getAbsoluteAdapterPosition());
                select(holder.getAbsoluteAdapterPosition());
            } catch (Exception e) {
                e.printStackTrace();
            }
        });

        if (rowIndex > -1) {
            if (rowIndex == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.title.requestFocus();
                }
                holder.num.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.title.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            } else {
                holder.num.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.title.setTextColor(ContextCompat.getColor(context, R.color.white));
            }
        } else {
            holder.num.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.title.setTextColor(ContextCompat.getColor(context, R.color.white));
        }
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        ImageHelperView logo;
        TextView num;
        TextView title;
        LinearLayout linearLayout;

        public ViewHolder(View view) {
            super(view);
            logo = view.findViewById(R.id.iv_episodes_logo);
            num = view.findViewById(R.id.episodes_num);
            title = view.findViewById(R.id.tv_episodes_name);
            linearLayout = view.findViewById(R.id.ll_episodes_list);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        rowIndex = position;
        notifyDataSetChanged();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemEpisodes itemEpisodes, int position);
    }
}
