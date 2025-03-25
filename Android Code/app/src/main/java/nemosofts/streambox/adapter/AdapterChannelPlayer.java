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
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterChannelPlayer extends RecyclerView.Adapter<AdapterChannelPlayer.ViewHolder> {

    private final List<ItemChannel> arrayList;
    private final Context context;
    private final RecyclerItemClickListener listener;
    private int rowIndex = 0;
    private final Boolean isTvBox;

    public AdapterChannelPlayer(Context context, List<ItemChannel> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.context = context;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_player_live,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(300, 300)
                    .centerCrop()
                    .noFade()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.logo);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        int step = 1;
        for (int i = 1; i < position + 1; i++) {
            step++;
        }
        holder.num.setText(String.valueOf(step));
        holder.title.setText(arrayList.get(position).getName());

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
                    holder.linearLayout.requestFocus();
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

        TextView num;
        ImageHelperView logo;
        TextView title;
        LinearLayout linearLayout;

        public ViewHolder(View view) {
            super(view);
            num = view.findViewById(R.id.tv_num);
            logo = view.findViewById(R.id.iv_live_list_logo);
            title = view.findViewById(R.id.tv_live_list_name);
            linearLayout = view.findViewById(R.id.ll_channels_list);
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
        void onClickListener(ItemChannel itemChannel, int position);
    }
}
