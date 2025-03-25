package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import org.jetbrains.annotations.NotNull;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterChannelEpg extends RecyclerView.Adapter<AdapterChannelEpg.ViewHolder> {

    private final Context context;
    private final List<ItemChannel> arrayList;
    private final RecyclerItemClickListener listener;
    private int rowIndex = 0;
    private final Boolean isTvBox;

    public AdapterChannelEpg(Context context, List<ItemChannel> arrayList, RecyclerItemClickListener listener) {
        this.context = context;
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NotNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_epg_live_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(300, 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.logo);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.title.setText(arrayList.get(position).getName());
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );

        if (rowIndex > -1) {
            if (rowIndex == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.relativeLayout.requestFocus();
                }
                holder.title.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.vw.setVisibility(View.VISIBLE);
            } else {
                holder.title.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.vw.setVisibility(View.GONE);
            }
        } else {
            holder.title.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.vw.setVisibility(View.GONE);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final ImageView logo;
        private final TextView title;
        private final View vw;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View view) {
            super(view);
            logo = view.findViewById(R.id.iv_live_logo);
            title = view.findViewById(R.id.tv_live_name);
            relativeLayout = view.findViewById(R.id.rl_epg_live);
            vw = itemView.findViewById(R.id.vw_live);
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemChannel itemChannel, int position);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        rowIndex = position;
        notifyDataSetChanged();
    }
}
