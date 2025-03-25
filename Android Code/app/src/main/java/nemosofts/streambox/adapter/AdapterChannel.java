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
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterChannel extends RecyclerView.Adapter<AdapterChannel.MyViewHolder> {

    private final List<ItemChannel> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;

    public AdapterChannel(Context context, List<ItemChannel> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        Boolean isTvBox  = ApplicationUtil.isTvBox(context);
        columnWidth = ApplicationUtil.getColumnWidth(context, Boolean.TRUE.equals(isTvBox)? 8 : 7, 0);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_live, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        if (arrayList.get(position).getName().isEmpty()){
            holder.title.setVisibility(View.GONE);
        } else {
            holder.title.setText(arrayList.get(position).getName());
        }

        holder.linearLayout.setVisibility(View.GONE);

        holder.logo.setScaleType(ImageView.ScaleType.FIT_CENTER);
        holder.logo.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));
        holder.vw.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .placeholder(R.drawable.bg_card_item_load)
                    .error(R.drawable.bg_card_item_load)
                    .into(holder.logo);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.vw.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View vw;
        private final ImageHelperView logo;
        private final TextView title;
        private final LinearLayout linearLayout;

        public MyViewHolder(View view) {
            super(view);

            vw = view.findViewById(R.id.fd_movie_card);
            logo = view.findViewById(R.id.iv_movie);
            title = view.findViewById(R.id.tv_movie_title);
            linearLayout = view.findViewById(R.id.ll_card_star);
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
        void onClickListener(ItemChannel itemChannel, int position);
    }
}
