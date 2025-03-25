package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemDns;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterSignInDns extends RecyclerView.Adapter<AdapterSignInDns.MyViewHolder> {

    private int rowIndex = 0;
    private final List<ItemDns> arrayList;
    private final RecyclerItemClickListener listener;
    private Boolean isFindFocus = false;
    private final Boolean isTvBox;

    public AdapterSignInDns(Context context, List<ItemDns> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_dns_list, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        holder.title.setText(arrayList.get(position).getTitle());
        if (rowIndex > -1) {
            if (rowIndex == position) {
                if (isFindFocus && isTvBox){
                    holder.linearLayout.requestFocus();
                }
                holder.tick.setVisibility(View.VISIBLE);
            } else {
                holder.tick.setVisibility(View.GONE);
            }
        } else {
            holder.tick.setVisibility(View.GONE);
        }

        holder.linearLayout.setOnClickListener(v ->
                listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition())
        );
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final ImageView tick;
        private final TextView title;
        private final LinearLayout linearLayout;

        private MyViewHolder(View view) {
            super(view);
            tick = view.findViewById(R.id.iv_tick);
            title = view.findViewById(R.id.tv_dns);
            linearLayout = view.findViewById(R.id.ll_dns_list);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setSelected(int position) {
        rowIndex = position;
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setSelectedFocus(int position) {
        isFindFocus = true;
        rowIndex = position;
        notifyDataSetChanged();
    }

    public String getSelectedBase() {
        return arrayList.get(rowIndex).getBase();
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
        void onClickListener(ItemDns itemDns, int position);
    }
}