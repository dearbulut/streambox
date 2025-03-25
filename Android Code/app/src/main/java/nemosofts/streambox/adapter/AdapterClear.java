package nemosofts.streambox.adapter;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;

import nemosofts.streambox.item.ItemSetting;

public class AdapterClear extends RecyclerView.Adapter<AdapterClear.ViewHolder> {

    private final List<ItemSetting> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterClear(List<ItemSetting> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_clear,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.title.setText(arrayList.get(position).getName());
        try{
            holder.icon.setImageResource(arrayList.get(position).getDrawableData());
        } catch (Exception e) {
            Log.e("AdapterClear", "Error setImageResource",e);
        }
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(holder.getAbsoluteAdapterPosition()));
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView title;
        private final ImageView icon;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);
            title = itemView.findViewById(R.id.tv_clear);
            icon = itemView.findViewById(R.id.iv_icon);
            relativeLayout = itemView.findViewById(R.id.rl_clear);
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
