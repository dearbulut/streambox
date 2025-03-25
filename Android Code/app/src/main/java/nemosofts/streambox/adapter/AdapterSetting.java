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
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemSetting;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterSetting extends RecyclerView.Adapter<AdapterSetting.ViewHolder> {

    private final List<ItemSetting> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;
    private final int columnHeight;

    public AdapterSetting(Context context, List<ItemSetting> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        columnWidth = ApplicationUtil.getColumnWidth(context,4, 0);
        columnHeight = (int) (columnWidth * 0.60);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_setting,parent, false);
        return new ViewHolder(v);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        final ItemSetting item = arrayList.get(position);

        holder.tvSetting.setText(item.getName());
        holder.rlSetting.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnHeight));
        try{
            holder.ivSetting.setImageResource(item.getDrawableData());
        } catch (Exception e) {
            Log.e("AdapterSetting", "Error setImageResource",e);
        }
        holder.rlSetting.setOnClickListener(v -> listener.onClickListener(item, position));
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        RelativeLayout rlSetting;
        TextView tvSetting;
        ImageView ivSetting;

        public ViewHolder(View view) {
            super(view);
            rlSetting = view.findViewById(R.id.rl_setting);
            tvSetting = view.findViewById(R.id.tv_setting);
            ivSetting = view.findViewById(R.id.iv_setting);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemSetting item, int position);
    }
}
