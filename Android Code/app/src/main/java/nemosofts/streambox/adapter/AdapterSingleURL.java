package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemSingleURL;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterSingleURL extends RecyclerView.Adapter<AdapterSingleURL.ViewHolder> {

    private final Context ctx;
    private final List<ItemSingleURL> arrayList;
    private final RecyclerItemClickListener listener;
    private final DBHelper dbHelper;

    public AdapterSingleURL(Context ctx, List<ItemSingleURL> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
        dbHelper = new DBHelper(ctx);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_url_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String anyName =   arrayList.get(position).getAnyName();
        String usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getSingleURL();
        holder.anyName.setText(anyName);
        holder.videoUrl.setText(usersUrl);
        holder.linearLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), position));


        int step = (position % 7) + 1;
        switch (step) {
            case 2 :
                holder.colorBg.setImageResource(R.color.color_setting_2);
                break;
            case 3 :
                holder.colorBg.setImageResource(R.color.color_setting_3);
                break;
            case 4 :
                holder.colorBg.setImageResource(R.color.color_setting_4);
                break;
            case 5 :
                holder.colorBg.setImageResource(R.color.color_setting_5);
                break;
            case 6 :
                holder.colorBg.setImageResource(R.color.color_setting_6);
                break;
            case 7 :
                holder.colorBg.setImageResource(R.color.color_setting_7);
                break;
            default :
                holder.colorBg.setImageResource(R.color.color_setting_1);
                break;
        }

        holder.linearLayout.setOnLongClickListener(v -> {
            DialogUtil.deleteDialog(ctx, () -> {
                try {
                    dbHelper.removeFromSingleURL(arrayList.get(holder.getAbsoluteAdapterPosition()).getId());
                    arrayList.remove(holder.getAbsoluteAdapterPosition());
                    notifyItemRemoved(holder.getAbsoluteAdapterPosition());
                    Toast.makeText(ctx, ctx.getString(R.string.delete), Toast.LENGTH_SHORT).show();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            return false;
        });
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView anyName;
        private final TextView videoUrl;
        private final ImageHelperView colorBg;
        private final LinearLayout linearLayout;

        public ViewHolder(View itemView) {
            super(itemView);
            anyName = itemView.findViewById(R.id.tv_any_name);
            videoUrl = itemView.findViewById(R.id.tv_video_url);
            colorBg = itemView.findViewById(R.id.iv_color_bg);

            linearLayout = itemView.findViewById(R.id.ll_single_list);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemSingleURL itemSingleURL, int position);
    }
}
