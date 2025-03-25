package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemVideo;

public class AdapterVideo extends RecyclerView.Adapter<AdapterVideo.VideoViewHolder> {

    private final RecyclerItemClickListener listener;
    private List<ItemVideo> arrayList;
    private final List<ItemVideo> filteredArrayList;
    private NameFilter filter;

    public AdapterVideo(List<ItemVideo> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.filteredArrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public VideoViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_url_list,parent, false);
        return new VideoViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull VideoViewHolder holder, int position) {
        ItemVideo itemVideo = arrayList.get(position);

        if (itemVideo.getPath().contains("/storage/emulated/0/")){
            holder.tvVideoUrl.setText(itemVideo.getTitle().replace("/storage/emulated/0/",""));
        } else {
            holder.tvVideoUrl.setText(itemVideo.getPath());
        }

        holder.tvAnyName.setText(itemVideo.getTitle());

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

        holder.linearLayout.setOnClickListener(v -> listener.onClickListener(getPosition(arrayList.get(position).getPath())));
    }

    public static class VideoViewHolder extends RecyclerView.ViewHolder {

        private final TextView tvAnyName;
        private final TextView tvVideoUrl;
        private final ImageHelperView colorBg;
        private final LinearLayout linearLayout;

        public VideoViewHolder(@NonNull View itemView) {
            super(itemView);
            tvAnyName = itemView.findViewById(R.id.tv_any_name);
            tvVideoUrl = itemView.findViewById(R.id.tv_video_url);
            colorBg = itemView.findViewById(R.id.iv_color_bg);

            linearLayout = itemView.findViewById(R.id.ll_single_list);
        }
    }

    private int getPosition(String path) {
        for (int i = 0; i < filteredArrayList.size(); i++) {
            if (path.equals(filteredArrayList.get(i).getPath())) {
                return i;
            }
        }
        return -1; // Not found
    }

    public Filter getFilter() {
        if (filter == null) {
            filter = new NameFilter();
        }
        return filter;
    }

    private class NameFilter extends Filter {

        @NonNull
        @Override
        protected FilterResults performFiltering(CharSequence constraint) {
            constraint = constraint.toString().toLowerCase();
            FilterResults result = new FilterResults();
            if (!constraint.toString().isEmpty()) {
                ArrayList<ItemVideo> filteredItems = new ArrayList<>();
                for (int i = 0, l = filteredArrayList.size(); i < l; i++) {
                    String nameList = filteredArrayList.get(i).getTitle();
                    if (nameList.toLowerCase().contains(constraint))
                        filteredItems.add(filteredArrayList.get(i));
                }
                result.count = filteredItems.size();
                result.values = filteredItems;
            } else {
                synchronized (this) {
                    result.values = filteredArrayList;
                    result.count = filteredArrayList.size();
                }
            }
            return result;
        }

        @SuppressLint("NotifyDataSetChanged")
        @Override
        protected void publishResults(CharSequence constraint, @NonNull FilterResults results) {
            @SuppressWarnings("unchecked")
            ArrayList<ItemVideo> filteredItems = (ArrayList<ItemVideo>) results.values;
            arrayList = filteredItems;
            notifyDataSetChanged();
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