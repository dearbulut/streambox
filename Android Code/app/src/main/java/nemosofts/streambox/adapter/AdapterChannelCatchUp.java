package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemChannel;

public class AdapterChannelCatchUp extends RecyclerView.Adapter<AdapterChannelCatchUp.ViewHolder> {

    private List<ItemChannel> arrayList;
    private final List<ItemChannel> filteredArrayList;
    private NameFilter filter;
    private final RecyclerItemClickListener listener;

    public AdapterChannelCatchUp(List<ItemChannel> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.filteredArrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_cat_catch_up,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.title.setText(arrayList.get(position).getName());

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .placeholder(R.drawable.bg_card_item_load)
                    .error(R.drawable.bg_card_item_load)
                    .into(holder.imageView);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(getPosition(arrayList.get(position).getStreamID())));
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView title;
        private final ImageView imageView;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);

            title = itemView.findViewById(R.id.tv_cat);
            imageView = itemView.findViewById(R.id.iv_catch_up);
            relativeLayout = itemView.findViewById(R.id.rl_cat);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    private int getPosition(String id) {
        for (int i = 0; i < filteredArrayList.size(); i++) {
            if (id.equals(filteredArrayList.get(i).getStreamID())) {
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
                ArrayList<ItemChannel> filteredItems = new ArrayList<>();
                for (int i = 0, l = filteredArrayList.size(); i < l; i++) {
                    String nameList = filteredArrayList.get(i).getName();
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
            ArrayList<ItemChannel> filteredItems = (ArrayList<ItemChannel>) results.values;
            arrayList = filteredItems;
            notifyDataSetChanged();
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(int position);
    }
}
