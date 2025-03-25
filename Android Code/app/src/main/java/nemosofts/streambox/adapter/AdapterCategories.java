package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemCat;

public class AdapterCategories extends RecyclerView.Adapter<AdapterCategories.ViewHolder> {

    private List<ItemCat> arrayList;
    private final List<ItemCat> filteredArrayList;
    private NameFilter filter;
    private final RecyclerItemClickListener listener;
    private final boolean isPage;

    public AdapterCategories(boolean isPage, List<ItemCat> arrayList, RecyclerItemClickListener listener) {
        this.isPage = isPage;
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
        if (isPage){
            holder.logo.setImageResource(R.drawable.ic_folders);
        }
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(getPosition(arrayList.get(position).getId())));
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView title;
        private final ImageView logo;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);

            title = itemView.findViewById(R.id.tv_cat);
            logo = itemView.findViewById(R.id.iv_catch_up);
            relativeLayout = itemView.findViewById(R.id.rl_cat);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    private int getPosition(String id) {
        for (int i = 0; i < filteredArrayList.size(); i++) {
            if (id.equals(filteredArrayList.get(i).getId())) {
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
                ArrayList<ItemCat> filteredItems = new ArrayList<>();
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
            ArrayList<ItemCat> filteredItems = (ArrayList<ItemCat>) results.values;
            arrayList = filteredItems;
            notifyDataSetChanged();
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(int position);
    }
}
