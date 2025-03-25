package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterCategory extends RecyclerView.Adapter<AdapterCategory.ViewHolder> {

    private final Context context;
    private List<ItemCat> arrayList;
    private final List<ItemCat> filteredArrayList;
    private final RecyclerItemClickListener listener;
    private int rowIndex = -1;
    private NameFilter filter;
    private final Boolean isTvBox;
    private Boolean isRequestFocus = true;

    public AdapterCategory(Context context,List<ItemCat> arrayList, RecyclerItemClickListener listener) {
        this.context = context;
        this.arrayList = arrayList;
        this.filteredArrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_category,parent, false);
        return new ViewHolder(v);
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, @SuppressLint("RecyclerView") int position) {
        ItemCat currentItem = arrayList.get(position);

        holder.titleTV.setVisibility(Boolean.TRUE.equals(isTvBox) ? View.VISIBLE : View.GONE);
        holder.titleMob.setVisibility(Boolean.TRUE.equals(isTvBox) ? View.GONE : View.VISIBLE);

        holder.titleTV.setText(currentItem.getName().isEmpty() ? "Uncategorized" : currentItem.getName());
        holder.titleTV.setOnClickListener(v -> {
            rowIndex = position;
            notifyDataSetChanged(); // Highlight selected item
            listener.onClickListener(getPosition(currentItem.getId()));
        });

        holder.titleMob.setText(currentItem.getName().isEmpty() ? "Uncategorized" : currentItem.getName());
        holder.titleMob.setOnClickListener(v -> {
            rowIndex = position;
            notifyDataSetChanged(); // Highlight selected item
            listener.onClickListener(getPosition(currentItem.getId()));
        });

        if (rowIndex == position) {
            holder.titleMob.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            holder.titleTV.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            if (Boolean.TRUE.equals(isTvBox)) {
                if (Boolean.TRUE.equals(isRequestFocus)){
                    holder.titleTV.requestFocus();
                }

                holder.vwMob.setVisibility(View.GONE);
                holder.vwTV.setVisibility(View.VISIBLE);
            } else {
                holder.vwMob.setVisibility(View.VISIBLE);
                holder.vwTV.setVisibility(View.GONE);
            }
        } else {
            holder.titleMob.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.vwMob.setVisibility(View.GONE);

            holder.titleTV.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.vwTV.setVisibility(View.GONE);
        }
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView titleMob;
        private final TextView titleTV;
        private final View vwMob;
        private final View vwTV;

        public ViewHolder(View itemView) {
            super(itemView);

            titleMob = itemView.findViewById(R.id.tv_cat_mob);
            vwMob = itemView.findViewById(R.id.vw_cat_mob);
            titleTV = itemView.findViewById(R.id.tv_cat_tv);
            vwTV = itemView.findViewById(R.id.vw_cat_tv);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(int position);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        rowIndex = position;
        notifyDataSetChanged();
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
                isRequestFocus = false;
            } else {
                synchronized (this) {
                    result.values = filteredArrayList;
                    result.count = filteredArrayList.size();
                }
                isRequestFocus = true;
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
}