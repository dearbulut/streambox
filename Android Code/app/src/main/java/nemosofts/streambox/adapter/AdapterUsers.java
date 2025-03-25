package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterUsers extends RecyclerView.Adapter<AdapterUsers.ViewHolder> {

    private final Context ctx;
    private List<ItemUsersDB> arrayList;
    private final List<ItemUsersDB> filteredArrayList;
    private NameFilter filter;
    private final RecyclerItemClickListener listener;
    private final DBHelper dbHelper;

    public AdapterUsers(Context ctx, List<ItemUsersDB> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.filteredArrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
        dbHelper = new DBHelper(ctx);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_users,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        String anyName;
        String usersName;
        String usersUrl;
        switch (arrayList.get(position).getUserType()) {
            case "xui" :
                anyName = arrayList.get(position).getAnyName();
                usersName = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  "Login Type:  Xtream Codes / Xui";
                break;
            case "stream" :
                anyName = arrayList.get(position).getAnyName();
                usersName = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  "Login Type:  1-stream";
                break;
            case "playlist" :
                anyName = arrayList.get(position).getAnyName();
                usersName = "Login Type:  M3U Playlist";
                usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getUserURL();
                break;
            default :
                anyName = arrayList.get(position).getAnyName();
                usersName = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getUserURL();
                break;
        }

        holder.tvAnyName.setText(anyName);
        holder.tvUsersUrl.setText(usersUrl);
        holder.tvUsersName.setText(usersName);

        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(getPosition(arrayList.get(position).getId())));
        holder.relativeLayout.setOnLongClickListener(v -> {
            DialogUtil.deleteDialog(ctx, () -> {
                try {
                    dbHelper.removeFromUser(arrayList.get(holder.getAbsoluteAdapterPosition()).getId());
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

        private final TextView tvAnyName;
        private final TextView tvUsersUrl;
        private final TextView tvUsersName;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);
            tvAnyName = itemView.findViewById(R.id.tv_users_any_name);
            tvUsersUrl = itemView.findViewById(R.id.tv_users_url);
            tvUsersName = itemView.findViewById(R.id.tv_users_name);
            relativeLayout = itemView.findViewById(R.id.rl_users_list);
        }
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
                ArrayList<ItemUsersDB> filteredItems = new ArrayList<>();
                for (int i = 0, l = filteredArrayList.size(); i < l; i++) {
                    String nameList = filteredArrayList.get(i).getAnyName();
                    String userNameList = filteredArrayList.get(i).getUseName();
                    if (nameList.toLowerCase().contains(constraint) || userNameList.toLowerCase().contains(constraint))
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
            ArrayList<ItemUsersDB> filteredItems = (ArrayList<ItemUsersDB>) results.values;
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
