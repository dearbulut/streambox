package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemUsers;

public class AdapterUsersDeviceID extends RecyclerView.Adapter<AdapterUsersDeviceID.ViewHolder> {

    private final Context ctx;
    private final List<ItemUsers> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterUsersDeviceID(Context ctx, List<ItemUsers> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_users_device_id,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String usersName = ctx.getString(R.string.user_list_user_name)+" " + arrayList.get(position).getUserName();
        holder.usersName.setText(usersName);
        holder.relativeLayout.setOnClickListener(v ->
                listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), position)
        );
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView usersName;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);
            usersName = itemView.findViewById(R.id.tv_users_name);
            relativeLayout = itemView.findViewById(R.id.rl_users_list);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemUsers itemUsers, int position);
    }
}
