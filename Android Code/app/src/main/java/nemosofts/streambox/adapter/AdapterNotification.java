package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemNotification;

public class AdapterNotification extends RecyclerView.Adapter<AdapterNotification.ViewHolder> {

    private final List<ItemNotification> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterNotification(List<ItemNotification> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_notification,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.title.setText(arrayList.get(position).getTitle());
        holder.titleSub.setText(arrayList.get(position).getMsg());
        holder.notifyOn.setText(arrayList.get(position).getDate());
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(position),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        TextView title;
        TextView titleSub;
        TextView notifyOn;
        RelativeLayout relativeLayout;

        public ViewHolder(View view) {
            super(view);
            title = view.findViewById(R.id.tv_notify_title);
            titleSub = view.findViewById(R.id.tv_notify_sub_title);
            notifyOn = view.findViewById(R.id.tv_notify_on);
            relativeLayout = view.findViewById(R.id.rl_notify);
        }
    }


    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemNotification notification, int position);
    }
}
