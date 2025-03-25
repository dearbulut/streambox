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
import nemosofts.streambox.item.ItemEpgFull;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterCatchUpEpg extends RecyclerView.Adapter<AdapterCatchUpEpg.ViewHolder> {

    private final List<ItemEpgFull> arrayList;
    private final RecyclerItemClickListener listener;
    private final Boolean is12h;

    public AdapterCatchUpEpg(Boolean is12h, List<ItemEpgFull> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.is12h = is12h;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_tab_epg_full,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.title.setText(ApplicationUtil.decodeBase64(arrayList.get(position).getTitle()));
        String dataTime = ApplicationUtil.getTimestamp(arrayList.get(position).getStartTimestamp(), is12h)
                + " - " + ApplicationUtil.getTimestamp(arrayList.get(position).getStopTimestamp(), is12h);
        holder.date.setText(dataTime);
        holder.active.setVisibility(arrayList.get(position).getHasArchive() == 1 ? View.VISIBLE : View.GONE);
        holder.activeNone.setVisibility(arrayList.get(position).getHasArchive() == 1 ? View.GONE : View.VISIBLE);
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView title;
        private final TextView date;
        private final TextView active;
        private final TextView activeNone;
        private final RelativeLayout relativeLayout;

        public ViewHolder(View itemView) {
            super(itemView);

            title = itemView.findViewById(R.id.tv_epg_title);
            date = itemView.findViewById(R.id.tv_epg_date);
            active = itemView.findViewById(R.id.tv_active);
            activeNone = itemView.findViewById(R.id.tv_none);
            relativeLayout = itemView.findViewById(R.id.rl_tab_epg_full);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemEpgFull item, int position);
    }
}
