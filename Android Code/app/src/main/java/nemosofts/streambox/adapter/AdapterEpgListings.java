package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.jetbrains.annotations.NotNull;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterEpgListings extends RecyclerView.Adapter<AdapterEpgListings.ViewHolder> {

    private final List<ItemEpg> arrayList;
    private final Boolean is12h;

    public AdapterEpgListings(Boolean is12h, List<ItemEpg> arrayList) {
        this.arrayList = arrayList;
        this.is12h = is12h;
    }

    @NotNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_epg_listings,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        ItemEpg itemEpg = arrayList.get(position);
        holder.tvEpg.setText(ApplicationUtil.decodeBase64(itemEpg.getTitle()));
        String timeStamp = ApplicationUtil.getTimestamp(itemEpg.getStartTimestamp(), is12h)
                + " - " + ApplicationUtil.getTimestamp(itemEpg.getStopTimestamp(), is12h);
        holder.tvEpgTime.setText(timeStamp);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        TextView tvEpg;
        TextView tvEpgTime;

        public ViewHolder(View view) {
            super(view);

            tvEpg = view.findViewById(R.id.iv_epg_test);
            tvEpgTime = view.findViewById(R.id.iv_epg_start_time);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

}
