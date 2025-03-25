package nemosofts.streambox.adapter;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import org.jetbrains.annotations.NotNull;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemChannel;

public class AdapterEpgLogo extends RecyclerView.Adapter<AdapterEpgLogo.ViewHolder> {

    private final List<ItemChannel> arrayList;

    public AdapterEpgLogo(List<ItemChannel> arrayList) {
        this.arrayList = arrayList;
    }

    @NotNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_epg_logo,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.ivEpgTitle.setText(arrayList.get(position).getName());

        try{
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(300, 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.ivEpgLogo);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        ImageView ivEpgLogo;
        TextView ivEpgTitle;

        public ViewHolder(View view) {
            super(view);
            ivEpgLogo = view.findViewById(R.id.iv_epg_logo);
            ivEpgTitle = view.findViewById(R.id.iv_epg_title);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }
}
