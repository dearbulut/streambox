package nemosofts.streambox.adapter;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageHelperView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemCast;

public class AdapterMovieCast extends RecyclerView.Adapter<AdapterMovieCast.ViewHolder> {

    private final List<ItemCast> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterMovieCast(List<ItemCast> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_cast, parent, false);
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.title.setText(arrayList.get(position).getName());

        try{
            Picasso.get()
                    .load("https://image.tmdb.org/t/p/w300"+arrayList.get(position).getProfile())
                    .placeholder(R.drawable.ic_credits)
                    .error(R.drawable.ic_credits)
                    .into(holder.profile);
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.linearLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final ImageHelperView profile;
        private final TextView title;
        private final LinearLayout linearLayout;

        public ViewHolder(View itemView) {
            super(itemView);
            profile = itemView.findViewById(R.id.iv_cast);
            title = itemView.findViewById(R.id.tv_cast);
            linearLayout = itemView.findViewById(R.id.ll_cast);
        }
    }

    @Override
    public long getItemId(int id) {
        return id;
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemCast itemCast, int position);
    }
}