package nemosofts.streambox.adapter;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RatingBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Callback;
import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterEpisodes extends RecyclerView.Adapter<AdapterEpisodes.MyViewHolder> {

    private final List<ItemEpisodes> arrayList;
    private final RecyclerItemClickListener listener;
    private final String seriesCover;
    private final String plotData;
    private final DBHelper dbHelper;

    public AdapterEpisodes(Context ctx, List<ItemEpisodes> arrayList, String cover, String plot,  RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.seriesCover = cover;
        this.plotData = plot;
        dbHelper = new DBHelper(ctx);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_episodes_list, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        try{
            Picasso.get()
                    .load(arrayList.get(position).getCoverBig().isEmpty() ? "null" : arrayList.get(position).getCoverBig())
                    .resize(450, 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .error(R.color.bg_color_load)
                    .into(holder.poster, new Callback() {
                        @Override
                        public void onSuccess() {
                            // do nothing
                        }
                        @Override
                        public void onError(Exception e) {
                            try {
                                Picasso.get()
                                        .load(seriesCover)
                                        .resize(450, 300)
                                        .centerCrop()
                                        .placeholder(R.color.bg_color_load)
                                        .error(R.color.bg_color_load)
                                        .into(holder.poster);
                            } catch (Exception ex) {
                                Log.e("Adapter","Error Picasso load" ,e);
                            }
                        }
                    });
        } catch (Exception e) {
            Log.e("Adapter","Error Picasso load" ,e);
        }

        holder.title.setText(arrayList.get(position).getTitle());

        try {
            long seek = dbHelper.getSeekFull(DBHelper.TABLE_SEEK_EPISODES, arrayList.get(position).getId(), arrayList.get(position).getTitle());
            if (seek > 0){
                holder.pb.setVisibility(View.VISIBLE);
                holder.pb.setProgress(Math.toIntExact(seek));
            } else {
                holder.pb.setVisibility(View.GONE);
            }
        } catch (Exception e) {
            holder.pb.setVisibility(View.GONE);
        }

        try {
            // Adding null check before parsing the rating
            String ratingString = arrayList.get(position).getRating().isEmpty() ? "0" : arrayList.get(position).getRating();
            double newRating = 0.0; // default value
            if (ratingString != null && !ratingString.isEmpty()) {
                newRating = convertToFiveRating(Double.parseDouble(ratingString));
            }
            holder.rb.setRating((float) newRating);
        } catch (Exception e) {
            holder.rb.setRating(0);
        }

        try {
            holder.duration.setText(ApplicationUtil.formatTimeToTime(arrayList.get(position).getDuration()));
        } catch (Exception e) {
            holder.duration.setText("0");
        }

        holder.plot.setText(arrayList.get(position).getPlot().isEmpty() ? plotData : arrayList.get(position).getPlot());
        holder.relativeLayout.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final ImageView poster;
        private final TextView title;
        private final TextView duration;
        private final TextView plot;
        private final RatingBar rb;
        private final ProgressBar pb;
        private final RelativeLayout relativeLayout;

        private MyViewHolder(View view) {
            super(view);
            poster = view.findViewById(R.id.iv_episodes);
            title = view.findViewById(R.id.tv_episodes);
            rb = view.findViewById(R.id.rb_episodes_list);
            duration = view.findViewById(R.id.tv_duration);
            plot = view.findViewById(R.id.tv_plot);
            relativeLayout = view.findViewById(R.id.rl_episodes);
            pb = view.findViewById(R.id.pr_episodes);
        }
    }

    public static double convertToFiveRating(double oldRating) {
        return (oldRating - 1) * 4 / 9 + 1;
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }


    public interface RecyclerItemClickListener{
        void onClickListener(ItemEpisodes itemEpisodes, int position);
    }
}