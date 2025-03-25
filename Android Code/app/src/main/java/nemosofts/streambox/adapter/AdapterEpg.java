package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemPost;

public class AdapterEpg extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    Context context;
    List<ItemPost> arrayList;
    static final int VIEW_PROG = 0;
    static final int VIEW_LOGO = 1;
    static final int VIEW_LISTINGS = 2;
    private final Boolean is12h;

    public AdapterEpg(Context context, Boolean is12h, List<ItemPost> arrayList) {
        this.context = context;
        this.arrayList = arrayList;
        this.is12h = is12h;
    }

    class LogoHolder extends RecyclerView.ViewHolder {

        RecyclerView rvLogo;

        LogoHolder(View view) {
            super(view);

            rvLogo = view.findViewById(R.id.rv_home_cat);
            LinearLayoutManager linearLayoutManager = new LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false);
            rvLogo.setLayoutManager(linearLayoutManager);
            rvLogo.setItemAnimator(new DefaultItemAnimator());
        }
    }

    class ListingsHolder extends RecyclerView.ViewHolder {

        RecyclerView rvListings;

        ListingsHolder(View view) {
            super(view);

            rvListings = view.findViewById(R.id.rv_home_cat);
            LinearLayoutManager linearLayoutManager = new LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false);
            rvListings.setLayoutManager(linearLayoutManager);
            rvListings.setItemAnimator(new DefaultItemAnimator());
        }
    }

    private static class ProgressViewHolder extends RecyclerView.ViewHolder {

        ProgressBar progressBar;

        private ProgressViewHolder(View v) {
            super(v);
            progressBar = v.findViewById(R.id.progressBar);
        }
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == VIEW_LOGO) {
            View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.layout_home_ui_categories, parent, false);
            return new LogoHolder(itemView);
        } else if (viewType == VIEW_LISTINGS) {
            View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.layout_home_ui_categories, parent, false);
            return new ListingsHolder(itemView);
        } else {
            View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.layout_progressbar, parent, false);
            return new ProgressViewHolder(v);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        if (holder instanceof LogoHolder logoHolder) {
            AdapterEpgLogo adapterEpgLogo = new AdapterEpgLogo(arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListLive());
            logoHolder.rvLogo.setAdapter(adapterEpgLogo);
        } else if (holder instanceof ListingsHolder listingsHolder) {
            AdapterEpgListings adapterEpg = new AdapterEpgListings(is12h, arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListEpg());
            listingsHolder.rvListings.setAdapter(adapterEpg);
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

    @Override
    public int getItemViewType(int position) {
        if (arrayList.get(position).getType() == null) {
            return VIEW_PROG;
        }
        return switch (arrayList.get(position).getType()) {
            case "logo" -> VIEW_LOGO;
            case "listings" -> VIEW_LISTINGS;
            default -> VIEW_PROG;
        };
    }
}