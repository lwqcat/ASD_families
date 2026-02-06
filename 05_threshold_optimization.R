library(tidyverse)
library(ggplot2)
library(patchwork)
library(cutpointr)

Threshold Calculation Function
# Calculates Youden Index on balanced data to validate robustness
calc_youden <- function(data, name) {
  if (is.null(data) || nrow(data) < 10) return(NULL)
  
  cp <- cutpointr(data, x = distance, class = related, 
                  pos_class = "related", direction = ">=",
                  method = maximize_metric, metric = youden)
  
  tibble(
    species = name,
    threshold_balanced = cp$optimal_cutpoint,
    youden_index_balanced = cp$youden,
    n_balanced_related = sum(data$related == "related"),
    n_balanced_unrelated = sum(data$related == "unrelated")
  )
}

# Run calculation across all balanced species
balanced_results <- map2_dfr(balanced_datasets, names(balanced_datasets), calc_youden)
write_csv(balanced_results, "youden_index_on_balanced_data.csv")

# 3. Density Plotting Function
# Generates Supplementary Figure 3 style plots
plot_strain_density <- function(strain_id, dataset_list, ref_data, index_df) {
  
  strain_data <- dataset_list[[strain_id]]
  ref_info <- ref_data %>% filter(species == strain_id)
  
  # Thresholds from original analysis
  new_threshold <- ref_info$threshold[1]    # Red Solid: Youden
  orig_threshold <- ref_info$current_nGD[1] # Blue Dashed: Original 
  
  # Get pretty species name
  display_name <- index_df$Species[index_df$Strain == strain_id]
  if(length(display_name) == 0) display_name <- strain_id
  
  ggplot(strain_data, aes(x = distance, fill = related)) +
    geom_density(alpha = 0.6, adjust = 1.2) +
    # Vertical threshold lines
    geom_vline(xintercept = new_threshold, color = "#e41a1c", linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = orig_threshold, color = "#377eb8", linetype = "dashed", linewidth = 1) +
    # Annotations
    annotate("text", x = new_threshold, y = Inf, label = sprintf("New: %.4f", new_threshold),
             vjust = 2, hjust = -0.1, color = "#e41a1c", size = 3, fontface = "bold") +
    annotate("text", x = orig_threshold, y = Inf, label = sprintf("Orig: %.4f", orig_threshold),
             vjust = 4, hjust = -0.1, color = "#377eb8", size = 3, fontface = "bold") +
    scale_fill_manual(values = c("related" = "#1b9e77", "unrelated" = "#d95f02")) +
    labs(title = display_name, x = "nGD Distance", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA))
}

# 4. Batch Production of Combined Figures
output_dir <- "balanced_dataset_plots"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Define ranks to export
rank_list <- c(25, 35, 50)
species_ranking <- balanced_results %>% arrange(desc(n_balanced_related))

for (n in rank_list) {
  top_strains <- head(species_ranking$species, n)
  
  # Create list of plots
  plot_list <- map(top_strains, ~plot_strain_density(.x, balanced_datasets, original_thresholds, species_index))
  
  # Combine using patchwork
  cols <- 5
  rows <- ceiling(n / cols)
  combined_p <- wrap_plots(plot_list, ncol = cols, nrow = rows)
  
  ggsave(file.path(output_dir, paste0("Supplementary_Fig3_Top", n, ".pdf")), 
         combined_p, width = cols * 4, height = rows * 3)
  
  # Save matching statistics
  species_ranking %>% head(n) %>% write_csv(file.path(output_dir, paste0("stats_top", n, ".csv")))
}
