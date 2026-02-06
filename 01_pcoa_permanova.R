library(vegan)
library(ggpubr)
library(reshape2)
library(ggsci)
library(lubridate)
library(dplyr)
library(ggExtra)

# Set global options
options(stringsAsFactors = FALSE)

# Data Cleaning & Subsetting
# Select relevant columns and filter groups
metadata <- metadata %>%
  select(STUDY_NO, Group) %>%
  rename(sample = STUDY_NO, group = Group)

# Define groups to compare here
target_groups <- c("GroupA", "GroupB") 
metadata <- metadata %>% filter(group %in% target_groups)
metadata$group <- factor(metadata$group, levels = target_groups)

# Align OTU table with metadata
otu_subset <- otu_table[, colnames(otu_table) %in% metadata$sample]
otu_subset <- as.matrix(otu_subset)
metadata <- metadata[metadata$sample %in% colnames(otu_subset), ]
otu_subset <- otu_subset[, metadata$sample]

# Distance Calculation (Bray-Curtis)
data_t <- as.data.frame(t(otu_subset))
data_t[is.na(data_t)] <- 0
dist_matrix <- vegdist(data_t, method = "bray")

# PCoA Calculation
pcoa <- cmdscale(dist_matrix, k = 3, eig = TRUE)
pc_importance <- round(pcoa$eig / sum(pcoa$eig) * 100, digits = 2)
pc_coords <- as.data.frame(pcoa$points[, 1:2])
colnames(pc_coords) <- c("pc_x", "pc_y")
pc_coords$sample <- rownames(pc_coords)

# Merge coordinates with metadata
plot_data <- merge(pc_coords, metadata, by = "sample")

# PERMANOVA Statistics
set.seed(123)
permanova <- adonis2(dist_matrix ~ group, data = metadata, permutations = 999)
p_val <- permanova$`Pr(>F)`[1]
r2_val <- round(permanova$R2[1], digits = 3)

# Visualization
# Define colors based on your legend
my_colors <- c("GroupA" = "", "GroupB" = "")

p_pcoa <- ggscatter(plot_data, x = "pc_x", y = "pc_y", color = "group",
                    fill = "group", shape = "group", palette = my_colors, size = 3,
                    ellipse = FALSE, alpha = 0.5, mean.point = FALSE,
                    star.plot = TRUE, star.plot.lty = 1, star.plot.lwd = 0.2) +
  labs(x = paste0("PCoA1 (", pc_importance[1], "%)"),
       y = paste0("PCoA2 (", pc_importance[2], "%)"),
       title = paste0("PERMANOVA P = ", p_val, ", R2 = ", r2_val)) +
  scale_shape_manual(values = rep(21, length(unique(plot_data$group)))) +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        text = element_text(size = 12))

# Save Plot
ggsave('pcoa_comparison_plot.pdf', width = 6, height = 5, plot = p_pcoa)
