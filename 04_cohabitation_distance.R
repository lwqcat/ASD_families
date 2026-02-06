
library(vegan)
library(tidyverse)
library(ggpubr)

# 1. Load Data and Calculate Global Distance Matrix
otu_table <- read.delim('species_abundance_table.csv', sep = ',', row.names = 1)
data_t <- as.data.frame(t(otu_table))
data_t[is.na(data_t)] <- 0

# Calculate Bray-Curtis dissimilarity
dist_matrix <- as.matrix(vegdist(data_t, method = "bray"))

# Convert matrix to long format (Pairwise list)
dist_long <- as.data.frame(as.table(dist_matrix)) %>%
  rename(ID1 = Var1, ID2 = Var2, BC_dist = Freq) %>%
  mutate(Pair = paste0(ID1, ID2))

# 2. Extract Specific Relationship Pairs
# Function to load pair lists and label them
get_pair_dist <- function(file_path, label, global_dist) {
  pairs <- read.delim(file_path, header = FALSE, sep = '\t')
  global_dist %>%
    filter(Pair %in% pairs$V1) %>%
    mutate(Relationship = label)
}

# Define your pair files and their corresponding labels
pair_files <- list(
  c("Multiplex_Pair.txt", "Multiplex sibling Pairs"),
  c("Simplex_Pair.txt", "Simplex sibling Pairs"),
  c("TD_Pair.txt", "TD sibling Pairs"),
  c("random_Pair.txt", "Random Pairs"),
  c("random_Pair_ASD.txt", "ASD Random Pairs")
)

# Batch processing
all_pairs_dist <- map_dfr(pair_files, ~get_pair_dist(.x[1], .x[2], dist_long))

# Set factor levels for consistent plotting order
rel_levels <- c("Random Pairs", "ASD Random Pairs", "Simplex sibling Pairs", 
                "Multiplex sibling Pairs", "TD sibling Pairs")
all_pairs_dist$Relationship <- factor(all_pairs_dist$Relationship, levels = rel_levels)

# 3. Visualization (Figure 2)
my_comparisons <- list(
  c("Random Pairs", "ASD Random Pairs"),
  c("Random Pairs", "Simplex sibling Pairs"),
  c("Random Pairs", "Multiplex sibling Pairs"),
  c("Random Pairs", "TD sibling Pairs"),
  c("Simplex sibling Pairs", "Multiplex sibling Pairs"),
  c("Simplex sibling Pairs", "TD sibling Pairs"),
  c("Multiplex sibling Pairs", "TD sibling Pairs")
)

# Custom Colors
pair_colors <- c("", "", "", "", "")

p_dist <- ggboxplot(all_pairs_dist, x = "Relationship", y = "BC_dist", 
                    color = "Relationship", palette = pair_colors,
                    add = "jitter", add.params = list(alpha = 0.5, size = 1.5)) +
  stat_compare_means(method = "kruskal.test", label.y = 1.1) + # Global P-value
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", 
                     label = "p.format", size = 3.5) +
  labs(x = "", y = "Bray–Curtis distance of species") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title = element_text(face = "bold"))

# 4. Save Outputs
write.csv(all_pairs_dist, "BC_distance_all_groups.csv", row.names = FALSE)
ggsave("Figure2_BC_Dissimilarity.pdf", plot = p_dist, width = 8, height = 7)
