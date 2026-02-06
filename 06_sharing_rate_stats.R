library(tidyverse)
library(tidyfst)
library(ggpubr)

st_df <- st_df %>%
  left_join(species_index, by = "species_name") %>%
  left_join(family_meta, by = c("subjectID_1" = "subjectID")) %>%
  rename(Family_type = Family_type) %>%
  mutate(Pair_ID = paste0(subjectID_1, "_", subjectID_2))

# Automated Sharing Rate Calculation
# This function replaces your multiple manual subsets (Ratio1, Ratio2...)
calc_sharing_ratio <- function(data, group_label) {
  # Calculate number of unique pairs in this group
  total_pairs <- data %>% filter(Family_type == group_label) %>% pull(Pair_ID) %>% n_distinct()
  
  data %>%
    filter(Family_type == group_label) %>%
    group_by(Family_type, Species, species_name, transmission_event) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Species, species_name) %>%
    mutate(transmission_rate = (n / total_pairs) * 100,
           Total_Group_Pairs = total_pairs)
}

# Apply to all groups including Random
groups <- c("Multiplex", "Simplex", "TD")
all_ratios <- map_dfr(groups, ~calc_sharing_ratio(st_df, .x))

# Statistical Comparison (Boxplot - Figure 3c)
# Focus only on "Yes" events (Actual sharing)
plot_data <- all_ratios %>% 
  filter(transmission_event == "Yes") %>%
  mutate(Family_type = factor(Family_type, levels = c("Multiplex", "Simplex", "TD")))

# Grouping for plot labels
n_labels <- all_ratios %>% group_by(Family_type) %>% summarise(N = first(Total_Group_Pairs))
x_labels <- setNames(paste0(n_labels$Family_type, "\n(n=", n_labels$N, ")"), n_labels$Family_type)

p_box <- ggboxplot(plot_data, x = "Family_type", y = "transmission_rate", 
                   fill = "Family_type", palette = "jco", add = "jitter") +
  stat_compare_means(comparisons = list(c("Multiplex", "Simplex"), c("Multiplex", "TD")),
                     method = "wilcox.test", label = "p.signif") +
  labs(y = "Strain-Sharing Rate per Species (%)", x = "") +
  scale_x_discrete(labels = x_labels) +
  theme_pubr()

# Save Outputs
ggsave("Fig3_Sharing_Boxplot.pdf", p_box, width = 7, height = 6)

