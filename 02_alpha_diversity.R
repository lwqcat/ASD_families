
library(ggplot2)
library(vegan)
library(tidyverse)
library(ggpubr)

# 1. Load Data
# div_data should contain 'sample', 'group', 'shannon', 'richness'
div_data <- read.csv("alpha_diversity_table.csv")

# 2. Subsetting & Comparisons
target_groups <- c("GroupA", "GroupB")
div_subset <- div_data %>% filter(Group %in% target_groups)
div_subset$Group <- factor(div_subset$Group, levels = target_groups)

comp_list <- list(target_groups)
my_colors <- c("", "")

# 3. Plotting Function (Reusable for Shannon and Richness)
plot_alpha <- function(df, y_var, y_label, colors, comparisons, output_name) {
  p <- ggviolin(df, x = "Group", y = y_var, fill = "Group",
                palette = colors, add = "boxplot", 
                add.params = list(fill = "white", width = 0.1)) +
    stat_compare_means(comparisons = comparisons, method = "wilcox.test",
                       label = "p.format", size = 5, bracket.size = 0.7) +
    labs(x = "", y = y_label) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.line = element_line(size = 1))
  
  ggsave(output_name, plot = p, width = 10, height = 12, units = "cm")
  return(p)
}

# 4. Generate Plots
p_shannon <- plot_alpha(div_subset, "shannon_diversity", 
                        expression(bold(paste(alpha, "-diversity (Shannon)"))), 
                        my_colors, comp_list, "Shannon_violin.pdf")

p_richness <- plot_alpha(div_subset, "richness", 
                         "Richness", 
                         my_colors, comp_list, "Richness_violin.pdf")
