library(tidyverse)
library(ggplot2)
library(ggpubr)
library(FSA) # Required for Dunn's Test (Post-hoc analysis)

# 1. Data Selection and Factor Leveling
# Define consistent color palette for publication
group_colors <- c(
  "Simplex" = "",   
  "Multiplex" = "", 
  "TD" = ""        
)

# Validate input data from previous steps
if (exists("Ratio")) {
  # Filter only for 'Yes' transmission events and specific groups
  divdata3 <- Ratio %>% 
    filter(transmission_event == "Yes" & Family_type %in% names(group_colors))
} else {
  stop("Critical Error: 'Ratio' object not found. Run sharing_rate_stats.R first.")
}

# Ensure correct factor levels for plot ordering
divdata3$Family_type <- factor(divdata3$Family_type, levels = c("Multiplex", "Simplex", "TD"))

# 2. Plotting S4A: High-Resolution Hollow Density Plot
# Hollow design improves clarity when multiple distributions overlap
p_s4a <- ggplot(divdata3, aes(x = transmission_rate, color = Family_type)) +
  geom_line(stat = "density", linewidth = 1.5, adjust = 1.2) +
  scale_color_manual(values = group_colors) +
  xlim(0, 20) + # Focus on the main distribution range (0-20%)
  labs(
    title = "Density distribution of strain sharing rates",
    x = "Percentage of strain sharing (%)",
    y = "Density",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = c(0.8, 0.75), 
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank()
  )

# Display plot
print(p_s4a)

# Save as high-resolution PDF for publication
ggsave("S4A_Density_Plot_Strain_Sharing.pdf", p_s4a, width = 9, height = 5, dpi = 600)

# 3. Comprehensive Statistical Testing
# A. Kruskal-Wallis Test (Non-parametric global comparison)
kw_test <- kruskal.test(transmission_rate ~ Family_type, data = divdata3)

# B. Dunn's Post-hoc Test with Benjamini-Hochberg correction
# This aligns with Supplementary Table 22 in standard microbiome publications
dunn_res <- dunnTest(transmission_rate ~ Family_type, 
                     data = divdata3, 
                     method = "bh")$res

# 4. Export Statistical Reports
# Summary table for Results section
summary_stats <- divdata3 %>%
  group_by(Family_type) %>%
  summarise(
    N_Observations = n(),
    Median = median(transmission_rate, na.rm = TRUE),
    Mean = mean(transmission_rate, na.rm = TRUE),
    SD = sd(transmission_rate, na.rm = TRUE)
  )

# Output results to console
print(summary_stats)
print(dunn_res)

# Write results to files
write_csv(summary_stats, "S4A_Summary_Statistics.csv")
write_csv(dunn_res, "S4A_Statistical_Tests_Dunn.csv")
