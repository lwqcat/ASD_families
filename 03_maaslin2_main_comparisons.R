library(Maaslin2)
library(tidyverse)
library(dplyr)
library(tibble)

# 1. Load Data
# df_input_data: Species abundance matrix (Samples as rows or columns depending on Maaslin2 config)
# df_input_metadata: Metadata including Group, Gender, Age, ADHD, and Nutrients

# 2. Identify Dietary Confounders
# Load significant nutrients from previous dietary analysis 
adjustment_nutrients <- unique(sig_nutrients_df$Nutrient[sig_nutrients_df$FDR_adjusted_p < 0.1])
available_nutrients <- adjustment_nutrients[adjustment_nutrients %in% colnames(df_input_metadata)]

# STRATEGY 1: Main Comparisons (Adjusting for Basic Confounders + ADHD)

# Example: 
# Analysis: Multiplex vs. TD (Adjusted for Age, Gender, ADHD)
# Corresponds to Supplementary Data 4
fit_mpx_td <- Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "output_mpx_td_corrected", 
  fixed_effects = c("Group", "Gender", "Age", "ADHD"),
  reference = c("Group,TD", "Gender,1", "ADHD,0"),
  analysis_method = "LM", normalization = "TSS", transform = "LOG",
  plot_heatmap = FALSE, plot_scatter = FALSE
)

# STRATEGY 2: Diet-Adjusted Comparisons

# Analysis: Multiplex vs. TD (Adjusted for Age, Gender, ADHD, and Diet)
# Corresponds to Supplementary Data 5
fit_spx_td_diet <- Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "output_mpx_td_diet_intake", 
  fixed_effects = c("Group", "Gender", "Age", "ADHD", available_nutrients),
  reference = c("Group,TD", "Gender,1", "ADHD,0"),
  analysis_method = "LM", normalization = "TSS", transform = "LOG",
  plot_heatmap = FALSE, plot_scatter = FALSE
)

# STRATEGY 3: Stratified Analysis (Non-ADHD Samples Only)

# Subset metadata for non-ADHD participants
metadata_nonADHD <- df_input_metadata %>% filter(ADHD == "0")
data_nonADHD <- df_input_data[, rownames(metadata_nonADHD)]

# Analysis: Multiplex (Non-ADHD) vs. TD (Adjusted for Age, Gender, Diet)
# Corresponds to Supplementary Data 7
fit_mpx_nonADHD_diet <- Maaslin2(
  input_data = data_nonADHD, 
  input_metadata = metadata_nonADHD, 
  output = "output_mpx_vs_td_nonADHD_diet", 
  fixed_effects = c("Group", "Gender", "Age", available_nutrients),
  reference = c("Group,TD", "Gender,1"),
  analysis_method = "LM", normalization = "TSS", transform = "LOG",
  plot_heatmap = FALSE, plot_scatter = FALSE
)

# 3. Post-processing: Extracting Significant Results (Q < 0.2)

extract_sig <- function(output_dir, metadata_col, q_threshold = 0.2) {
  res_path <- file.path(output_dir, "all_results.tsv")
  if(!file.exists(res_path)) return(NULL)
  
  read.delim(res_path, sep = "\t") %>%
    filter(metadata == metadata_col & qval < q_threshold) %>%
    arrange(qval)
}

# Example usage:
mpx_vs_td_sig <- extract_sig("output_mpx_td_corrected", "Group")
write.csv(mpx_vs_td_sig, "Supplementary_Data_MPX_vs_TD_Significant.csv", row.names = FALSE)
