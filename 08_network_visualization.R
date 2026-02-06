
# --- Extended Data Figure 8B: Strain-Sharing Network Analysis ---
# Visualizing the connectivity between subjects based on shared microbial strains.

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)

# Network Construction 
# Define shared strain count as edge weight
# We focus on "Strong Connections" (shared species >= 10) to reduce noise
min_weight <- 10

network_edges <- st_df %>%
  filter(transmission_event == "Yes") %>%
  group_by(sampleID_1, sampleID_2, Family_type) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  rename(from = sampleID_1, to = sampleID_2) %>%
  filter(weight >= min_weight)

# Prepare node attributes
nodes_from <- network_edges %>% select(id = from, Family_type)
nodes_to <- network_edges %>% select(id = to, Family_type)
network_nodes <- bind_rows(nodes_from, nodes_to) %>% 
  distinct(id, .keep_all = TRUE) %>%
  mutate(Family_type = factor(Family_type, levels = c("Multiplex", "Simplex", "TD")))

# Build tidygraph object
graph_obj <- tbl_graph(nodes = network_nodes, edges = network_edges, directed = FALSE) %>%
  activate(nodes) %>%
  filter(!node_is_isolated()) # Remove subjects with no strong sharing

# 3. Visualization 
my_colors <- c("Multiplex" = "", "Simplex" = "", "TD" = "")

# Function to generate standardized network plots
plot_sharing_network <- function(g_obj, layout_type = "fr", suffix = "") {
  p <- ggraph(g_obj, layout = layout_type) +
    geom_edge_link(aes(width = weight), alpha = 0.4, color = "grey70") +
    geom_node_point(aes(color = Family_type), size = 3, alpha = 0.8) +
    scale_edge_width(range = c(0.4, 2), name = paste("Shared Strains (≥", min_weight, ")", sep="")) +
    scale_color_manual(values = my_colors, name = "Family Type") +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    ) +
    labs(title = paste("Strain-Sharing Network (Layout:", layout_type, ")"))
  
  # Save outputs
  ggsave(paste0("Fig_E8B_Network_", layout_type, suffix, ".pdf"), p, width = 8, height = 7)
  return(p)
}

p1 <- plot_sharing_network(graph_obj, layout_type = "kk")   # Kamada-Kawai (Energy based)

print(p1)
