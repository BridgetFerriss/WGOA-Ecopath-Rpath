source("code/WGOA_EGOA_comp.R")
source("code/ggraph_webplot_Rpath.R")
library(tidyverse)

webplot(w.bal)

#ab.bal <- Rpath::rpath(Rpath::AB.params, eco.name = "Anchovy Bay")
#ebs.bal <- Rpath::rpath(Rpath::Ecosense.EBS, eco.name = "Eastern Bering Sea")
set.seed(123)
p <- ggraph_webplot_Rpath(w.bal, eco.name = "western Gulf of Alaska", 
                          h_spacing = 5, 
                          node_size_min = 1,
                          node_size_max = 30,
                          fleet_color = "#F11B00", 
                          text_size = 3,
                          #highlighted_groups = c("rex_sole_adult", "rex_sole_juvenile"),
                          #highlighted_color  = "black",   # red for highlighted nodes
                          #dim_alpha        = 0.20,         # how faded everything else gets (0–1)
                          max.overlaps = 10,
                          low_tl_spread = 3)


#DO NOT RENDER IN R, it will take a century, but it saves very fast with ggsave
ggsave("figures/WGOAfoodwebplot_high3.png", p , width= 14, height= 8)

set.seed(123)
ep <- ggraph_webplot_Rpath(e.bal, eco.name = "eastern Gulf of Alaska", 
                          h_spacing = 5, 
                          node_size_min = 1,
                          node_size_max = 20,
                          fleet_color = "#F11B00", 
                          text_size = 3,
                          #highlighted_groups = c("rex_sole_adult", "rex_sole_juvenile"),
                          #highlighted_color  = "black",   # red for highlighted nodes
                          #dim_alpha        = 0.20,         # how faded everything else gets (0–1)
                          max.overlaps = 20,
                          low_tl_spread = 10)


#DO NOT RENDER IN R, it will take a century, but it saves very fast with ggsave
ggsave("figures/EGOAfoodwebplot_high2.png", ep , width= 14, height= 8)





wgoap <- rpathviz::webplotviz(w.bal, eco.name = "western Gulf of Alaska",  line.col = "grey",
                          h_spacing = 3,
                          node_size_min = 1,
                          node_size_max = 30,
                          fleet_color = "#B40F20",
                          groups_palette = "rpath_pal_dark",
                          text_size = 3,
                          max.overlaps = 50,
                          gradient = TRUE,
                          labels = TRUE,
                          cluster_method = "edge_betweenness",
                          low_tl_spread = 10)

ggsave("figures/WGOAfoodwebplot2.png", wgoap , width= 16, height= 10)


egoap <- rpathviz::webplotviz(e.bal, eco.name = "eastern Gulf of Alaska",  line.col = "grey",
                              h_spacing = 3,
                              node_size_min = 1,
                              node_size_max = 30,
                              fleet_color = "#B40F20",
                              groups_palette = "rpath_pal_light",#c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
                              text_size = 3,
                              max.overlaps = 50,
                              gradient = TRUE,
                              labels = TRUE,
                              cluster_method = "edge_betweenness",
                              low_tl_spread = 10)

ggsave("figures/EGOAfoodwebplot2.png", egoap , width= 16, height= 10)
