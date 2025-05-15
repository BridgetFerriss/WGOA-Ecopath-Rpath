source("code/WGOA_EGOA_comp.R")
source("code/ggraph_webplot_Rpath.R")
library(tidyverse)

webplot(w.bal)

#ab.bal <- Rpath::rpath(Rpath::AB.params, eco.name = "Anchovy Bay")
#ebs.bal <- Rpath::rpath(Rpath::Ecosense.EBS, eco.name = "Eastern Bering Sea")

p <- ggraph_webplot_Rpath(w.bal, eco.name = "Western Gulf of Alaska", h_spacing = 5, fleet_color = "#F11B00", text_size = 3)
#DO NOT RENDER IN R, it will take a century, but it saves very fast with ggsave
ggsave("figures/EBSfoodwebplot2.png", p , width= 16, height= 10)

