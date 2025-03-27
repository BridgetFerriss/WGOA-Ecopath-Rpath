source("code/WGOA_EGOA_comp.R")
source("code/ggraph_webplot_Rpath.R")
library(tidyverse)

webplot(w.bal)

p <- ggraph_webplot_Rpath(w.bal, eco.name = "Western Gulf of Alaska", h_spacing = 5, fleet_color = "#F11B00", text_size = 3)
#DO NOT RENDER IN R, it will take a century, but it saves very fast with ggsave
ggsave("C:/Users/biadias/Desktop/WGOAfoodwebplot.png", p , width= 16, height= 10)

