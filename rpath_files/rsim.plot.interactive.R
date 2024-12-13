# --- # --- # --- # --- # --- # --- # --- # --- # --- # 
# Interactive plots for Rsim plot using plotly:: package and tidyverse
# Author: Bia Dias
# Date: 12/13/2024
# contact: bia.dias@noaa.gov
# --- # --- # --- # --- # --- # --- # --- # --- # --- # 

library(plotly)
library(tidyverse)
library(colorspace)

rsim.plot.interactive <- function(Rsim.output, spname="all", indplot = FALSE) {
  rsim_name <- deparse(substitute(Rsim.output))
  mrg <- list(l = 50, r = 50,
              b = 50, t = 50,
              pad = 20)
  
  if ("all" %in% spname) {
    spname <- colnames(Rsim.output$out_Biomass)[2:ncol(Rsim.output$out_Biomass)]
  }
  # Extract and calculate rel. B
  if (!indplot) {
    # Multiple functional groups
    biomass <- Rsim.output$out_Biomass[, spname, drop = FALSE]
    start.bio <- biomass[1,]
    start.bio[start.bio == 0] <- 1
    rel.bio <- sweep(biomass, 2, start.bio, "/") # divide each column by its start value
  } else {
    # For a single functional group
    spnum <- which(Rsim.output$params$spname == spname)
    biomass <- Rsim.output$out_Biomass[, spnum]
    rel.bio <- biomass / biomass[1]
    rel.bio <- matrix(rel.bio, ncol = 1, dimnames = list(NULL, spname))
  }
  
  # Create a time vector
  time <- seq_len(nrow(rel.bio))
  df <- cbind(time, as.data.frame(rel.bio))
  df_long <- pivot_longer(df, cols = -time, names_to = "Species", values_to = "RelativeBiomass")
  
  # Plotly object
  n_species <- length(unique(df_long$Species))
  my_colors <- colorRampPalette(rainbow_hcl(n_species))(n_species)
  df_long$Species <- factor(df_long$Species, levels = spname)
  
  rsim.int.plotly <- plot_ly(data = df_long,
               x = ~time,
               y = ~RelativeBiomass,
               color = ~Species,
               colors = my_colors,
               type = 'scatter',
               mode = 'lines') %>%
    layout(title = paste("Relative Biomass Over Time:" , rsim_name),
           xaxis = list(title = "Months"),
           yaxis = list(title = "Relative Biomass"),
           margin=mrg)
  
  return(rsim.int.plotly)
}

