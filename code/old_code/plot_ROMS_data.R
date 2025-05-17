wgoa1000cop <- Long_WGOA_B_summary_1000_v2_corrected %>%
  filter(varname== "Cop") %>% 
  filter(simulation == "ssp126")

wgoa300cop <- Long_WGOA_B_summary_300_v2_corrected %>%
  filter(varname== "Cop") %>% 
  filter(simulation == "ssp126")

ggplot() + 
  geom_line(data=wgoa1000, aes(x=year, y=mean_biomass), color='darkred', linewidth=1) + 
  geom_line(data=wgoa300, aes(x=year, y=mean_biomass), color='#A0A0A0', linewidth=1, alpha=0.7)


wgoa1000PhS <- Long_WGOA_B_summary_1000_v2_corrected %>%
  filter(varname== "PhS") %>% 
  filter(simulation == "ssp126")

wgoa300PhS <- Long_WGOA_B_summary_300_v2_corrected %>%
  filter(varname== "PhS") %>% 
  filter(simulation == "ssp126")



ggplot() + 
  geom_line(data=wgoa1000PhS, aes(x=year, y=mean_biomass), color='darkgreen', linewidth=1) + 
  geom_line(data=wgoa300PhS, aes(x=year, y=mean_biomass), color='#A0A0A0', linewidth=1, alpha=0.7)
