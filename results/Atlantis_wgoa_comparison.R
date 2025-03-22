# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV file
goaclim <- read.csv("C:/Users/biadias/Desktop/GOACLIM.csv") %>% 
  rename(metric=Metric..please.be.consistent.about.units.within.your.model.)
  
exclude_cols <- c("Year", "Model", "Species", "Scenario", "metric")

# Pivot only the remaining (assumed numeric) columns into a long format
goaclim_long <- goaclim %>%
  pivot_longer(
    cols = -all_of(exclude_cols),
    names_to = "Variable",
    values_to = "Value"
  )



goaclim_long_biomass <- goaclim_long %>% filter(metric=="Total Biomass")
goaclim_long_catch <- goaclim_long %>% filter(metric=="Catch")

ggplot(goaclim_long_biomass, aes(x = Year, y = Value, color = Model)) +
  geom_point(size = 3) +
  facet_wrap(~ Species) +
  labs(
    title = "Data Over the Years by Species",
    x = "Year",
    y = "Biomass (mt)"
  ) 

#-----------------------


# Filter data for the historical period: years 2000 to 2024
goaclim_hist_b <- goaclim_long_biomass %>%
  filter(Year >= 2000, Year <= 2024)

# Filter data for SSP projections: year 2100
goaclim_ssp_b <- goaclim_long_biomass %>%
  filter(Year == 2100)


# Filter data for the historical period: years 2000 to 2024
goaclim_hist_c <- goaclim_long_catch %>%
  filter(Year >= 2000, Year <= 2024)

# Filter data for SSP projections: year 2100
goaclim_ssp_c <- goaclim_long_catch %>%
  filter(Year == 2100)

# Create the plot:
biomass <- ggplot() +
  # Historical data as lines (colored by Model)
  geom_point(data = goaclim_hist_b,
            aes(x = Year, y = Value, color = Model, group = interaction(Model, Variable)),
            size = 3) +
  # SSP data as points (with different shapes for each Scenario)
  geom_point(data = goaclim_ssp_b,
             aes(x = Year, y = Value, color = Model, shape = Scenario),
             size = 3) +
  # Label SSP points with their Scenario names
  geom_text_repel(data = goaclim_ssp_b,
                  aes(x = Year, y = Value, label = Scenario),
                  size = 3,
                  nudge_y = 0.05,
                  show.legend = FALSE) +
  theme(axis.ticks.x = element_line(colour = "darkgrey", linewidth = 0.25),
        axis.line.x = element_line(colour = "darkgrey", linewidth = 0.25),
        axis.text.x = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.y = element_text(colour = "black", size = 7))+
  facet_wrap(~ Species) +
  #scale_x_continuous(breaks = c(seq(2000, 2024, by = 4), 2100)) +
  labs(title = "Historical Data (2000-2024) and SSP Projections for 2100",
       x = "Year",
       y = "Biomass (mt)",
       color = "Model",
       shape = "SSP Scenario") 
ggsave("figures/biom_goaclim.png", biomass, width=8, height=4.6)

# Create the plot:
catch <- ggplot() +
  # Historical data as lines (colored by Model)
  geom_point(data = goaclim_hist_c,
             aes(x = Year, y = Value, color = Model, group = interaction(Model, Variable)),
             size = 3) +
  # SSP data as points (with different shapes for each Scenario)
  geom_point(data = goaclim_ssp_c,
             aes(x = Year, y = Value, color = Model, shape = Scenario),
             size = 3) +
  # Label SSP points with their Scenario names
  geom_text_repel(data = goaclim_ssp_c,
                  aes(x = Year, y = Value, label = Scenario),
                  size = 3,
                  nudge_y = 0.05,
                  show.legend = FALSE) +
  theme(axis.ticks.x = element_line(colour = "darkgrey", linewidth = 0.25),
        axis.line.x = element_line(colour = "darkgrey", linewidth = 0.25),
        axis.text.x = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.y = element_text(colour = "black", size = 7))+
  facet_wrap(~ Species) +
  #scale_x_continuous(breaks = c(seq(2000, 2024, by = 4), 2100)) +
  labs(title = "Historical Data (2000-2024) and SSP Projections for 2100",
       x = "Year",
       y = "Catch (mt)",
       color = "Model",
       shape = "SSP Scenario") 

ggsave("figures/catch_goaclim.png", catch,width=8, height=4.6)
