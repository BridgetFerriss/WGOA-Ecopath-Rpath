#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#Quick anomaly generation code for primary produuction
#------------------------------------------------------------------------------#



# Load necessary library
library(dplyr)

# Load the dataset (assuming the dataset is in a CSV file)
pp <- read.csv("WGOA_source_data/ROMSOutputWGOA/Long_WGOA_prod_300.csv")

# Calculate the mean of CGOA_prod_Ph
mean_prod_wgoa <- mean(pp$prod_biom_month, na.rm = TRUE)

# Transform CGOA_prod_Ph to be centered at 1 by subtracting the mean and then scaling
pp_trans <- pp %>%
  mutate(wgoa_prod_centered_1 = ((prod_biom_month - mean_prod_wgoa) / mean_prod_wgoa) + 1)

write.csv(pp_trans, "WGOA_source_data/ROMSOutputWGOA/Long_WGOA_prod_300_anommaly.csv")


