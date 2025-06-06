# get_biomass_stratum
#' 
#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables a list of the cruisedat, haul, and catch tables from RACEBASE (all regions, all years)
#' @param speciescode five-digit numeric species code from the GAP species guides
#' @param survey_area a character code for the survey area, either "GOA" or "AI"
#' @param vulnerability the vulnerability of the species to the survey (defaults to 1)
#' @param strata a dataframe with the number and areas of the strata. Defaults to the strata table from RACEBASE.
#'
#' @return a dataframe containing the mean weighted cpue, 
#' @export
#'
#' @examples
#' x <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA")
#' head(x)
get_biomass_stratum <- function(racebase_tables = list(
                                  cruisedat = cruisedat,
                                  haul = haul,
                                  catch = catch
                                ),
                                #speciescode = 30060, # POP
                                #survey_area = "AI",
                                vulnerability = 1,
                                predator = "P.cod", #speciescode = 30060, # POP
                                model    = "EBS"    #survey_area = "AI"
                                #strata = switch(survey_area,
                                #  "GOA" = goa_strata,
                                #  "AI" = ai_strata)
                                ) {
  
  
  # KYA added
    speciescode = as.integer(pred_params[[predator]]$race)
    model_name    <- model    # renamed to avoid name confusion during lookup
    predator_name <- predator # renamed to avoid name confusion during lookup
  
  stratbins    <- strata_lookup %>% 
                  #mutate(stratum_bin = .data[[stratbin_col]]) %>%
                  filter(model==model_name) 
  #stratbins <- strata_lookup %>% filter(model==model_name)  
  At <- sum(stratbins$area)

  # Get cpue table
  x <- get_cpue(
    #racebase_tables = racebase_tables,
    model = model, #survey_area = survey_area,
    predator = predator #speciescode = speciescode
  )
#browser()
  # Total CPUE for species, year, stratum
  # no RACEBASE equivalent (building block of BIOMASS_STRATUM)
  x2 <- x %>%
    group_by(year, stratum_bin,species_name) %>%
    dplyr::summarize(
      haul_count = length(unique(hauljoin)), # number of total abundance hauls
      mean_wgt_cpue = mean(wgtcpue, na.rm = TRUE),
      var_wgt_cpue = ifelse(haul_count <= 1, NA, var(wgtcpue) / haul_count),
      mean_num_cpue = mean(numcpue, na.rm = TRUE),
      var_num_cpue = ifelse(haul_count <= 1, NA, var(numcpue) / haul_count),
      catch_count = length(which(catch_kg > 0)), # number of hauls with nonzero catch
    .groups="keep") %>%
    dplyr::ungroup() %>%
    select(
      year, stratum_bin,species_name, 
      haul_count, catch_count,
      mean_wgt_cpue, var_wgt_cpue,
      mean_num_cpue, var_num_cpue
    ) %>%
    add_column(.after = "stratum_bin", species_code = speciescode) 

  if (all(x2$catch_count <= x2$haul_count)) {
    cat(model_name,predator_name,"Number of hauls with positive catches is realistic.\n")
  }

  # RACEBASE equivalent table: BIOMASS_STRATUM
  biomass_stratum <- x2 %>%
    dplyr::left_join(stratbins, by = "stratum_bin") %>%
    rowwise() %>% # for applying ifelse() by row
    mutate(
      stratum_biomass = area * mean_wgt_cpue / vulnerability * 0.001, # kg --> mt
      stratum_ratio = area / At,
      biomass_var = area^2 * var_wgt_cpue * 1e-6, # kg--> mt, square it because it's variance
      qt_size = ifelse(haul_count <= 1, 0,
        qt(p = 0.025, df = haul_count - 1, lower.tail = F)
      ),
      min_biomass = stratum_biomass - qt_size * sqrt(biomass_var),
      max_biomass = stratum_biomass + qt_size * sqrt(biomass_var),
      stratum_pop = area * mean_num_cpue, # not sure why this calculation would be different from the stratum_biomass
      pop_var = area^2 * var_num_cpue,
      min_pop = stratum_pop - qt_size * sqrt(pop_var),
      max_pop = stratum_pop + qt_size * sqrt(pop_var)
    ) %>%
    mutate(
      min_biomass = ifelse(min_biomass < 0, 0, min_biomass),
      min_pop = ifelse(min_pop < 0, 0, min_pop)
    ) %>% # set low CI to zero if it's negative
    select(survey, year, stratum_bin, species_name, species_code, haul_count, catch_count, mean_wgt_cpue, var_wgt_cpue, mean_num_cpue, var_num_cpue, stratum_biomass, biomass_var, min_biomass, max_biomass, stratum_pop, pop_var, min_pop, max_pop, area, stratum_ratio) %>%
    add_column(.after = "year", model = model_name) %>%
        mutate(
      Ni = area / 0.01,
      fi = (Ni * (Ni - haul_count)) / haul_count
    )
  return(biomass_stratum)
}



#
#
#
#
# biomass_stratum_oracle <- read.csv("data/biomass_stratum_2021.csv") %>%
#   filter(YEAR == 2021) %>%
#   filter(SPECIES_CODE == 30060) %>%
#   select(-SPECIES_CODE) %>%
#   janitor::clean_names() %>%
#   arrange(stratum)
#
# biomass_stratum_R <- biomass_stratum %>%
#   select(-area, -Ni, -fi, -stratum_ratio) %>%
#   mutate(
#     stratum_pop = as.integer(stratum_pop),
#     min_pop = as.integer(min_pop)
#   ) %>%
#   as.data.frame()
#
# head(biomass_stratum_R)
# head(biomass_stratum_oracle)
#
# # Differences between BIOMASS_STRATUM
# diffdf::diffdf(biomass_stratum_R, biomass_stratum_oracle)
# # Difference between biomass_stratum produced by this code and the table in the GOA schema for 2021 for mean_wgt_cpue:
# biomass_stratum_R$mean_wgt_cpue - biomass_stratum_oracle$mean_wgt_cpue
# ######################### All good up to this point #######################
# ###########################################################################
# ###########################################################################
# ###########################################################################
# # Test out biomass_stratum, check with oracle version ---------------------
# pop_check <- read.csv("data/biomass_stratum_2021.csv") # GOA
# test_stratum <- pop_check %>%
#   filter(YEAR == 2021 & SPECIES_CODE == 30060) %>%
#   left_join(strata, by = c("STRATUM" = "stratum")) %>%
#   mutate(stratum_ratio = area / At)
#
# test_total <- test_stratum %>%
#   dplyr::group_by(YEAR) %>%
#   dplyr::summarize(
#     haul_count = sum(HAUL_COUNT),
#     catch_count = sum(CATCH_COUNT),
#     # The values below are not matching with the GOA schema
#     mean_wgt_cpue = sum(MEAN_WGT_CPUE * area, na.rm = TRUE) / At, # weighted avg cpue across strata
#     mean_num_cpue = sum(MEAN_NUM_CPUE * area, na.rm = TRUE) / At,
#     var_wgt_cpue = sum(stratum_ratio^2 * VAR_WGT_CPUE, na.rm = TRUE),
#     var_num_cpue = sum(stratum_ratio^2 * VAR_NUM_CPUE, na.rm = TRUE)
#   ) %>%
#   ungroup()
#
# test_total
# # OK it's definitely how I'm calculating it that's the issue, because the BIOMASS_STRATUM table behaves the same way when I do these calculations like this.
#
#
#
# # Check strata ------------------------------------------------------------
# strata_sqldev <- read.csv("data/goastrata_test.csv") %>% filter(SURVEY == "GOA")
# all_equal(goa_ai_strata %>% filter(SURVEY == "GOA"), strata_sqldev)
# # Stratum areas all equal; no issues there.
#
#
#
