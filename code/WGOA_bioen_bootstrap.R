#------------------------------------------------------------------------------#
# AUTHORS: Bia Dias
# AFFILIATIONS: CICOES University of Washington / Alaska Fisheries Science Center
# E-MAIL: bia.dias@noaa.gov
#
# PURPOSE: Empirical, bootstrap-derived bioenergetics parameters (Topt, Tmax)
#   for WGOA Ecopath groups using bottom-temperature data from the BT survey.
#
# APPROACH:
#   Topt <- biomass-weighted mean bottom temperature across stations
#   Tmax <- maximum bottom temperature observed across stations
#
#   Two bootstrap variants are run per group:
#     1. WITH replacement    – standard bootstrap (n = n_stations, B iterations)
#     2. WITHOUT replacement – subsampling at `subsample_frac` of n_stations
#
# INPUT:  WGOA_source_data/station_summary_WGOA.csv   (from WGOA_guilds_temp.R)
#         WGOA_source_data/WGOA_bioen.csv             (existing parameter table)
# OUTPUT: WGOA_source_data/WGOA_bioen_bootstrap.csv
#------------------------------------------------------------------------------#

library(tidyverse)

set.seed(123)

# ── 0. Configuration ----------------------------------------------------------

n_boot         <- 5000   # bootstrap iterations
subsample_frac <- 0.80   # fraction of stations for without-replacement draws
ci_probs       <- c(0.025, 0.25, 0.50, 0.75, 0.975)  # quantiles to report

# ── 1. Load data --------------------------------------------------------------
species_weighted_thermal_envelopes_WGOA <- read.csv("WGOA_source_data/species_weighted_thermal_envelopes_WGOA.csv")
station_summary <- read.csv("WGOA_source_data/station_summary_WGOA.csv")
bioen_orig      <- read.csv("WGOA_source_data/WGOA_bioen.csv")

# Drop stations with no catch (can't weight temperatures meaningfully)
station_summary <- station_summary |>
  filter(station_catch > 0, !is.na(station_avg_bt))

# ── 2. race_group → Species mapping ------------------------------------------
# Juveniles share the adult thermal envelope (no juvenile-specific survey data).

bioen_map <- tribble(
  ~race_group,             ~Species,
  "Arrowtooth flounder",   "arrowtooth_flounder_juvenile",
  "Arrowtooth flounder",   "arrowtooth_flounder_adult",
  "Atka mackerel",         "atka_mackerel",
  "Flathead sole",         "flathead_sole_adult",
  "Octopus",               "octopus",
  "Pacific cod",           "pacific_cod_juvenile",
  "Pacific cod",           "pacific_cod_adult",
  "Pacific ocean perch",   "pacific_ocean_perch_adult",
  "Walleye pollock",       "walleye_pollock_juvenile",
  "Walleye pollock",       "walleye_pollock_adult",
  "Sablefish",             "sablefish_adult",
  "Pacific sleeper shark", "pacific_sleeper_shark",
  "Pacific herring",       "pacific_herring_adult",
  "Pacific halibut",       "pacific_halibut_juvenile",
  "Pacific halibut",       "pacific_halibut_adult",
  "Squid",                 "squid",
  "Pacific capelin",       "pacific_capelin",
  "Pacific sandlance",     "pacific_sandlance"
)

# ── 3. Bootstrap functions ----------------------------------------------------

# Weighted mean and max for one (re)sample of stations
bootstrap_stats <- function(df_group) {
  topt <- weighted.mean(df_group$station_avg_bt, w = df_group$station_catch)
  tmax <- max(df_group$station_avg_bt)
  c(Topt = topt, Tmax = tmax)
}

# Run B iterations for a single group's station data
run_bootstrap <- function(df_group, B, replace, frac = 1.0) {
  n <- nrow(df_group)
  size <- if (replace) n else max(2L, floor(n * frac))

  replicate(B, {
    idx <- sample.int(n, size = size, replace = replace)
    bootstrap_stats(df_group[idx, ])
  }) |>
    t() |>
    as.data.frame()
}

# Summarise a vector of bootstrap estimates at the desired quantiles
summarise_boot <- function(x, param, variant) {
  q <- quantile(x, probs = ci_probs, na.rm = TRUE)
  tibble(
    param   = param,
    variant = variant,
    mean    = mean(x, na.rm = TRUE),
    q2.5    = q["2.5%"],
    q25     = q["25%"],
    median  = q["50%"],
    q75     = q["75%"],
    q97.5   = q["97.5%"]
  )
}

# ── 4. Run bootstraps for every race_group in bioen_map ----------------------

groups <- unique(bioen_map$race_group)

boot_results <- map_dfr(groups, function(grp) {

  df_grp <- station_summary |> filter(race_group == grp)

  if (nrow(df_grp) < 3) {
    warning("Skipping '", grp, "': fewer than 3 usable stations.")
    return(NULL)
  }

  # -- with replacement ----
  boot_wr   <- run_bootstrap(df_grp, B = n_boot, replace = TRUE)
  # -- without replacement (subsampling) ----
  boot_wor  <- run_bootstrap(df_grp, B = n_boot, replace = FALSE, frac = subsample_frac)

  bind_rows(
    summarise_boot(boot_wr$Topt,  "Topt", "with_replacement"),
    summarise_boot(boot_wr$Tmax,  "Tmax", "with_replacement"),
    summarise_boot(boot_wor$Topt, "Topt", "without_replacement"),
    summarise_boot(boot_wor$Tmax, "Tmax", "without_replacement")
  ) |>
    mutate(race_group = grp, .before = 1)
})

# ── 5. Pivot to one row per race_group × variant, join Species names ----------

boot_wide <- boot_results |>
  pivot_wider(
    names_from  = param,
    values_from = c(mean, q2.5, q25, median, q75, q97.5),
    names_glue  = "{param}_{.value}"
  )

# Expand to individual Species via mapping table
boot_by_species <- bioen_map |>
  left_join(boot_wide, by = "race_group")

# ── 6. Build output table: new point estimates + CIs + original values --------

bioen_bootstrap <- bioen_orig |>
  left_join(
    boot_by_species |>
      # Use median as the new empirical point estimate (robust to skew)
      select(Species, race_group, variant,
             Topt_new = Topt_median, Topt_lo = Topt_q2.5, Topt_hi = Topt_q97.5,
             Tmax_new = Tmax_median, Tmax_lo = Tmax_q2.5, Tmax_hi = Tmax_q97.5),
    by = "Species"
  ) |>
  rename(Topt_orig = Topt, Tmax_orig = Tmax)

# ── 7. Save outputs -----------------------------------------------------------

write.csv(boot_results,     "WGOA_source_data/WGOA_bioen_bootstrap_distributions.csv", row.names = FALSE)
write.csv(bioen_bootstrap,  "WGOA_source_data/WGOA_bioen_bootstrap.csv",              row.names = FALSE)

cat("Done. Files written to WGOA_source_data/\n")
cat("  WGOA_bioen_bootstrap_distributions.csv  — full bootstrap summaries per group\n")
cat("  WGOA_bioen_bootstrap.csv                — new Topt/Tmax with 95% CI alongside original values\n")

# ── 8. Quick diagnostic plot --------------------------------------------------

boot_results |>
  filter(variant == "without_replacement") |>
  mutate(race_group = str_wrap(race_group, 20)) |>
  ggplot(aes(x = reorder(race_group, median), y = median,
             ymin = q2.5, ymax = q97.5, color = param)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  facet_wrap(~param, scales = "free_x") +
  labs(
    title = "Bootstrap estimates of Topt and Tmax (without replacement)",
    subtitle = paste0("Median ± 95% CI across ", n_boot, " bootstrap iterations"),
    x = NULL, y = "Bottom temperature (°C)"
  ) +
  theme_bw() +
  theme(legend.position = "none")


#############

empirical <- species_weighted_thermal_envelopes_WGOA |>
     select(race_group, Topt_empirical = weighted_avg_bot,
                                  Tmax_empirical = weighted_maxA_bot)
 
   comparison <- bioen_map |>
     left_join(empirical, by = "race_group") |>
     left_join(bioen_orig, by = "Species") |>
     rename(Topt_lit = Topt, Tmax_lit = Tmax) |>
     mutate(
         Topt_diff = Topt_empirical - Topt_lit,
         Tmax_diff = Tmax_empirical - Tmax_lit
       )
 
   comparison |>
     select(Species, race_group, Topt_lit, Topt_empirical, Topt_diff,
                                            Tmax_lit, Tmax_empirical, Tmax_diff) |>
    arrange(Topt_diff)


comparison_long <- comparison |>
     select(Species, Topt_lit, Topt_empirical, Tmax_lit, Tmax_empirical) |>
     pivot_longer(
         cols = -Species,
         names_to = c("param", "source"),
         names_sep = "_(?=lit|empirical)") |>
     mutate(
         source  = factor(source, levels = c("lit", "empirical"),
                                               labels = c("Literature", "WGOA empirical")),
         param   = factor(param, levels = c("Topt", "Tmax")),
         # Clean up labels - remove _adult/_juvenile suffix for display
           Species_label = str_replace_all(Species, "_", " ") |>
                           str_to_sentence() |>
                           str_remove(" adult| juvenile")
       )
 
   ggplot(comparison_long,
                   aes(x = value, y = reorder(Species_label, value),
                                    color = source, shape = source)) +
   geom_line(aes(group = Species_label), color = "grey70", linewidth = 0.6) +
   geom_point(size = 3) +
   facet_wrap(~param, scales = "free_x") +
   scale_color_manual(values = c("Literature" = "#E05C2A", "WGOA empirical" = "#2A7AB5")) +
   labs(
       title    = "Lit vs. WGOA empirical race survey temp parameters",
       #subtitle = "Lines connect paired estimates for each guild; all empirical values are colder",
       x        = "Temperature (°C)",
       y        = NULL,
       color    = NULL, shape = NULL
     ) +
   theme_bw(base_size = 11) +
   theme(legend.position = "bottom", panel.grid.minor = element_blank())
