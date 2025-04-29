#------------------------------------------------------------------------------#
#AUTHORS: Matt Callahan - NOAA Affiliate
#REVIEWED BY: Bia Dias
#AFFILIATIONS: AKFIN - Pacific States Marine Fisheries Commission
#E-MAIL OF CORRESPONDENCE AUTHOR: matt.callahan@noaa.gov, bia.dias@noaa.gov
#
# script to pull catch data from the AKFIN data
#------------------------------------------------------------------------------#


library(odbc)
library(keyring)
library(dplyr)

options(scipen = 999)

con <- dbConnect(
  odbc::odbc(),
  "akfin",
  UID = key_list("akfin_oracle_db")$username,
  PWD = keyring::key_get(
    "akfin_oracle_db",
    keyring::key_list("akfin_oracle_db")$username
  )
)

# specify last year of data
end_year <- 2023

# first pull wgoa catch with year, species group, species, retained_or_discarded, and gear
wgoa_catch1 <- dbGetQuery(
  con,
  paste0(
    "with dat as (select
year,
species_group_code,
species_group_name,
species_name,
sum(weight_posted) catch_mt,
retained_or_discarded,
agency_gear_code,
CASE WHEN COUNT(DISTINCT vessel_id) <= 2 OR COUNT(DISTINCT processor_permit_id) <= 2 THEN 1 ELSE 0 END as conf_flag
from council.comprehensive_blend_ca
where reporting_area_code in ('610', '620', '630')
and year <= ",
    end_year,
    "
group by year,
species_group_code,
species_group_name,
species_name,
retained_or_discarded,
agency_gear_code)
select year,
species_group_code,
species_group_name,
species_name,
retained_or_discarded,
agency_gear_code,
conf_flag,
CASE WHEN conf_flag = 0 then catch_mt else NULL END as catch_mt
from dat
order by year"
  )
) %>%
  rename_with(tolower)

# There are more confidential data than may be desirable so here are some more aggregated outputs
# wgoa catch with year, species group, species, and retained_or_discarded
wgoa_catch2 <- dbGetQuery(
  con,
  paste0(
    "with dat as (select
year,
species_group_code,
species_group_name,
species_name,
sum(weight_posted) catch_mt,
retained_or_discarded,
CASE WHEN COUNT(DISTINCT vessel_id) <= 2 OR COUNT(DISTINCT processor_permit_id) <= 2 THEN 1 ELSE 0 END as conf_flag
from council.comprehensive_blend_ca
where reporting_area_code in ('610', '620', '630')
and year <= ",
    end_year,
    "
group by year,
species_group_code,
species_group_name,
species_name,
retained_or_discarded)
select year,
species_group_code,
species_group_name,
species_name,
retained_or_discarded,
conf_flag,
CASE WHEN conf_flag = 0 then catch_mt else NULL END as catch_mt
from dat
order by year"
  )
) %>%
  rename_with(tolower)

# wgoa catch with year, species group,  and species
wgoa_catch3 <- dbGetQuery(
  con,
  paste0(
    "with dat as (select
year,
species_group_code,
species_group_name,
species_name,
sum(weight_posted) catch_mt,
CASE WHEN COUNT(DISTINCT vessel_id) <= 2 OR COUNT(DISTINCT processor_permit_id) <= 2 THEN 1 ELSE 0 END as conf_flag
from council.comprehensive_blend_ca
where reporting_area_code in ('610', '620', '630')
and year <= ",
    end_year,
    "
group by year,
species_group_code,
species_group_name,
species_name)
select year,
species_group_code,
species_group_name,
species_name,
conf_flag,
CASE WHEN conf_flag = 0 then catch_mt else NULL END as catch_mt
from dat
order by year"
  )
) %>%
  rename_with(tolower)

# wgoa catch with year and species group
wgoa_catch4 <- dbGetQuery(
  con,
  paste0(
    "with dat as (select
year,
species_group_code,
species_group_name,
sum(weight_posted) catch_mt,
CASE WHEN COUNT(DISTINCT vessel_id) <= 2 OR COUNT(DISTINCT processor_permit_id) <= 2 THEN 1 ELSE 0 END as conf_flag
from council.comprehensive_blend_ca
where reporting_area_code in ('610', '620', '630')
and year <= ",
    end_year,
    "
group by year,
species_group_code,
species_group_name)
select year,
species_group_code,
species_group_name,
conf_flag,
CASE WHEN conf_flag = 0 then catch_mt else NULL END as catch_mt
from dat
order by year"
  )
) %>%
  rename_with(tolower)

# FLAG ####
# # wgoa state catch 
# ADFG_I_SPECIES_CODE
# AKFIN_SPECIES_CODE
wgoa_catch5 <- dbGetQuery(
  con,
  paste0(
    "with dat as (select
             akfin_year,
             species_name,
             sum(cfec_whole_pounds) catch_lb,
             fmp_gear,
             harvest_description,
             CASE WHEN COUNT(DISTINCT vessel_id) <= 2 OR COUNT(DISTINCT processor_permit_id) <= 2 THEN 1 ELSE 0 END as conf_flag
             from council.comprehensive_ft
             where reporting_area_code in ('610', '620', '630')
             and akfin_year <= ",
    end_year,
    "
group by akfin_year,
species_name,
fmp_gear,
harvest_description)
select akfin_year,
species_name,
fmp_gear,
harvest_description,
conf_flag,
CASE WHEN conf_flag = 0 then catch_lb else NULL END as catch_lb
from dat
order by akfin_year"
  )
) %>%
  rename_with(tolower)


# species group lookup table
species_group_content <- dbGetQuery(con, paste0("select * from akr.species_group_content")) %>%
  rename_with(tolower)

agency_species <-  dbGetQuery(con,
                              paste0("select * from akr.agency_specie where agency = 'AKR'")) %>%
  rename_with(tolower)


#convert state catch from pounds to metric tons
wgoa_catch5 <- wgoa_catch5 %>%mutate(catch_mt= catch_lb*0.000453592)

# save
# sorry for the long species names, you can rename whichever of these you use to whatever you want.
write.csv(
  wgoa_catch1,
  paste0(
    "data/",
    end_year,
    "/wgoa_catch_year_group_spec_ret_gear.csv"
  ),
  row.names = FALSE
)
write.csv(
  wgoa_catch2,
  paste0("data/", end_year, "/wgoa_catch_year_group_spec_ret.csv"),
  row.names = FALSE
)
write.csv(
  wgoa_catch3,
  paste0("data/", end_year, "/wgoa_catch_year_group_spec.csv"),
  row.names = FALSE
)
write.csv(wgoa_catch4,
          paste0("data/", end_year, "/wgoa_catch_year_group.csv"),
          row.names = FALSE)

write.csv(wgoa_catch5,
  paste0("data/",end_year,"/wgoa_catch_year_group_spec_ret_gear_state.csv"),
  row.names = FALSE)

write.csv(species_group_content,
          "data/metadata/akr_species_group_content.csv",
          row.names = FALSE)
write.csv(agency_species,
          "data/metadata/akr_species_code_lookup.csv",
          row.names = FALSE)
