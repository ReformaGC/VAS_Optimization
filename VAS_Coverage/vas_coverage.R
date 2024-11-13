rm(list=ls())

# VAS COVERAGE SPATIAL ANALYSIS #
# DHS Survey Spatial Analysis for Optimizing Vitamin A Supplementation (VAS) Coverage in African Countries. #
# Stefano Balbo # 
# Note: some datasets are withheld under private policies. #

gc() 
setwd("C:\\Users\\stefa\\Documents\\Code\\VAS_Optimization\\VAS_Coverage\\"); getwd()

library(data.table)
library(haven)
library(labelled)
library(dplyr)
library(survey)
library(openxlsx)
library(sf)
library(mapview)
library(erer)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)
library(ggmap)
library(osmdata)


# Cargamos Children's Data - Children's Recode (KR) # Agrega country_name #
countries = list(
  "Benin" = "data_files/Benin/BJKR71DT/BJKR71FL.DTA",
  "Burkina_Faso" = "data_files/BF/BFKR81DT/BFKR81FL.DTA",
  "Cameroon" = "data_files/Cameroon/CMKR71DT/CMKR71FL.DTA",
  "CAR" = "data_files/CAR/CFKR31DT/CFKR31FL.DTA",
  "Cote_dIvoire" = "data_files/CdI/CIKR81DT/CIKR81FL.DTA",
  "DRC" = "data_files/DRC/CDKR61DT/CDKR61FL.DTA",
  "Sierra_Leone" = "data_files/SL/SLKR7ADT/SLKR7AFL.DTA",
  "Sao_Tome_and_P" = "data_files/STP/STKR51DT/STKR51FL.DTA",
  "Chad" = "data_files/Chad/TDKR71DT/TDKR71FL.DTA",
  "Congo" = "data_files/Congo/CGKR61DT/CGKR61FL.DTA",
  "Gambia" = "data_files/Gambia/GMKR81DT/GMKR81FL.DTA",
  "Gabon" = "data_files/Gabon/GAKR71DT/GAKR71FL.DTA",
  "Ghana" = "data_files/Ghana/GHKR8BDT/GHKR8BFL.DTA",
  "Guinea" = "data_files/Guinea/GNKR71DT/GNKR71FL.DTA",
  "Liberia" = "data_files/Liberia/LBKR7ADT/LBKR7AFL.DTA",
  "Mali" = "data_files/Mali/MLKR7ADT/MLKR7AFL.DTA",
  "Mauritania" = "data_files/Mauritania/MRKR71DT/MRKR71FL.DTA",
  "Niger" = "data_files/Niger/NIKR61DT/NIKR61FL.DTA",
  "Nigeria" = "data_files/Nigeria/NGKR7BDT/NGKR7BFL.DTA",
  "Senegal" = "data_files/Senegal/SNKR8RDT/SNKR8RFL.DTA",
  "Togo" = "data_files/Togo/TGKR61DT/TGKR61FL.DTA"
  )

for (country in names(countries)) {
  file_path = countries[[country]]
  
  if (file.exists(file_path)) {
    assign(country, read_dta(file_path))
    eval(parse(text = paste0(country, "$country = '", country, "'")))
    message(paste("Loaded data for", country))
  } else {
    warning(paste("File not found for", country, "at path", file_path))
  }
}
countries


# Filtramos key variables. DHS Survey post-2014.
skip_countries = c("CAR", "Congo", "Niger")
country_names = setdiff(names(countries), skip_countries)

for (country in country_names) {
  data = get(country)
  summarized_data = data %>%
    group_by(caseid) %>%
    summarise(
      midx,
      country = country,
      country_code = v000,
      sample_month = v006,
      sample_year = v007,
      cluster = v001,
      household = v002,
      area = v004,
      weight = v005,
      wt = (v005 / 1000000),
      psu = v021,
      sample_stratum = v022,
      sample_domain = v023,
      wealth = v190,
      region_id = v024,
      residence_id = v025,
      residence = ifelse(v025 == 1, "Urban", "Rural"),
      distance_facility = v467d, # 0 No problem 1 Big problem 2 Not a big problem
      child_age = hw1, # Children age in months
      child_vitA_recent = h33, # Child was taken to a medical facility for treatment of the fever and/or cough and received vitamin A (most recent)
      child_vitA_last6m = h34, # Received or not a vitamin A dose in form of an ampoule, a capsule or syrup in last 6 months
      birth_vitA_a2m = m54, # Received Vitamin A dose in first 2 months after delivery
      DPT1 = h3, # DPT (Diphteria, pertussis and tetanus vacccination) variables
      DPT2 = h5,
      DPT3 = h7,
      diarrhea = h11, # 0 no, 1 last 24hs, 2 last 2 weeks, 8/9 doesnt know
      months_ANCV = m13, # ANCV (Antenatal care visits) # Months pregnant at first antenatal visit 
      amount_ANCV = m14 # Antenatal visits during pregnancy
    )
  assign(country, summarized_data)
  message(paste("Summarization completed for", country))
}

{# Para CAR, Congo and Niger use overall average results; DHS survey too old.
  CAR = CAR %>%
    group_by(caseid) %>%
    summarise(midx,
              country = country); names(CAR) 
  
  Congo = Congo %>%
    group_by(caseid) %>%
    summarise(midx,
              country = country); names(Congo) 
  
  Niger = Niger %>%
    group_by(caseid) %>%
    summarise(midx,
              country = country); names(Niger) 
}


# Aplicamos SVY y generamos la dummy de VAS
for (country in country_names) {

  data = get(country)
  data = subset(data, child_age >= 6)
  data$residence_is_urban = ifelse(data$residence_id == 1, 1, 0)
  data$received_vas = ifelse(data$child_vitA_last6m == 1, 1, 0)

  design = svydesign(
    id = ~psu,
    strata = ~sample_stratum,
    weights = ~weight,
    data = data,
    nest = TRUE
  )
  
    assign(country, data)
  assign(paste0(country, "_design"), design)
    message(paste("Survey design setup completed for", country))
}


country_ids = c(
  "DRC" = 1, "Congo" = 2, "Chad" = 3, "Ghana" = 4, "Guinea" = 5, "Guinea-Bissau" = 6, 
  "Niger" = 7, "Mauritania" = 8, "Cote_dIvoire" = 9, "Benin" = 10, "Liberia" = 11,
  "Nigeria" = 12, "Sao_Tome_and_P" = 13, "Togo" = 14, "CAR" = 15, "Sierra_Leone" = 16,
  "Cabo_Verde" = 17, "Equatorial_Guinea" = 18, "Gabon" = 19, "Cameroon" = 20,
  "Burkina_Faso" = 21, "Gambia" = 22, "Mali" = 23, "Senegal" = 24
)


# Calculamos VAS Coverage subset Residence y Age
filtered_country_ids = country_ids[country_names]
options(survey.lonely.psu = "adjust")

calculate_vas_coverage = function(design, age_range) {
  subset_data = subset(design, child_age >= age_range[1] & child_age <= age_range[2])
  svymean(~received_vas, subset_data, na.rm = TRUE)[1] * 100
}

age_groups = list(
  "6-23 months" = c(6, 23),
  "24-59 months" = c(24, 59),
  "6-59 months" = c(6, 59)
)

results = data.frame(
  Country_Index = integer(),
  Country_Name = character(),
  Place_of_Residence = character(),
  VAS_Coverage_6_23_months = numeric(),
  VAS_Coverage_24_59_months = numeric(),
  VAS_Coverage_total_under_5y = numeric(),
  stringsAsFactors = FALSE
)

for (country in names(filtered_country_ids)) {
  country_id = filtered_country_ids[country]
  data = get(country)
  design = svydesign(id = ~psu, strata = ~sample_stratum, weights = ~weight, data = data, nest = TRUE)
  
  all_design = design  
  urban_design = subset(design, residence_is_urban == 1)  
  rural_design = subset(design, residence_is_urban == 0) 
  
  for (residence in c("All", "Urban", "Rural")) {
    if (residence == "All") {
      current_design = all_design
    } else if (residence == "Urban") {
      current_design = urban_design
    } else {
      current_design = rural_design
    }
    
    vas_coverages = sapply(age_groups, calculate_vas_coverage, design = current_design)
    
    results = rbind(results, data.frame(
      Country_Index = country_id,
      Country_Name = country,
      Place_of_Residence = residence,
      VAS_Coverage_6_23_months = vas_coverages["6-23 months.received_vas"],
      VAS_Coverage_24_59_months = vas_coverages["24-59 months.received_vas"],
      VAS_Coverage_total_under_5y = vas_coverages["6-59 months.received_vas"]
    ))
  }
  
  message(paste("VAS coverage calculations completed for", country))
}
print(results)

path = "results/DHS_VAS_Coverage_by_country.xlsx"
write.xlsx(results, file = path)


# # # # # # # # # # # # # # # # # # # # # # # 

rm(list=setdiff(ls(), c("DRC", "DRC_design", "Togo", "Togo_design", "Niger", "Liberia", "Liberia_design")))

# DRC # Togo # Niger # Liberia #

Niger = read_dta("data_files/Niger/NIKR61DT/NIKR61FL.DTA")
Niger$country = "Niger"

Niger = Niger %>%
  filter(hw1 >= 6) %>% 
  group_by(caseid) %>%
  summarise(
    midx,
    country = "Niger",
    country_code = v000,
    sample_month = v006,
    sample_year = v007,
    cluster = v001,
    household = v002,
    area = v004,
    weight = v005,
    wt = (v005 / 1000000),
    psu = v021,
    sample_stratum = v022,
    sample_domain = v023,
    wealth = v190,
    region_id = v024,
    residence_id = v025,
    residence = ifelse(v025 == 1, "Urban", "Rural"),
    residence_is_urban = ifelse(v025 == 1, 1, 0),
    distance_facility = v467d, 
    child_age = hw1, 
    child_vitA_recent = h33,  
    child_vitA_last6m = h34,  
    birth_vitA_a2m = m54,  
    DPT1 = h3,  
    DPT2 = h5,
    DPT3 = h7,
    diarrhea = h11, 
    months_ANCV = m13,
    amount_ANCV = m14  
  ) %>%
  mutate(
    received_vas = ifelse(child_vitA_last6m == 1, 1, 0)
  )

Niger_design = svydesign(
  id = ~psu,
  strata = ~sample_stratum,
  weights = ~weight,
  data = Niger,
  nest = TRUE
); message("Survey design setup completed for Niger")


# Distance to nearest facility #

# NOTE: To ensure respondent confidentiality, randomly displaced the GPS latitude/longitude positions.
# - Urban clusters are displaced up to 2 kilometers.
# - Rural clusters are displaced up to 5 kilometers, with 1% of the rural clusters displaced up to 10 kilometers.
# The displacement is restricted so that the points stay within the country and within the DHS survey region. 
# It introduces random error, which can substantively affect the results of analyses that look at small geographic areas.  
# Specifically, measuring direct distance from a GPS location to nearest facility is NOT accurate. This step is just a test.

DRC_hh_GIS = st_read("data_files/DRC/GPS/CDGE61FL_DHS_hous/CDGE61FL_fixed.shp")
DRC_fa_GIS = st_read("data_files/DRC/GPS/CDGE71FLSR_SPA_faci/CDGE71FLSR_fixed.shp")

Togo_hh_GIS = st_read("data_files/Togo/GPS/TGGE62FL_DHS_hous/TGGE62FL.shp")
Togo_fa_GIS = st_read("data_files/Togo/GPS/TogoHealthFacilities_SPA_faci/hotosm_tgo_health_facilities_points_shp.shp")

Niger_hh_GIS = st_read("data_files/Niger/GPS/NIGE61FL_DHS_hous/NIGE61FL.shp")
Niger_fa_GIS = st_read("data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.gpkg")

Liberia_hh_GIS = st_read("data_files/Liberia/GPS/LBGE7AFL_DHS_hous/LBGE7AFL2.shp")
Liberia_fa_GIS = st_read("data_files/Liberia/GPS/facilities/hotosm_lbr_health_facilities_points_shp2.shp")

Togo_fa_GIS = st_set_crs(Togo_fa_GIS, 4326)
Togo_hh_GIS = st_set_crs(Togo_hh_GIS, 4326)
Liberia_fa_GIS = st_set_crs(Liberia_fa_GIS, 4326)
Liberia_hh_GIS = st_set_crs(Liberia_hh_GIS, 4326)

calculate_nearest_distance = function(hh_GIS, fa_GIS) {
  distance_mts = st_distance(hh_GIS, fa_GIS)
  distance_nearest_facility_mts = apply(as.matrix(distance_mts), 1, FUN = min)
  distance_nearest_facility_km = distance_nearest_facility_mts / 1000
  
    hh_GIS$distance_nearest_facility_km = distance_nearest_facility_km
  return(hh_GIS)
}

DRC_hh_GIS = calculate_nearest_distance(DRC_hh_GIS, DRC_fa_GIS)
Togo_hh_GIS = calculate_nearest_distance(Togo_hh_GIS, Togo_fa_GIS)
Niger_hh_GIS = calculate_nearest_distance(Niger_hh_GIS, Niger_fa_GIS)
Liberia_hh_GIS = calculate_nearest_distance(Liberia_hh_GIS, Liberia_fa_GIS)

summary(DRC_hh_GIS$distance_nearest_facility_km)
summary(Togo_hh_GIS$distance_nearest_facility_km)
summary(Niger_hh_GIS$distance_nearest_facility_km)
summary(Liberia_hh_GIS$distance_nearest_facility_km)

mapview(DRC_hh_GIS)
mapview(Togo_hh_GIS)
mapview(Niger_hh_GIS)
mapview(Liberia_hh_GIS)

# Distance grouping by quantiles #
DRC_hh_GIS$distance_group = cut(DRC_hh_GIS$distance_nearest_facility_km, 
                                 breaks = quantile(DRC_hh_GIS$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                                 include.lowest = TRUE, labels = FALSE)
Togo_hh_GIS$distance_group = cut(Togo_hh_GIS$distance_nearest_facility_km, 
                                  breaks = quantile(Togo_hh_GIS$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                                  include.lowest = TRUE, labels = FALSE)
Niger_hh_GIS$distance_group = cut(Niger_hh_GIS$distance_nearest_facility_km, 
                                   breaks = quantile(Niger_hh_GIS$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                                   include.lowest = TRUE, labels = FALSE)
Liberia_hh_GIS$distance_group = cut(Liberia_hh_GIS$distance_nearest_facility_km, 
                                     breaks = quantile(Liberia_hh_GIS$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                                     include.lowest = TRUE, labels = FALSE)
table(DRC_hh_GIS$distance_group)
table(Togo_hh_GIS$distance_group)
table(Niger_hh_GIS$distance_group)
table(Liberia_hh_GIS$distance_group)


# Merge into Children dataset # Use Household Data - Household Recode (HR) as a bridge #

DRC_HH = read_dta("data_files/DRC/CDHR61DT/CDHR61FL.DTA") 
DRC_HH$country = "DRC"

Togo_HH = read_dta("data_files/Togo/TGHR61DT/TGHR61FL.DTA") 
Togo_HH$country = "Togo"

Niger_HH = read_dta("data_files/Niger/NIHR61DT/NIHR61FL.DTA") 
Niger_HH$country = "Niger"

Liberia_HH = read_dta("data_files/Liberia/LBHR7ADT/LBHR7AFL.DTA")
Liberia_HH$country = "Liberia"

DRC_HH = DRC_HH %>% 
  rename(cluster = hv001)
Togo_HH = Togo_HH %>% 
  rename(cluster = hv001)
Niger_HH = Niger_HH %>% 
  rename(cluster = hv001)
Liberia_HH = Liberia_HH %>% 
  rename(cluster = hv001)

merge_hh_geometries = function(hh_data, hh_GIS) {
  hh_GIS = hh_GIS %>% rename(cluster = DHSCLUST)
  merged_hh = left_join(hh_data, hh_GIS, by = "cluster")
  return(merged_hh)
}

DRC_HH = merge_hh_geometries(DRC_HH, DRC_hh_GIS)
Togo_HH = merge_hh_geometries(Togo_HH, Togo_hh_GIS)
Niger_HH = merge_hh_geometries(Niger_HH, Niger_hh_GIS)
Liberia_HH = merge_hh_geometries(Liberia_HH, Liberia_hh_GIS)

summary(DRC_HH$distance_nearest_facility_km)
summary(Togo_HH$distance_nearest_facility_km)
summary(Niger_HH$distance_nearest_facility_km)
summary(Liberia_HH$distance_nearest_facility_km)

table(DRC_HH$distance_group)
table(Togo_HH$distance_group)
table(Niger_HH$distance_group)
table(Liberia_HH$distance_group)


merge_children = function(children_data, hh_data) {
  hh_data = hh_data %>% rename(household = hv002)
  hh_data = hh_data %>% 
    select(cluster, household, distance_nearest_facility_km, distance_group)
  merged_data = left_join(children_data, hh_data, by = c("cluster", "household"))
  return(merged_data)
}

DRC = merge_children(DRC, DRC_HH)
Togo = merge_children(Togo, Togo_HH)
Niger = merge_children(Niger, Niger_HH)
Liberia = merge_children(Liberia, Liberia_HH)

summary(DRC$distance_nearest_facility_km)
summary(DRC$distance_group)
summary(Togo$distance_nearest_facility_km)
summary(Togo$distance_group)
summary(Niger$distance_nearest_facility_km)
summary(Niger$distance_group)
summary(Liberia$distance_nearest_facility_km)
summary(Liberia$distance_group)

{
path = "results/"

write.csv(DRC, paste(path,'DRC_ch_final.csv', sep = ''))
write_dta(DRC, paste0(path, "DRC_ch_final.dta"))

write.csv(Togo, paste(path,'Togo_ch_final.csv', sep = ''))
write_dta(Togo, paste0(path, "Togo_ch_final.dta"))

write.csv(Niger, paste(path,'Niger_ch_final.csv', sep = ''))
write_dta(Niger, paste0(path, "Niger_ch_final.dta"))

write.csv(Liberia, paste(path,'Liberia_ch_final.csv', sep = ''))
write_dta(Liberia, paste0(path, "Liberia_ch_final.dta"))
}

names(DRC)

merge_geometries = function(children_data, hh_GIS) {
  hh_GIS = hh_GIS %>% rename(cluster = DHSCLUST)
    merged_data = left_join(children_data, hh_GIS 
                             %>% select(cluster, geometry), by = "cluster")
    merged_data = st_as_sf(merged_data, sf_column_name = "geometry", crs = 4326)
  return(merged_data)
}

DRC_sf = merge_geometries(DRC, DRC_hh_GIS)
Togo_sf = merge_geometries(Togo, Togo_hh_GIS)
Niger_sf = merge_geometries(Niger, Niger_hh_GIS)
Liberia_sf = merge_geometries(Liberia, Liberia_hh_GIS)

summary(DRC_sf$geometry)
summary(Togo_sf$geometry)
summary(Niger_sf$geometry)
summary(Liberia_sf$geometry)

st_write(DRC_sf, paste0(path, 'DRC_ch_final.gpkg'), append = FALSE)
st_write(Togo_sf, paste0(path, 'Togo_ch_final.gpkg'), append = FALSE)
st_write(Niger_sf, paste0(path, 'Niger_ch_final.gpkg'), append = FALSE)
st_write(Liberia_sf, paste0(path, 'Liberia_ch_final.gpkg'), append = FALSE)

rm("DRC_hh_GIS", "Togo_hh_GIS", "Niger_hh_GIS", "Liberia_hh_GIS",
   "DRC_fa_GIS", "Togo_fa_GIS", "Niger_fa_GIS", "Liberia_fa_GIS",
   "DRC_HH", "Togo_HH", "Niger_HH", "Liberia_HH")

# DRC_sf = st_read("results/DRC_ch_final.gpkg")
# Togo_sf = st_read("results/Togo_ch_final.gpkg")
# Niger_sf = st_read("results/Niger_ch_final.gpkg")
# Liberia_sf = st_read("results/Liberia_ch_final.gpkg")


# # # NA Checking # # #
summary(DRC_sf$received_vas)
summary(Togo_sf$received_vas)
summary(Niger_sf$received_vas)
summary(Liberia_sf$received_vas)

mapview(DRC_sf)
mapview(Togo_sf)
mapview(Niger_sf)
mapview(Liberia_sf)

table(DRC_sf$wealth)
table(Togo_sf$wealth)
table(Niger_sf$wealth)
table(Liberia_sf$wealth)

summary(DRC_sf$distance_nearest_facility_km)
summary(Togo_sf$distance_nearest_facility_km)
summary(Niger_sf$distance_nearest_facility_km)
summary(Liberia_sf$distance_nearest_facility_km)


# # # # # # Probit Regression # # # # # # 

# # # DRC # # # 
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor

DRC_sf = DRC_sf %>%
  rename(vas = received_vas, 
         residence_binary = residence_is_urban) %>%
  mutate(
    vas = case_when(vas == 2 ~ 0, vas == 3 ~ 0, TRUE ~ as.integer(vas)),
    wealth_factor = factor(wealth, levels = c(1, 2, 3, 4, 5),
                           labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
  )

DRC_sf = DRC_sf %>% 
  filter(!is.na(vas), !is.na(distance_nearest_facility_km), !is.na(wealth_factor))

form1 = as.formula(vas ~ distance_nearest_facility_km + wealth_factor + distance_nearest_facility_km:wealth_factor)

probit_model = glm(form1, family = binomial(link = "probit"), data = DRC_sf, weights = wt, x = TRUE)
summary(probit_model)

marginal_effects = erer::maBina(probit_model, x.mean = TRUE, rev.dum = TRUE, digits = 3)
print(marginal_effects)

coefficients = as.data.frame(probit_model$coefficients)
coefficients = tibble::rownames_to_column(coefficients, var = "Variable")
colnames(coefficients)[2] = "Coefficient"

ma_effects = as.data.frame(marginal_effects$out)
ma_effects = tibble::rownames_to_column(ma_effects, var = "Variable")

path = "results/probit/"
dir.create(path, showWarnings = FALSE, recursive = TRUE)
write.xlsx(coefficients, paste0(path, "DRC_coefdistancewealth.xlsx"))
write.xlsx(ma_effects, paste0(path, "DRC_marginal_effects.xlsx"))


# # # Togo # # # 
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor

Togo_sf = Togo_sf %>%
  rename(vas = received_vas, 
         residence_binary = residence_is_urban) %>%
  mutate(
    vas = case_when(vas == 2 ~ 0, vas == 3 ~ 0, TRUE ~ as.integer(vas)),
    wealth_factor = factor(wealth, levels = c(1, 2, 3, 4, 5),
                           labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
  )

Togo_sf = Togo_sf %>% 
  filter(!is.na(vas), !is.na(distance_nearest_facility_km), !is.na(wealth_factor))

form1 = as.formula(vas ~ distance_nearest_facility_km + wealth_factor + distance_nearest_facility_km:wealth_factor)

probit_model = glm(form1, family = binomial(link = "probit"), data = Togo_sf, weights = wt, x = TRUE)
summary(probit_model)

marginal_effects = erer::maBina(probit_model, x.mean = TRUE, rev.dum = TRUE, digits = 3)
print(marginal_effects)

coefficients = as.data.frame(probit_model$coefficients)
coefficients = tibble::rownames_to_column(coefficients, var = "Variable")
colnames(coefficients)[2] = "Coefficient"

ma_effects = as.data.frame(marginal_effects$out)
ma_effects = tibble::rownames_to_column(ma_effects, var = "Variable")

path = "results/probit/"
dir.create(path, showWarnings = FALSE, recursive = TRUE)
write.xlsx(coefficients, paste0(path, "Togo_coefdistancewealth.xlsx"))
write.xlsx(ma_effects, paste0(path, "Togo_marginal_effects.xlsx"))


# # # Niger # # # 
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor

Niger_sf = Niger_sf %>%
  rename(vas = received_vas, 
         residence_binary = residence_is_urban) %>%
  mutate(
    vas = case_when(vas == 2 ~ 0, vas == 3 ~ 0, TRUE ~ as.integer(vas)),
    wealth_factor = factor(wealth, levels = c(1, 2, 3, 4, 5),
                           labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
  )

Niger_sf = Niger_sf %>% 
  filter(!is.na(vas), !is.na(distance_nearest_facility_km), !is.na(wealth_factor))

form1 = as.formula(vas ~ distance_nearest_facility_km + wealth_factor + distance_nearest_facility_km:wealth_factor)

probit_model = glm(form1, family = binomial(link = "probit"), data = Niger_sf, weights = wt, x = TRUE)
summary(probit_model)

marginal_effects = erer::maBina(probit_model, x.mean = TRUE, rev.dum = TRUE, digits = 3)
print(marginal_effects)

coefficients = as.data.frame(probit_model$coefficients)
coefficients = tibble::rownames_to_column(coefficients, var = "Variable")
colnames(coefficients)[2] = "Coefficient"

ma_effects = as.data.frame(marginal_effects$out)
ma_effects = tibble::rownames_to_column(ma_effects, var = "Variable")

path = "results/probit/"
dir.create(path, showWarnings = FALSE, recursive = TRUE)
write.xlsx(coefficients, paste0(path, "Niger_coefdistancewealth.xlsx"))
write.xlsx(ma_effects, paste0(path, "Niger_marginal_effects.xlsx"))


# # # Liberia # # # 
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor

Liberia_sf = Liberia_sf %>%
  rename(vas = received_vas, 
         residence_binary = residence_is_urban) %>%
  mutate(
    vas = case_when(vas == 2 ~ 0, vas == 3 ~ 0, TRUE ~ as.integer(vas)),
    wealth_factor = factor(wealth, levels = c(1, 2, 3, 4, 5),
                           labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
  )

Liberia_sf = Liberia_sf %>% 
  filter(!is.na(vas), !is.na(distance_nearest_facility_km), !is.na(wealth_factor))

form1 = as.formula(vas ~ distance_nearest_facility_km + wealth_factor + distance_nearest_facility_km:wealth_factor)

probit_model = glm(form1, family = binomial(link = "probit"), data = Liberia_sf, weights = wt, x = TRUE)
summary(probit_model)

marginal_effects = erer::maBina(probit_model, x.mean = TRUE, rev.dum = TRUE, digits = 3)
print(marginal_effects)

coefficients = as.data.frame(probit_model$coefficients)
coefficients = tibble::rownames_to_column(coefficients, var = "Variable")
colnames(coefficients)[2] = "Coefficient"

ma_effects = as.data.frame(marginal_effects$out)
ma_effects = tibble::rownames_to_column(ma_effects, var = "Variable")

path = "results/probit/"
dir.create(path, showWarnings = FALSE, recursive = TRUE)
write.xlsx(coefficients, paste0(path, "Liberia_coefdistancewealth.xlsx"))
write.xlsx(ma_effects, paste0(path, "Liberia_marginal_effects.xlsx"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # Maps # # #

rm(list=setdiff(ls(), c("DRC_sf", "Togo_sf", "Niger_sf", "Liberia_sf")))


# DRC
DRC_limits = st_read("data_files/DRC/GPS/congo_limits/congo_limits.shp") # country regions layer

names(DRC_sf)
names(DRC_limits)
table(DRC_sf$vas)
summary(DRC_sf$geom)
summary(DRC_limits$geometry)

DRC_sf = DRC_sf %>%
  mutate(
    child_age_dummy = ifelse(DRC_sf$child_age >= 24, 1, 0)
  ); table(DRC_sf$child_age_dummy)

cluster_summary_under24 = DRC_sf %>%
  filter(child_age_dummy == 0) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_above24 = DRC_sf %>%
  filter(child_age_dummy == 1) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels = scales::percent(breaks)

gg_map_vas_under24 = ggplot() +
  geom_sf(data = DRC_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("maps/VAS_Coverage_Under24_DRC.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


gg_map_vas_above24 = ggplot() +
  geom_sf(data = DRC_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("maps/VAS_Coverage_Above24_DRC.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale

cluster_summary = rbind(cluster_summary_above24, cluster_summary_under24)

gg_map_distance = ggplot() +
  geom_sf(data = DRC_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Distance to Nearest Health Facility by Cluster",
    subtitle = "To ensure confidentiality, DHS randomly displaced the GPS latitude/longitude positions. 
    It introduces random error, which can substantively affect the results of analyses. 
    This step is just a test.",
    caption = "Source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("maps/Distance_to_Facility_DRC.png", gg_map_distance, width = 12, height = 10, dpi = 300)


# Facility locations and type
DRC_fa_GIS = st_read("data_files/DRC/GPS/CDGE71FLSR_SPA_faci/CDGE71FLSR.shp") # Facility-GPS points
names(DRC_fa_GIS)
summary(DRC_fa_GIS$geometry)
table(DRC_fa_GIS$SPATYPEN) # type of facility (hospital or others)
table(DRC_fa_GIS$SPATYPEC) # dummy for type of facility (should group Hospitals vs Other)
table(DRC_fa_GIS$SPAMANGN) # ownership facility (public or private)
table(DRC_fa_GIS$SPAMANGC) # dummy for ownership facility (should group Public vs Private)

DRC_fa_GIS$FacilityType = ifelse(DRC_fa_GIS$SPATYPEC %in% c(1, 4, 5), 'Other', 'Hospital')
DRC_fa_GIS$Ownership = ifelse(DRC_fa_GIS$SPAMANGC == 1, 'Public', 'Private')

table(DRC_fa_GIS$FacilityType)
table(DRC_fa_GIS$Ownership)

gg_facility_map = ggplot() +
  geom_sf(data = DRC_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = DRC_fa_GIS, aes(color = Ownership, shape = FacilityType, geometry = geometry), size = 2) +
  scale_color_manual(values = c("Public" = "purple", "Private" = "#FF7F50")) +
  scale_shape_manual(values = c("Hospital" = 19, "Other" = 15)) +
  labs(title = "DRC Health Facilities",
       subtitle = "Facility Type and Ownership",
       caption = "Source: GADM | DHS Survey Datasets",
       color = "Ownership",
       shape = "Facility Type") +
  theme_minimal()
print(gg_facility_map)
ggsave("maps/Facility_classification_DRC.png", gg_facility_map, width = 12, height = 10, dpi = 300)


# Road lengths (KM), population and transport costs
# fuel_price = 1.1 # pump price for diesel fuel (US$ per liter) # we assume == U$D 1.10
# fuel_cost_per_km = (fuel_price / 12) # As a general rule, a gasoline car consumes one liter per 12 kilometers, which translates into a consumption of 4 to 12 liters of fuel per 100 kilometers.
# names(DRC_sf)
# summary(DRC_sf$geom)
# summary(DRC_limits$geometry)
# 
# DRC_sf_roads = st_read("data_files/DRC/GPS/congo_roads/congo_roads.shp"); names(DRC_sf_roads) # DRC_sf Roads DATA #
# table(DRC_sf_roads$NTLCLASS)
# DRC_sf_roads2 = subset(DRC_sf_roads, NTLCLASS == "DRC", drop = FALSE)
# DRC_sf_roads = DRC_sf_roads2; rm(DRC_sf_roads2)
# mapview(DRC_limits, color = "red") + mapview(DRC_sf_roads, color = "green")
# 
# summary(DRC_sf_roads$LENGTH_KM) 
# DRC_sf_popul = openxlsx::read.xlsx("data_files/DRC/DRC_population and density.xlsx")
# colnames(DRC_sf_popul) = c("Province", "Pop_density_per_km2_2019", "Area_km2", "Population_2019", "Old_region"); names(DRC_sf_popul)
# 
# table(DRC_sf$region_name)
# table(DRC_sf_limits$NAME_1)
# table(DRC_sf_popul$Old_region) # Old regions match with raster data
# table(DRC_sf_popul$Province) # New regions
# DRC_sf_roads = st_transform(DRC_sf_roads, st_crs(DRC_sf_limits))
# roads_by_province = st_join(DRC_sf_roads, DRC_sf_limits, join = st_intersects)
# table(roads_by_province$NAME_1)
# 
# province_road_lengths = roads_by_province %>%
#   group_by(NAME_1) %>%
#   dplyr::summarize(total_road_length_km = sum(LENGTH_KM, na.rm = TRUE)); summary(province_road_lengths$total_road_length_km)
# 
# table(province_road_lengths$NAME_1)
# table(DRC_sf_popul$Province)
# 
# province_data = left_join(province_road_lengths, DRC_sf_popul, by = c("NAME_1" = "Province")); province_data
# province_data = province_data %>%
#   mutate(per_capita_transport_cost = (total_road_length_km * fuel_cost_per_km) / Population_2019); province_data
# summary(province_data$total_road_length_km)
# 
# table(province_data$NAME_1)
# table(DRC_sf_limits$NAME_1)
# 
# province_data_no_geom = st_drop_geometry(province_data)
# 
# merged_data = left_join(DRC_sf_limits, province_data_no_geom, by = "NAME_1")
# merged_data = rename(merged_data, Province = NAME_1); names(merged_data)
# merged_data$total_road_length_km = round(merged_data$total_road_length_km, 0)
# summary(merged_data$total_road_length_km)
# 
# colors = c("darkgreen", "#1B9E77", "orange", "red", "darkred")
# values = rescale(c(0, 0.5, 1))
# gg_map_costs = ggplot(data = merged_data) +
#   geom_sf(aes(fill = per_capita_transport_cost), color = "white") +
#   geom_sf_text(aes(label = paste(Province, '\nPopulation:', Population_2019, '\nRoad Km:', round(total_road_length_km))), 
#                size = 3, check_overlap = TRUE) +
#   scale_fill_gradientn(colors = colors, 
#                        values = values, 
#                        name = "Per Capita\nTransport Cost", 
#                        labels = scales::comma, 
#                        limits = range(merged_data$per_capita_transport_cost, na.rm = TRUE)) +
#   labs(title = "Per Capita Transport Costs",
#        subtitle = "By province in the Democratic Republic of the DRC_sf",
#        caption = "Source: GADM | Global Roads Open Access Data Set (NASA)") +
#   theme_minimal()
# print(gg_map_costs)



rm(list=setdiff(ls(), c("DRC_sf", "Togo_sf", "Niger_sf", "Liberia_sf")))

# Togo

Togo_limits = st_read("data_files/Togo/GPS/togo_limits/togo_limits.shp") # country regions layer

names(Togo_sf)
names(Togo_limits)
table(Togo_sf$vas)
summary(Togo_sf$geom)
summary(Togo_limits$geometry)

Togo_sf = Togo_sf %>%
  mutate(
    child_age_dummy = ifelse(Togo_sf$child_age >= 24, 1, 0)
  ); table(Togo_sf$child_age_dummy)

cluster_summary_under24 = Togo_sf %>%
  filter(child_age_dummy == 0) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_above24 = Togo_sf %>%
  filter(child_age_dummy == 1) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels = scales::percent(breaks)

gg_map_vas_under24 = ggplot() +
  geom_sf(data = Togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("maps/VAS_Coverage_Under24_Togo.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


gg_map_vas_above24 = ggplot() +
  geom_sf(data = Togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("maps/VAS_Coverage_Above24_Togo.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale

cluster_summary = rbind(cluster_summary_above24, cluster_summary_under24)

gg_map_distance = ggplot() +
  geom_sf(data = Togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Distance to Nearest Health Facility by Cluster",
    subtitle = "To ensure confidentiality, DHS randomly displaced the GPS latitude/longitude positions. 
    It introduces random error, which can substantively affect the results of analyses. 
    This step is just a test.",
    caption = "Source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("maps/Distance_to_Facility_Togo.png", gg_map_distance, width = 12, height = 10, dpi = 300)


# Facility locations and type
Togo_fa_GIS = st_read("data_files/Togo/GPS/TogoHealthFacilities_SPA_faci/hotosm_tgo_health_facilities_points_shp.shp") # Facility-GPS points
head(Togo_fa_GIS)
Togo_fa_GIS = Togo_fa_GIS[Togo_fa_GIS$amenity %in% c("hospital", "clinic"), ]

Togo_fa_GIS$FacilityType = factor(Togo_fa_GIS$amenity, levels = c("clinic", "hospital"))
Togo_fa_GIS$Ownership = "Public"

st_crs(Togo_fa_GIS) = 4326

gg_facility_map = ggplot() +
  geom_sf(data = Togo_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = Togo_fa_GIS, aes(color = Ownership, shape = FacilityType), size = 2) +
  scale_color_manual(values = c("Public" = "purple")) +
  scale_shape_manual(values = c("clinic" = 15, "hospital" = 19)) +
  labs(
    title = "Togo Health Facilities",
    subtitle = "Facility Type and Ownership",
    caption = "Source: GADM | DHS Survey Datasets",
    color = "Ownership",
    shape = "Facility Type") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14))
print(gg_facility_map)
ggsave("maps/Facility_classification_Togo.png", gg_facility_map, width = 12, height = 10, dpi = 300)





rm(list=setdiff(ls(), c("DRC_sf", "Togo_sf", "Niger_sf", "Liberia_sf")))

# Niger

Niger_limits = st_read("data_files/Niger/GPS/niger_limits/gadm41_niger.shp")

names(Niger_sf)
names(Niger_limits)
table(Niger_sf$vas)
summary(Niger_sf$geom)
summary(Niger_limits$geometry)

Niger_sf = Niger_sf %>%
  mutate(
    child_age_dummy = ifelse(Niger_sf$child_age >= 24, 1, 0)
  ); table(Niger_sf$child_age_dummy)

cluster_summary_under24 = Niger_sf %>%
  filter(child_age_dummy == 0) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_above24 = Niger_sf %>%
  filter(child_age_dummy == 1) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels = scales::percent(breaks)

gg_map_vas_under24 = ggplot() +
  geom_sf(data = Niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("maps/VAS_Coverage_Under24_Niger.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


gg_map_vas_above24 = ggplot() +
  geom_sf(data = Niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("maps/VAS_Coverage_Above24_Niger.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale

cluster_summary = rbind(cluster_summary_above24, cluster_summary_under24)

gg_map_distance = ggplot() +
  geom_sf(data = Niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Distance to Nearest Health Facility by Cluster",
    subtitle = "To ensure confidentiality, DHS randomly displaced the GPS latitude/longitude positions. 
    It introduces random error, which can substantively affect the results of analyses. 
    This step is just a test.",
    caption = "Source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("maps/Distance_to_Facility_Niger.png", gg_map_distance, width = 12, height = 10, dpi = 300)


# Facility locations and type
Niger_fa_GIS = st_read("data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.gpkg") # Facility-GPS points
head(Niger_fa_GIS)

Niger_fa_GIS$FacilityType = "CSI"
Niger_fa_GIS$Ownership = "CSI"

st_crs(Niger_fa_GIS) = 4326

gg_facility_map = ggplot() +
  geom_sf(data = Niger_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = Niger_fa_GIS, aes(color = FacilityType), size = 2) +
  scale_color_manual(values = c("CSI" = "purple")) +
  labs(
    title = "Niger Health Facilities",
    subtitle = "Facility Type and Ownership",
    caption = "Source: GADM | DHS Survey Datasets",
    color = "FacilityType") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14))
print(gg_facility_map)
ggsave("maps/Facility_classification_Niger.png", gg_facility_map, width = 12, height = 10, dpi = 300)




rm(list=setdiff(ls(), c("DRC_sf", "Togo_sf", "Niger_sf", "Liberia_sf")))

# Liberia

Liberia_limits = st_read("data_files/Liberia/GPS/liberia_limits/gadm41_liberia.shp") # country regions layer

names(Liberia_sf)
names(Liberia_limits)
table(Liberia_sf$vas)
summary(Liberia_sf$geom)
summary(Liberia_limits$geometry)

Liberia_sf = Liberia_sf %>%
  mutate(
    child_age_dummy = ifelse(Liberia_sf$child_age >= 24, 1, 0)
  ); table(Liberia_sf$child_age_dummy)

cluster_summary_under24 = Liberia_sf %>%
  filter(child_age_dummy == 0) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_above24 = Liberia_sf %>%
  filter(child_age_dummy == 1) %>%
  group_by(cluster) %>%
  summarise(
    vas_coverage = mean(vas),
    distance = mean(distance_nearest_facility_km),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels = scales::percent(breaks)

gg_map_vas_under24 = ggplot() +
  geom_sf(data = Liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("maps/VAS_Coverage_Under24_Liberia.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


gg_map_vas_above24 = ggplot() +
  geom_sf(data = Liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS_Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("maps/VAS_Coverage_Above24_Liberia.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale

cluster_summary = rbind(cluster_summary_above24, cluster_summary_under24)

gg_map_distance = ggplot() +
  geom_sf(data = Liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Distance to Nearest Health Facility by Cluster",
    subtitle = "To ensure confidentiality, DHS randomly displaced the GPS latitude/longitude positions. 
    It introduces random error, which can substantively affect the results of analyses. 
    This step is just a test.",
    caption = "Source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("maps/Distance_to_Facility_Liberia.png", gg_map_distance, width = 12, height = 10, dpi = 300)


# Facility locations and type
Liberia_fa_GIS = st_read("data_files/Liberia/GPS/facilities/hotosm_lbr_health_facilities_points_shp2.shp") # Facility-GPS points
head(Liberia_fa_GIS)

Liberia_fa_GIS$amenity = "CSI"
table(Liberia_fa_GIS$amenity)

Liberia_fa_GIS$FacilityType = Liberia_fa_GIS$amenity
Liberia_fa_GIS$Ownership = "Public"; table(Liberia_fa_GIS$Ownership)
table(Liberia_fa_GIS$FacilityType)
table(Liberia_fa_GIS$Ownership)

st_crs(Liberia_fa_GIS) = 4326
summary(Liberia_fa_GIS$geometry)

gg_facility_map = ggplot() +
  geom_sf(data = Liberia_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = Liberia_fa_GIS, aes(color = FacilityType), size = 2) +
  scale_color_manual(values = c("CSI" = "purple")) +
  labs(
    title = "Liberia Health Facilities",
    subtitle = "Facility Type and Ownership",
    caption = "Source: GADM | DHS Survey Datasets",
    color = "FacilityType") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14))
print(gg_facility_map)
ggsave("maps/Facility_classification_Liberia.png", gg_facility_map, width = 12, height = 10, dpi = 300)

































