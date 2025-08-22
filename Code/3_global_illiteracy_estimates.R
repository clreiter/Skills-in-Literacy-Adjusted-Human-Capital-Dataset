#============================================================#
#=====> ESTIMATING ILLITERACY RATES BY BROAD AGE AND SEX <=====#
#       (1970-2025)
#============================================================#

#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Changing the working directory
setwd("/Users/claudiareiter/Library/CloudStorage/Dropbox/Claudia/IIASA/Link4Skills/SLAMYS 2.0/Github")

#=> Installing and loading necessary packages
#install.packages("pacman")
pacman::p_load(tidyverse, wcde)

#=> Read WIC back-projections
wic <- read_csv("Input/WIC_reconstruction.csv")

#=> Define list of years and countries
all_years <- seq(1970, 2025, 5)
all_countries <- wic %>%
  filter(ccode < 900) %>%
  distinct(ccode) %>%
  pull()

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#=====> STEP 1: LOADING ILLITERACY DATA, PREDICT VALUES FOR YEARS 2020 AND 2025 AND APPLY AGE RATIO
#-----------------------------------------------------------------------------------------------------------

#=> load illiteracy data as estimated in Lutz et al. 2021
illiteracy <- read_csv("Input/illiterate_prop_1970-2015.csv") %>%
  rename(country_code = iso) %>%
  filter(country_code %in% wic_locations$isono)

#=> smooth data to remove unrealistic jumps
illiteracy_smoothed <- illiteracy %>%
  group_by(country_code) %>%
  group_modify(~ {
    model <- loess(illiterate_prop ~ year, data = .x, span = 2.5)
    .x$illiterate_prop_smoothed <- predict(model)
    .x
  }) %>%
  mutate(illiterate_prop_smoothed = ifelse(illiterate_prop_smoothed <0, 0, illiterate_prop_smoothed)) %>%
  dplyr::select(-illiterate_prop) %>%
  rename(illiterate_prop = illiterate_prop_smoothed)

#=> predict values for 2020 and 2025 using linear trend assumptions
future_years <- crossing(
  country_code = unique(illiteracy_smoothed$country_code),
  year = c(2020, 2025))

predictions <- illiteracy_smoothed %>%
  group_by(country_code) %>%
  summarise(model = list(lm(illiterate_prop ~ year, data = pick(everything()))), .groups = "drop") %>%
  left_join(future_years, by = "country_code") %>%
  mutate(predicted = map2_dbl(model, year, ~predict(.x, newdata = tibble(year = .y))),
         predicted = ifelse(predicted <0, 0, predicted)) %>% 
  dplyr::select(country_code, year, predicted) 

# combine historical estimates with projected estimates
illiteracy <- illiteracy_smoothed %>%
  bind_rows(predictions %>% rename(illiterate_prop = predicted)) %>%
  arrange(country_code, year)

#=> apply region-specific age and sex ratio from DHS/MICS illiteracy rates
wic_regions <- wic_locations %>%
  rename(country_code = isono) %>%
  dplyr::select(country_code, region)

#=> load MICS/DHS illiteracy rates and calculate region-specific age and sex ratios

# load data and calculate illiteracy rates
dhs_mics_illiteracy <- read_csv("Output/mics_dhs_literacy_rev.csv") %>%
  mutate(share_illiterate = 1 - share_literate) %>%
  dplyr::select(-share_literate)

# filter illiteracy rate of total working-age population
total_illiteracy <- dhs_mics_illiteracy %>%
  filter(age_group == "20--64", sex == "Both") %>%
  rename(total_share_illiterate = share_illiterate) %>%
  dplyr::select(country_code, year, total_share_illiterate) 

# calculate adjustment and define limits
age_sex_adjustment <- dhs_mics_illiteracy %>%
  filter(country_code %in% all_countries) %>%
  dplyr::select(country_code, year, age_group, sex, share_illiterate) %>%
  left_join(total_illiteracy) %>%
  left_join(wic_regions) %>%
  mutate(adjustment = share_illiterate/total_share_illiterate) %>%
  filter(!is.na(region)) %>%
  mutate(adjustment = ifelse(adjustment < 0.000000001, 1,
                             ifelse(is.infinite(adjustment), 1, 
                                    ifelse(is.nan(adjustment), 1,
                                           ifelse(is.na(adjustment), 1, adjustment)))),
         diff = share_illiterate - total_share_illiterate,
         adjustment = ifelse(abs(diff) < 0.01, 1, adjustment),
         adjustment = ifelse(adjustment > 2, 2, adjustment),
         adjustment = ifelse(adjustment < 0.5, 0.5, adjustment)) %>%
  group_by(sex, age_group, region) %>%
  reframe(adjustment = mean(adjustment)) 

# apply adjustment to the illiteracy rates for different sub-populations

illiteracy_total <- illiteracy %>%
  mutate(age_group = "20--64",
         sex = "Both")

# female, 20-64
illiteracy_female <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Female", age_group == "20--64")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "20--64",
         sex = "Female")

# male, 20-64
illiteracy_male <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Male", age_group == "20--64")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "20--64",
         sex = "Male")

# female, 20-39
illiteracy_female_young <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Female", age_group == "20--39")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "20--39",
         sex = "Female")

# male, 20-39
illiteracy_male_young <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Male", age_group == "20--39")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "20--39",
         sex = "Male")

# female, 40-64
illiteracy_female_old <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Female", age_group == "40--64")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "40--64",
         sex = "Female")

# male, 40-64
illiteracy_male_old <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Male", age_group == "40--64")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "40--64",
         sex = "Male")

# both, 20-39
illiteracy_young <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Both", age_group == "20--39")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "20--39",
         sex = "Both")

# both, 40-64
illiteracy_old <- illiteracy %>% 
  left_join(wic_regions) %>%
  left_join(age_sex_adjustment %>% filter(sex == "Both", age_group == "40--64")) %>%
  mutate(adjustment = ifelse(is.na(adjustment), 1, adjustment),
         illiterate_prop = illiterate_prop * adjustment) %>%
  dplyr::select(-adjustment, -region) %>%
  mutate(illiterate_prop = ifelse(illiterate_prop > 100, 100, illiterate_prop),
         age_group = "40--64",
         sex = "Both")

# combine illiteracy raes of all sub-populations
illiteracy <- bind_rows(illiteracy_total, 
                        illiteracy_female,
                        illiteracy_male,
                        illiteracy_female_old,
                        illiteracy_male_old,
                        illiteracy_female_young,
                        illiteracy_male_young,
                        illiteracy_young,
                        illiteracy_old) %>%
  filter(country_code %in% all_countries)

# save data
write_csv(illiteracy, "Output/illiteracy_by_age_sex_1970-2025.csv")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
