#============================================================#
#=====> SLAMYS Time Series Estimation for 185 countries <=====#
#============================================================#

#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Changing the working directory
setwd("/Users/claudiareiter/Library/CloudStorage/Dropbox/Claudia/IIASA/Link4Skills/SLAMYS 2.0/Github")

#=> Installing and load necessary packages
# install_packages("pacman")
pacman::p_load(tidyverse, wcde, fastDummies, MASS, performance, stargazer)

#=> Read latest WIC back-projection data
wic <- read_csv("Input/WIC_reconstruction.csv")

#=> Define list of years and countries
all_years <- seq(1970, 2025, 5)
all_countries <- wic %>%
  filter(ccode < 900) %>%
  distinct(ccode) %>%
  pull()

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#=====> STEP 1: LOADING MEAN YEARS OF SCHOOLING AND CALCULATE VALUES BY BROAD AGE AND SEX
#-----------------------------------------------------------------------------------------------------------

# Read reconstructed MYS
mys_past <- read_csv("Input/WIC_reconstructed_mys.csv") %>%
  mutate(ccode = as.numeric(substr(region, 4, nchar(region)))) %>%
  dplyr::select(ccode, Time, sex, agest, mys) %>%
  left_join(wic %>% dplyr::select(ccode, Time, sex, agest, pop)) %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"),
         sex = ifelse(sex == "f", "Female", "Male")) %>%
  rename(country_code = ccode,
         year = Time) 

# aggregate MYS for 20-64, both genders
mys_past_total <- mys_past %>%
  group_by(country_code, year) %>%
  reframe(mys = weighted.mean(mys, pop)) %>%
  mutate(age_group = "20--64", sex = "Both") %>%
  dplyr::select(country_code, year, age_group, sex, mys)

# aggregate MYS for 20-64, by gender
mys_past_sex <- mys_past %>%
  group_by(country_code, year, sex) %>%
  reframe(mys = weighted.mean(mys, pop)) %>%
  mutate(age_group = "20--64") %>%
  dplyr::select(country_code, year, age_group, sex, mys)

# aggregate MYS for both genders, by broad age group
mys_past_age <- mys_past %>%
  group_by(country_code, year, age_group) %>%
  reframe(mys = weighted.mean(mys, pop)) %>%
  mutate(sex = "Both") %>%
  dplyr::select(country_code, year, age_group, sex, mys)

# aggregate MYS by broad age group and gender
mys_past_age_sex <- mys_past %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(mys = weighted.mean(mys, pop)) %>%
  dplyr::select(country_code, year, age_group, sex, mys)

# load MYS SSP2 projections from WCDE
mys_proj <- get_wcde(indicator = "bmys", version = "wcde-v3") %>%
  filter(age %in% c("20--39", "40--64", "20--64"),
         year > 2015) %>%
  rename(mys = bmys,
         age_group = age) %>%
  dplyr::select(country_code, year, age_group, sex, mys)

mys <- bind_rows(mys_past_total, 
                 mys_past_sex,
                 mys_past_age, 
                 mys_past_age_sex,
                 mys_proj) %>%
  left_join(wic_locations, by = c("country_code" = "isono")) %>%
  dplyr::select(country_code, name, region, continent, year, age_group, sex, mys) %>%
  filter(year %in% all_years,
         country_code %in% all_countries)

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 2: CALCULATING SLAMYS

# load estimated SAFs
saf <- read_csv("Output/saf_final_1970-2025.csv")

slamys <- mys %>%
  left_join(saf) %>%
  mutate(slamys = mys * saf) %>%
  mutate(age_group = factor(age_group, levels = c("20--39", "40--64", "20--64"))) %>%
  arrange(country_code, name, year, sex, age_group) %>%
  dplyr::select(country_code, name, year, age_group, sex, mys, saf, slamys, predicted, source)

# save final results
write_csv(slamys, "Output/slamys_1970-2025.csv")

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 3: PLOT SLAMYS RESULTS
#_______________________________________________________________________________

# define limits
ymin <- min(slamys$slamys, na.rm = TRUE)
ymax <- max(slamys$slamys, na.rm = TRUE)

pdf("Output/Figures/SLAMYS_by_country_age_sex.pdf", width = 10, height = 6)

for (ctry in unique(slamys$name)) {
  plot_data <- slamys %>% filter(name == ctry)
  
  p <- ggplot(plot_data, aes(x = year, y = slamys, color = sex)) +
    geom_line() +
    geom_point(data = subset(plot_data, predicted == 0), aes(color = source)) +
    facet_wrap(~ age_group) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = paste(ctry),
         x = "Year", y = "(Predicted) SLAMYS") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()
