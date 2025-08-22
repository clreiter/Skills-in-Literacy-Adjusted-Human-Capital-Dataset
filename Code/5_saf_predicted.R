#==============================================================================================#
#=====> PREDICT SKILL-ADJUSTMENT FACTOR (SAF) FOR ALL COUNTRIES AND YEARS WITH MISSING DATA <=====#
#==============================================================================================#

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
#=====> STEP 1: LOADING QEI DATA, PREDICT VALUES FOR YEARS 1935-1940 AND 1995-2010 AND APPLY DIFFERENT TIME LAGS FOR DIFFERENT AGE GROUPS
#-----------------------------------------------------------------------------------------------------------

#=> load quality of education indcator as estimated in Lutz et al. 2021 

qei <- read.csv("Input/qei_1945_1990.csv", sep = ";") %>%
  rename(country_code = iso) %>%
  dplyr::select(country_code, year, hlo) %>%
  filter(!is.na(hlo))

#=> predict values for 1995-2010 using linear trend assumptions
future_years <- crossing(
  country_code = unique(qei$country_code),
  year = c(1995, 2000, 2005, 2010))

predictions <- qei %>%
  group_by(country_code) %>%
  summarise(model = list(lm(hlo ~ year, data = pick(everything()))), .groups = "drop") %>%
  left_join(future_years, by = "country_code") %>%
  mutate(predicted = map2_dbl(model, year, ~predict(.x, newdata = tibble(year = .y)))) %>% 
  dplyr::select(country_code, year, predicted) 

#use 1945 values for 1935 and 1940
qei_1935 <- qei %>%
  filter(year == 1945) %>%
  mutate(year = 1935)

qei_1940 <- qei %>%
  filter(year == 1945) %>%
  mutate(year = 1940)

# combine QEI estimates for all years
qei_total <- qei %>%
  bind_rows(predictions %>% rename(hlo = predicted)) %>%
  arrange(country_code, year) %>%
  mutate(year = year +25,
         age_group = "20--64") %>%
  filter(year < 2030)

# estimate QEI for 20-39-year-olds using time lag of 15 years
qei_young <- qei %>%
  bind_rows(predictions %>% rename(hlo = predicted)) %>%
  arrange(country_code, year) %>%
  mutate(age_group = "20--39",
         year = year + 15) %>%
  filter(year > 1965 & year < 2030)

# estimate QEI for 40-64-year-olds using time lag of 35 years
qei_old <- qei %>%
  bind_rows(predictions %>% rename(hlo = predicted)) %>%
  bind_rows(qei_1935) %>%
  bind_rows(qei_1940) %>%
  arrange(country_code, year) %>%
  mutate(age_group = "40--64",
         year = year + 35) %>%
  filter(year < 2030)

# combine all estimates
qei <- bind_rows(qei_total, 
                 qei_young, 
                 qei_old)

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 2: CALCULATE SHARE OF POPULATION WITH AT LEAST UPPER SECONDARY EDUCATION
#_______________________________________________________________________________

# by braod age group and sex
highLS_past_age_sex <- wic %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64")) %>%
  group_by(ccode, Time, age_group, sex) %>%
  summarise(
    total_pop = sum(pop),
    highLS_pop = sum(pop[edu %in% c("e5", "e6")]),
    highLS_share = (highLS_pop / total_pop)*100,
    .groups = "drop") %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male")) %>%
  rename(country_code = ccode,
         year = Time) %>%
  dplyr::select(country_code, year, age_group, sex, highLS_share) 

# by sex
highLS_past_sex <- wic %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  group_by(ccode, Time, sex) %>%
  summarise(
    total_pop = sum(pop),
    highLS_pop = sum(pop[edu %in% c("e5", "e6")]),
    highLS_share = (highLS_pop / total_pop)*100,
    .groups = "drop") %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male")) %>%
  rename(country_code = ccode,
         year = Time) %>%
  mutate(age_group = "20--64") %>%
  dplyr::select(country_code, year, age_group, sex, highLS_share) 

# by broad age group
highLS_past_age <- wic %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64")) %>%
  group_by(ccode, Time, age_group) %>%
  summarise(
    total_pop = sum(pop),
    highLS_pop = sum(pop[edu %in% c("e5", "e6")]),
    highLS_share = (highLS_pop / total_pop)*100,
    .groups = "drop") %>%
  mutate(sex = "Both") %>%
  rename(country_code = ccode,
         year = Time) %>%
  dplyr::select(country_code, year, age_group, sex, highLS_share) 

# total working-age population
highLS_past <- wic %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  group_by(ccode, Time) %>%
  summarise(
    total_pop = sum(pop),
    highLS_pop = sum(pop[edu %in% c("e5", "e6")]),
    highLS_share = (highLS_pop / total_pop)*100,
    .groups = "drop") %>%
  mutate(sex = "Both") %>%
  rename(country_code = ccode,
         year = Time) %>%
  mutate(age_group = "20--64") %>%
  dplyr::select(country_code, year, age_group, sex, highLS_share) 

# load projected data from WCDE
highLS_proj <- get_wcde(indicator = "bprop", version = "wcde-v3") %>%
  filter(education %in% c("Upper Secondary", "Post Secondary"), 
         age %in% c("20--39", "20--64", "40--64"),
         year > 2015) %>%
  group_by(country_code, year, age, sex) %>%
  reframe(highLS_share = sum(bprop)) %>%
  rename(age_group = age) %>%
  dplyr::select(country_code, year, age_group, sex, highLS_share)

# combine reconstructed and projected values
highLS <- bind_rows(highLS_past, highLS_past_age, highLS_past_age_sex, highLS_past_sex, highLS_proj)

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 3: CALCULATE OLD-AGE DEPENDENCY RATIO 
#_______________________________________________________________________________

odr_past <- wic %>%
  filter(agest > 15,
         Time < 2020) %>%
  mutate(dep = ifelse(agest < 65, "working", "old")) %>%
  group_by(ccode, Time) %>%
  summarise(working_pop = sum(pop[dep == "working"]),
            old_pop = sum(pop[dep == "old"]),
            odr = old_pop/working_pop,
            .groups = "drop") %>%
  rename(year = Time,
         country_code = ccode) %>%
  dplyr::select(country_code, year, odr)


# load projected data from WCDE
odr_proj <- get_wcde(indicator = "odr", version = "wcde-v3") %>%
  filter(year > 2015) %>%
  dplyr::select(country_code, year, odr)

# combine reconstructed and projected data
odr <- bind_rows(odr_past, odr_proj)

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 4: PREDICTION MODEL
#_______________________________________________________________________________

#---> Load all necessary variables

# SAF based on PIAAC/STEP
d1 <- read_csv("Output/saf_piaac_step.csv") %>%
  dplyr::select(country_code, year, age_group, sex, saf)

# SAF based on MICS/DHS
d2 <- read_csv("Output/saf_dhs_mics.csv") %>%
  dplyr::select(country_code, year, age_group, sex, saf_dhs_mics) %>%
  filter(country_code %in% all_countries,
         year %in% all_years)

# Proportion of population with at least upper secondary level education
d3 <- highLS %>%
  filter(country_code %in% all_countries,
         year %in% all_years)

# Proportion of illiterate population
d4 <- read_csv("Output/illiteracy_by_age_sex_1970-2025.csv") %>%
  filter(country_code %in% all_countries,
         year %in% all_years)

# WIC population old-age dependency ratio data
d5 <- odr %>%
  filter(country_code %in% all_countries,
         year %in% all_years)

# QEI scores 
d6 <- qei %>%
  filter(country_code %in% all_countries,
         year %in% all_years)

#---> PREDICTION MODEL 1 BY BROAD AGE GROUP AND SEX

# define data source
piaac_step_obs <- d1 %>% distinct(country_code, year) %>% mutate(source = "PIAAC/STEP")
dhs_mics_obs <- d2 %>% distinct(country_code, year) %>% 
  anti_join(piaac_step_obs) %>% 
  mutate(source = "DHS/MICS")
source <- bind_rows(piaac_step_obs, dhs_mics_obs)

# bringing everything together
r1 <- d3 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d4) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  dummy_cols(select_columns = "year") %>%
  filter(sex!= "Both",
         age_group!= "20--64",
         country_code %in% all_countries) %>%
  mutate(saf = ifelse(is.na(saf), saf_dhs_mics*0.8, saf)) %>%       # adjustment for DHS/MIC SAF
  left_join(source)

# estimation model for skills adjustment factor
model1 <- lm(log(saf) ~  age_group + sex + highLS_share +  odr + illiterate_prop + hlo +
               year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
               year_2000 + year_2005 + year_2010 + year_2015 + year_2020, data = r1)
summary(model1)
multicollinearity(model1)
# low multicollinearity

# Stepwise regression model
library(MASS)
step.model <- stepAIC(model1, direction = "both", 
                      scope = list(lower = ~ age_group + sex, upper = ~ age_group + sex + highLS_share +  odr + illiterate_prop + hlo +
                                     year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
                                     year_2000 + year_2005 + year_2010 + year_2015 + year_2020),
                      trace = FALSE)
summary(step.model)
# keep all variables except year_2020
lm1 <- lm(log(saf) ~  age_group + sex + highLS_share +  odr + illiterate_prop + hlo + 
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
            year_2000 + year_2005 + year_2010 + year_2015, data = r1)

# save regression output summary
# stargazer(lm1,
#           type = "html",
#           out = "Output/predictions.html",        
#           digits = 3,
#           dep.var.labels = "Skill-adjustment factor", 
#           single.row = T)

# predict values usint the regression models
lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

# defining final SAFs (predicted only if no empirical values available)
df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(saf_pred = exp(fit1), 
         saf_lwr = exp(lwr1), 
         saf_upr = exp(upr1)) %>%
  mutate(saf = ifelse(is.na(saf), saf_pred, saf),
         pred = ifelse(is.na(source), 1, 0)) %>% 
  arrange(country_code, year) %>%
  dplyr::select(country_code, year, age_group, sex, saf, pred, source)


#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 5: CALCULATE AGGREGATE SAF VALUES BASED ON POPULATION SIZE
#_______________________________________________________________________________

wic <- wic %>%
  rename(year = Time, 
         country_code = ccode)

# filter working-age population by broad age and sex
pop_age_sex_past <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))  %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male"))

# load projected population data from WCDE
pop_age_sex_proj <- get_wcde(indicator = "pop", pop_age = "all",  pop_sex = "all", version = "wcde-v3") %>%
  filter(age %in% c("20--24",
                    "25--29",
                    "30--34",
                    "35--39",
                    "40--44",
                    "45--49",
                    "50--54",
                    "55--59",
                    "60--64"), 
         year > 2015 & year < 2030,
         sex != "Both") %>%
  mutate(age = as.numeric(substr(age, 1, 2)),
         age_group = ifelse(age <40, "20--39", "40--64")) %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(pop = sum(pop)) %>%
  dplyr::select(country_code, year, age_group, sex, pop) 

# combine reconstructed and projected values
pop_age_sex <- bind_rows(pop_age_sex_past,
                         pop_age_sex_proj)


# aggregate SAF for 20-64, both sexes
df2 <- df1 %>% 
  left_join(pop_age_sex) %>%
  group_by(country_code, year, pred, source) %>%
  reframe(saf = weighted.mean(saf, pop)) %>%
  mutate(sex = "Both",
         age_group = "20--64")
  
# aggregate SAF for 20-64, by sex
df3 <- df1 %>% 
  left_join(pop_age_sex) %>%
  group_by(country_code, year, sex, pred, source) %>%
  reframe(saf = weighted.mean(saf, pop)) %>%
  mutate(age_group = "20--64")

# aggregate SAF for both sexes, by age group 
df4 <- df1 %>% 
  left_join(pop_age_sex) %>%
  group_by(country_code, year, age_group, pred, source) %>%
  reframe(saf = weighted.mean(saf, pop)) %>%
  mutate(sex = "Both")

# bring everything together
df5 <- bind_rows(df1,
                 df2, 
                 df3,
                 df4) %>%
  mutate(age_group = factor(age_group, levels = c("20--39", "40--64", "20--64"))) %>%
  arrange(country_code, year, sex, age_group) %>%
  rename(predicted = pred) 

# add country name 
wic_locations <- wic_locations
df6 <- df5 %>%
  left_join(wic_locations %>% dplyr::select(isono, name), by = c("country_code" = "isono")) %>%
  dplyr::select(country_code, name, year, age_group, sex, saf, predicted, source)

# save data
write_csv(df6, "Output/saf_final_1970-2025.csv")

#_______________________________________________________________________________
#_______________________________________________________________________________
#=====> STEP 6: PLOT SAF RESULTS
#_______________________________________________________________________________

# define limits
ymin <- min(df6$saf, na.rm = TRUE)
ymax <- max(df6$saf, na.rm = TRUE)

pdf("Output/Figures/SAF_by_country_age_sex.pdf", width = 10, height = 6)

for (ctry in unique(df6$name)) {
  plot_data <- df6 %>% filter(name == ctry)
  
  p <- ggplot(plot_data, aes(x = year, y = saf, color = sex)) +
    geom_line() +
    geom_point(data = subset(plot_data, predicted == 0), aes(color = source)) +
    facet_wrap(~ age_group) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = paste(ctry),
         x = "Year", y = "(Predicted) SAF") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()

