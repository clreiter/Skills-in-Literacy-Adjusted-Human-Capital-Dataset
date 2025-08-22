#==========================================================================#
#=====> ESTIMATING SKILL-ADJUSTMENT FACTOR (SAF) FROM DHS & MICS <=====#
#==========================================================================#


#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Changing the working directory
setwd("/Users/claudiareiter/Library/CloudStorage/Dropbox/Claudia/IIASA/Link4Skills/SLAMYS 2.0/Github")

#=> Installing and loading necessary packages
#install.packages("pacman")
pacman::p_load(tidyverse, wcde)

#=> Define list of 38 OECD countries (3-digit numeric ISO country code)
oecd_countries <- c(36, 40, 56, 124, 152, 170, 188, 203, 208, 233, 246, 250, 276, 
                    300, 348, 352,372, 376, 380, 392, 410, 428, 440, 442, 484, 528,
                    554, 578, 616, 620, 703, 705, 724, 752, 756, 792, 826, 840)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#=====> STEP 1: CALCULATE 2015 OECD AVERAGE LITERACY RATE
#-----------------------------------------------------------------------------------------------------------

# read illiteracy data and calculate literacy rates
literacy<-read_csv("Output/illiteracy_by_age_sex_1970-2025.csv") %>%
  filter(year == 2015) %>%
  mutate(literate_prop = 100-illiterate_prop)

literacy_oecd<- literacy %>% filter(country_code %in% oecd_countries) 

#=> read 2015 WIC population size from latest back-projections

wic <- read_csv("Input/WIC_reconstruction.csv") %>%
  rename(year = Time, 
         country_code = ccode)

# filter working-age population by sex
pop_sex <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  group_by(country_code, year, sex) %>%
  reframe(pop = sum(pop)) %>%
  mutate(age_group = "20--64",
         sex = ifelse(sex == "f", "Female", "Male"))

# filter total working-age population
pop <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  group_by(country_code, year) %>%
  reframe(pop = sum(pop)) %>%
  mutate(age_group = "20--64",
         sex = "Both")

# filter working-age population by broad age group
pop_age <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))  %>%
  group_by(country_code, year, age_group) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = "Both")

# filter working-age population by broad age group and sex
pop_age_sex <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))  %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male"))

# load projected population size from WCDE
pop_proj <- get_wcde(indicator = "bpop", version = "wcde-v3") %>%
  filter(year  > 2015,
         age %in% c("20--64", "20--39", "40--64")) %>%
  rename(age_group = age,
         pop = bpop) %>%
  dplyr::select(country_code, year, age_group, sex, pop)

# combine historical and projected values
pop <- bind_rows(pop, pop_sex, pop_age, pop_age_sex, pop_proj)

# filter 2015 population size
pop2015 <- pop %>% filter(year == 2015)

#=>calculate population-weighted OECD average
literacy_oecd_avg<- literacy_oecd %>% 
  left_join(pop2015) %>%
  group_by(age_group, sex) %>%
  reframe(oecd_weighted_mean = weighted.mean(literate_prop, pop))

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#=====> STEP 2: CALCULATE SKILL-ADJUSTMENT FACTOR (SAF) FROM DHS AND MICS DATA
#-----------------------------------------------------------------------------------------------------------

#=> calculate skills-adjustment factor for DHS and MICS countries by broad age and sex
dhs_mics_saf <- read_csv("Output/mics_dhs_literacy_rev.csv") %>%
  left_join(literacy_oecd_avg) %>%
  mutate(saf_dhs_mics = (share_literate*100)/oecd_weighted_mean) %>%
  dplyr::select(country_code, year, age_group, sex, saf_dhs_mics)

# save data
write_csv(dhs_mics_saf, "Output/saf_dhs_mics.csv")



