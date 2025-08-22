#=======================================================#
#=====> PREDICTION OF MALE LITERACY RATES <=====#
#       (for female-only MICS and DHS waves)
#=======================================================#

#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------
#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Changing the working directory
setwd("/Users/claudiareiter/Library/CloudStorage/Dropbox/Claudia/IIASA/Link4Skills/SLAMYS 2.0/Github")

#=> Installing and loading necessary packages
#install.packages("pacman")
pacman::p_load(tidyverse, readxl, wcde, stargazer)

# load country-region info from WIC
geo <- wcde::wic_locations

#=======================================================#
#=====> STEP 1: READ DATA
#-----------------------------------------------------------------------------------------------------------

# read data from WIC back-projections
wic <- read_csv("Input/WIC_reconstruction.csv")

# read literacy data from MICS and DHS
literacy <- read_xlsx('Input/mics_dhs_literacy.xlsx')

#=======================================================#
#=====> STEP 2: DATA TRANSFORMATION FOR PREDICTIONS
#-----------------------------------------------------------------------------------------------------------

literacy_rev <- literacy %>% 
  filter(age_group == "Total",
         !is.na(lit),
         Sex!="both") %>%
  dplyr::select(Country, year, Sex, lit, iso_code) %>%
  rename(name = Country, 
         sex = Sex,
         share_literate = lit,
         isono = iso_code) %>%
  mutate(name = ifelse(name == "Central Africa Republic", "Central African Republic", name),
         name = ifelse(name == "Kosovo under UNSC res.1244", "Kosovo", name)) %>%
  distinct() %>%
  mutate(sex = ifelse(sex == "F", "Female", "Male"))  %>%
  group_by(isono, name, year, sex) %>%
  reframe(share_literate = mean(share_literate))

# bring to wide format   
literacy_wide <- literacy_rev %>%
  group_by(isono, name, year, sex) %>%
  reframe(share_literate = mean(share_literate)) %>%
  pivot_wider(names_from = sex, values_from = share_literate) %>%
  left_join(geo %>% dplyr::select(isono, region)) 

# load WIC reconstruction data on educational attainment (share with no education)
share_no_edu <- wic %>% 
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  group_by(ccode, Time, sex) %>%
  summarise(
    total_pop = sum(pop),
    no_edu_pop = sum(pop[edu == "e1"]),
    share_no_edu = (no_edu_pop / total_pop) * 100
  ) %>%
  ungroup()

# calculate gender gap in share of people with no education
ggapedu <- share_no_edu %>%
  dplyr::select(ccode, Time, sex, share_no_edu) %>%
  pivot_wider(names_from = sex, values_from = share_no_edu) %>%
  mutate(ggapedu = m - f) %>%
  rename(isono = ccode,
         year = Time) %>%
  dplyr::select(isono, year, ggapedu)
  
# load gender gap in share of people with no education from WIC projections  
ggapedu_proj <- get_wcde(indicator = "bprop", version = "wcde-v3") %>%
  filter(education == "No Education",
         age == "20--64",
         sex != "Both") %>%
  dplyr::select(country_code, year, age, sex, bprop) %>%
  rename(isono = country_code,
         age_group = age) %>%
  pivot_wider(names_from = sex, values_from = bprop) %>%
  mutate(ggapedu = Male - Female) %>%
  dplyr::select(isono, year, ggapedu) 

# combine reconstruction and projections
ggapedu <- ggapedu %>% bind_rows(ggapedu_proj)


# combine variables
literacy_wide <- literacy_wide %>% left_join(ggapedu)

#=======================================================#
#=====> STEP 3: MALE PREDICTIONS FOR TOTAL WORKING-AGE POPULATION
#-----------------------------------------------------------------------------------------------------------
# drop NA values
literacy_full <- literacy_wide %>% drop_na()

# prediction model
model1 <- lm(Male ~ Female + region + ggapedu, data = literacy_full)
summary(model1)

# use model 1 to predict male values
predicted_literacy <- literacy_wide %>% filter(is.na(Male))
predicted_literacy$Male_estimated <- predict(model1, newdata = predicted_literacy)

# Combine predicted and original male values into a final column
literacy_wide <- literacy_wide %>%
  mutate(Male_predicted = predict(model1, newdata = .),
         Male_final = ifelse(is.na(Male),
                        Male_predicted,
                        Male)) %>%
  mutate(Male_final = ifelse(Male_final > 1, 1, Male_final))

# plot predicted vs observed values
#ggplot(literacy_wide, aes(x = Male_predicted, y = Male)) + 
#  geom_point() + 
#  geom_abline()

# bring to long format
literacy_long <- literacy_wide %>%
  dplyr::select(isono, name, year,  Male_final) %>%
  pivot_longer(cols = 4, names_to = "sex", values_to = "share_literate_new") %>%
  mutate(share_illiterate_new = 1-share_literate_new,
         sex = "Male") %>%
  drop_na()

# combine with survey waves where both males and females were tested
literacy_total <- literacy_rev %>%
    full_join(literacy_long) %>%
  mutate(share_literate = coalesce(share_literate, share_literate_new)) %>%
  dplyr::select(-share_literate_new, -share_illiterate_new)

#=======================================================#
#=====> STEP 4: DATA TRANSFORMATIONS FOR PREDICTIONS BY BROAD AGE AND SEX
#-----------------------------------------------------------------------------------------------------------

# data transformations for literacy by age and sex
literacy_rev <- literacy %>% 
  filter(age_group != "Total",
         !is.na(lit),
         Sex!="both") %>%
  dplyr::select(Country, year, age_group, Sex, lit, iso_code) %>%
  rename(name = Country, 
         sex = Sex,
         share_literate = lit,
         isono = iso_code) %>%
  mutate(name = ifelse(name == "Central Africa Republic", "Central African Republic", name),
         name = ifelse(name == "Kosovo under UNSC res.1244", "Kosovo", name)) %>%
  distinct() %>%
  mutate(sex = ifelse(sex == "F", "Female", "Male"),
         age_group = ifelse(age_group == "20-39", "20--39", "40--64")) %>%
  group_by(isono, name, year, age_group, sex) %>%
  reframe(share_literate = mean(share_literate))

# bring to wide format
literacy_wide <- literacy_rev %>%
  group_by(isono, name, year, age_group, sex) %>%
  reframe(share_literate = mean(share_literate)) %>%
  pivot_wider(names_from = sex, values_from = share_literate) %>%
  left_join(geo %>% dplyr::select(isono, region)) 

# calculate share of people with no education by broad age and sex from WIC reoconstruction
wic_age <- wic %>%
  filter(agest > 15 & agest < 65,
         Time < 2020) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))

share_no_edu_age <- wic_age %>%
  group_by(ccode, Time, age_group, sex) %>%
  summarise(
    total_pop = sum(pop),
    no_edu_pop = sum(pop[edu == "e1"]),
    share_no_edu = (no_edu_pop / total_pop)*100,
    .groups = "drop") 

# calculate gender gap in share of people with no education
ggapedu_age <- share_no_edu_age %>%
  dplyr::select(ccode, Time, age_group, sex, share_no_edu) %>%
  pivot_wider(names_from = sex, values_from = share_no_edu) %>%
  mutate(ggapedu = m - f) %>%
  rename(isono = ccode,
         year = Time,
         no_edu_males = m) %>%
  dplyr::select(isono, year, age_group, ggapedu, no_edu_males)

# load gender gap in share of people with no education by broad age and sex from WIC projections  
ggapedu_age_proj <- get_wcde(indicator = "bprop", version = "wcde-v3") %>%
  filter(education == "No Education",
         age %in% c("20--39", "40--64"),
         sex != "Both") %>%
  dplyr::select(country_code, year, age, sex, bprop) %>%
  rename(isono = country_code,
         age_group = age) %>%
  pivot_wider(names_from = sex, values_from = bprop) %>%
  mutate(ggapedu = Male - Female) %>%
  dplyr::select(isono, year, Male, age_group, ggapedu) %>%
  rename(no_edu_males = Male)

# combine reconstructed and projected data
ggapedu_age <- bind_rows(ggapedu_age,
                         ggapedu_age_proj) 

# combine variables
literacy_wide <- literacy_wide %>% left_join(ggapedu_age)

#=======================================================#
#=====> STEP 5: MALE PREDICTIONS BY BROAD AGE AND SEX
#-----------------------------------------------------------------------------------------------------------

# drop NA values
literacy_full <- literacy_wide %>% drop_na()

model2 <- lm(Male ~ Female + ggapedu + age_group + region, data = literacy_full)
summary(model2)

# save model output
# stargazer(model2,
#           type = "html",
#           out = "Output/literacy_rates_male_predictions.html",        
#           digits = 3,
#           title = "Regression Results: Male Literacy Rate",
#           dep.var.labels = "Male literacy rate", 
#           single.row = T)

# use model 2 to predict male values
predicted_literacy <- literacy_wide %>% filter(is.na(Male))

# Predict male literacy using the model
predicted_literacy$Male_estimated <- predict(model2, newdata = predicted_literacy)

# Combine predicted and original male values into a final column
literacy_wide <- literacy_wide %>%
  mutate(Male_predicted = predict(model2, newdata = .),
         Male_final = ifelse(is.na(Male),
                             Male_predicted,
                             Male)) %>%
  mutate(Male_final = ifelse(Male_final > 1, 1, Male_final))

# plot predicted vs observed values
#ggplot(literacy_wide, aes(x = Male_predicted, y = Male)) + 
 # geom_point() + 
  #geom_abline()

# bring to long format
literacy_long <- literacy_wide %>%
  dplyr::select(isono, name, year,  age_group, Male_final) %>%
  pivot_longer(cols = 5, names_to = "sex", values_to = "share_literate_new") %>%
  mutate(sex = "Male") %>%
  drop_na()

# combine with survey waves where both males and females were tested
literacy_broad_age <- literacy_rev %>%
  full_join(literacy_long) %>%
  mutate(share_literate = coalesce(share_literate, share_literate_new)) %>%
  dplyr::select(-share_literate_new)

# combine new literacy data for total working-age population with data by age and sex
literacy_new <- bind_rows(literacy_total %>% mutate(age_group = "20--64"),
                          literacy_broad_age) %>%
  arrange(name, year, sex, age_group)


#=======================================================#
#=====> STEP 6: CALCULATE VALUES FOR BOTH GENDERS COMBINED
#-----------------------------------------------------------------------------------------------------------

wic <- wic %>%
  rename(year = Time, 
         country_code = ccode)

# calculate reconstructed population size by broad age group and sex
pop_age_sex_past <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))  %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male"))

# calculate reconstructed population size by sex
pop_sex_past <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  group_by(country_code, year, sex) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male"),
         age_group = "20--64") 

pop_age_sex_past <- pop_age_sex_past %>%
  bind_rows(pop_sex_past)

# load WIC projected population size by broad age group and sex
pop_age_sex_proj <- get_wcde(indicator = "bpop", pop_age = "all",  pop_sex = "all", version = "wcde-v3") %>%
  filter(age %in% c("20--39",
                    "40--64",
                    "20--64"), 
         year > 2015 & year < 2030,
         sex != "Both") %>%
  rename(age_group = age,
         pop = bpop) %>%
  group_by(country_code, year, age_group, sex) %>%
  reframe(pop = sum(pop)) %>%
  dplyr::select(country_code, year, age_group, sex, pop) 


# combine reconstructed and projected population size
pop_age_sex <- bind_rows(pop_age_sex_past,
                         pop_age_sex_proj)


# aggregate values for both geders based on population size
literacy_both <- literacy_new %>%
  rename(country_code = isono) %>%
  left_join(pop_age_sex) %>% 
  filter(!is.na(pop)) %>%
  group_by(country_code, name, year, age_group) %>%
  reframe(share_literate = weighted.mean(share_literate)) %>%
  mutate(sex = "Both")
  
literacy_final <- bind_rows(literacy_new %>% rename(country_code = isono), 
                            literacy_both) %>%
  arrange(country_code, name, year)

# save data
write_csv(literacy_final, "Output/mics_dhs_literacy_rev.csv")

#_______________________________________________________________________________
#_______________________________________________________________________________

