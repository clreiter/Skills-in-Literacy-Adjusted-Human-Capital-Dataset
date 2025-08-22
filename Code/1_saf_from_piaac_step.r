#=============================================================#
#=====> SKILL-ADJUSTMENT FACTOR (SAF) for 48 Countries <=====#
#       (PIAAC 1, PIAAC 2, STEP)
#=============================================================#

#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Changing the working directory
setwd("/Users/claudiareiter/Library/CloudStorage/Dropbox/Claudia/IIASA/Link4Skills/SLAMYS 2.0/Github")

#=> Installing and loading necessary packages
#install.packages("pacman")
pacman::p_load(tidyverse, intsvy, zoo, foreign, openxlsx, wcde, magrittr, readstata13, BIFIEsurvey)

#-----------------------------------------------------------------------------------------------------------
#NOTE: DATA CODING (unless specified otherwise)

#   Age:                  Education:                              
#   15 = 15-19            1 = Lower secondary or less
#   20 = 20-24            2 = Upper secondary or higher       
#   25 = 25-29            
#   30 = 30-34            
#   35 = 35-39            
#   40 = 40-44
#   45 = 45-49   
#   50 = 50-54
#   55 = 55-59   
#   60 = 60-64


# define list of 38 OECD countries (3-digit numeric ISO country code)

oecd_countries <- c(36, 40, 56, 124, 152, 170, 188, 203, 208, 233, 246, 250, 276, 
                    300, 348, 352,372, 376, 380, 392, 410, 428, 440, 442, 484, 528,
                    554, 578, 616, 620, 703, 705, 724, 752, 756, 792, 826, 840)


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#=====> STEP 1: ESTIMATE STANDARD OF COMPARISON 
#       (POPULATION-WEIGHTED OECD MEAN PIAAC LITERACY SCORE BY COUNTRY AND BROAD AGE GROUP, 2015 (PIAAC Cycle 1))

#=> Read PIAAC Cycle 1 data
load("Input/PIAAC_Cycle1_full.RData")

# recode age, sex, and education variable
piaac1_rev <- piaac1 %>%
  mutate(age = AGEG5LFS*5 + 10, 
         sex = ifelse(GENDER_R == 1, "Male", 
                      ifelse (GENDER_R == 2, "Female", NA)),
         age_group = ifelse(age > 15 & age <40, "20--39",
                            ifelse(age > 15, "40--64", "Below 20")),
         educ = EDCAT7, 
         educ = recode(EDCAT7, 
                "2" = "1",
                "3" = "2",
                "4" = "2",
                "5" = "2",
                "6" = "2", 
                "7" = "2",
                "8" = "2"),
                educ = ifelse(educ == "N", NA, educ))
          
#=> calculate mean literacy score by country (population 20-64) for all OECD countries
lit_mean_piaac_oecd <- piaac1_rev %>%
  filter(CNTRYID %in% oecd_countries, age > 15) %>%
  piaac.mean.pv(pvlabel =  paste0("PVLIT", 1:10), by = c("CNTRYID")) %>%
  rename(country_code = CNTRYID,  n = Freq, mean_skills =  Mean) %>%
  mutate(country_code = as.numeric(as.character(country_code)),
         age_group = "20--64")

#=> calculate mean literacy score by country and broad age group for all OECD countries
lit_mean_piaac_oecd_age<- piaac1_rev %>%
  filter(CNTRYID %in% oecd_countries, age > 15, !is.na(age_group)) %>%
  piaac.mean.pv(pvlabel =  paste0("PVLIT", 1:10), by = c("CNTRYID", "age_group")) %>%
  rename(country_code = CNTRYID,  n = Freq, mean_skills =  Mean) %>%
  mutate(country_code = as.numeric(as.character(country_code))) %>%
  bind_rows(lit_mean_piaac_oecd)    # add mean values by country

#=> read 2015 WIC population size by broad age group
wic <- read_csv("Input/WIC_reconstruction.csv") %>%
  rename(year = Time, 
         country_code = ccode)

pop <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  group_by(country_code, year) %>%
  reframe(pop = sum(pop)) %>%
  mutate(age_group = "20--64",
         sex = "Both")

pop_age <- wic %>%
  filter(agest > 15 & agest < 65) %>%
  mutate(age_group = ifelse(agest < 40, "20--39", "40--64"))  %>%
  group_by(country_code, year, age_group) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = "Both")

wic_pop2015 <- bind_rows(pop, pop_age) %>%
  filter(year == 2015)

#=> calculate population-weighted OECD PIAAC literacy mean by broad age group
oecd_standard <- lit_mean_piaac_oecd_age %>%
  left_join(wic_pop2015) %>%  
  group_by(age_group) %>% 
  reframe(oecd_standard = weighted.mean(mean_skills, pop),                 
          n_oecd = sum(n))
  
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#=====> STEP 2: CALCULATE EMPIRICAL LITERACY SKILLS BY AGE, SEX, AND BROAD EDUCATION

#-----------------------------------------------------------------------------------------------------------
# a) Base Year 2015 (PIAAC Cycle 1)

#=> Removing some objects from memory
rm(list= ls()[!(ls() %in% c("piaac1", "piaac1_rev", "oecd_standard", "oecd_countries", "wic"))])

#=> CALCULATE BASE YEAR LITERACY SKILLS BASED ON PIAAC DATA

#calculate literacy skills by country, 5-year age groups, sex, and broad education
lit_mean_piaac_country_age_sex_educ <- piaac1_rev %>%
  filter(!is.na(sex), !is.na(age), !is.na(educ)) %>%
  piaac.mean.pv(pvlabel =  paste0("PVLIT", 1:10), by = c("CNTRYID", "sex", "age", "educ")) %>%
  rename(country_code = CNTRYID,  n = Freq, mean_skills =  Mean) %>%
  mutate(age = as.numeric(as.character(age)), 
         country_code = as.numeric(as.character(country_code))) 

# remove observations with n < 10
lit_mean_piaac_country_age_sex_educ %<>%
  filter(n>=10)

# add rows with missing observations
country_code <- unique(lit_mean_piaac_country_age_sex_educ$country_code)
educ <- unique(lit_mean_piaac_country_age_sex_educ$educ)
age <- unique(lit_mean_piaac_country_age_sex_educ$age)
sex <- unique(lit_mean_piaac_country_age_sex_educ$sex)

complete_df <- expand.grid(country_code = country_code, 
                           age = age,
                           sex = sex,
                           educ = educ)

lit_mean_piaac_country_age_sex_educ %<>% right_join(complete_df) 

# fill missing value using data from following age group
lit_mean_piaac_country_age_sex_educ_complete <- lit_mean_piaac_country_age_sex_educ %>% 
  arrange(age) %>%
  group_by(country_code, sex, educ) %>%
  mutate(mean_skills = na.locf0(mean_skills, fromLast = T)) %>%
  ungroup %>%
  arrange(country_code, age, sex, educ)

#=> CALCULATE BASE YEAR LITERACY SKILLS BASED ON STEP DATA

#=> read STEP data
armenia <- read.dta13("Input/STEP Armenia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 51) 

bolivia <- read.dta13("Input/STEP Bolivia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 68)

colombia <- read.dta13("Input/STEP Colombia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 170)

georgia <- read.dta13("Input/STEP Georgia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 268)

ghana <- read.dta13("Input/STEP Ghana_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 288)

kenya <- read.dta13("Input/STEP Kenya_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 404)

ukraine <- read.dta13("Input/STEP Ukraine_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 804)

vietnam <- read.dta13("Input/STEP Vietnam_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(country_code = 704)

#calculate literacy skills by country, 5-year age groups, educ, and sex
step <- rbind(armenia, bolivia, colombia, georgia, ghana, kenya, ukraine, vietnam) %>%
  mutate(age5 = cut(age, breaks=c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65), right = FALSE),
         age=as.numeric(as.character(str_sub(age5, 2, 3))),
         educ = ifelse (m1a_q09==1 | m1a_q09==2 | m1a_q09 == 3, 1, 0),
         educ = ifelse (m1a_q09==4 | m1a_q09==5 | m1a_q09==6 | m1a_q09==7 | m1a_q09==8 | m1a_q09==9, 2, educ)) %>%
  filter(educ > 0)

bifie_step <- BIFIE.data(step, wgt=c("W_FinSPwt"), wgtrep=NULL, pv_vars=c("PVLIT"))
step_mean <- BIFIE.univar(bifie_step, vars=c("PVLIT"), group=c("country_code", "age", "gender", "educ"))

mean_skills <- step_mean$stat$M
country_code <- step_mean$stat$groupval1
age <- step_mean$stat$groupval2
gender <- step_mean$stat$groupval3
educ <- step_mean$stat$groupval4
n <- step_mean$stat$Ncases

lit_mean_step_country_age_sex_educ <- data.frame(country_code, age, gender, educ, mean_skills, n) %>%
  mutate(sex = ifelse(gender == 0, "Male", 
             ifelse (gender == 1, "Female", NA))) %>%
  select(-gender)

# remove observations with n < 10
lit_mean_step_country_age_sex_educ %<>%
  filter(n>=10)

# add rows with missing observations
country_code <- unique(lit_mean_step_country_age_sex_educ$country_code)
educ <- unique(lit_mean_step_country_age_sex_educ$educ)
age <- unique(lit_mean_step_country_age_sex_educ$age)
sex <- unique(lit_mean_step_country_age_sex_educ$sex)

complete_df <- expand.grid(country_code = country_code, 
                           age = age,
                           sex = sex,
                           educ = educ)

lit_mean_step_country_age_sex_educ %<>% right_join(complete_df) 

# fill missing value using data from following age group
lit_mean_step_country_age_sex_educ_complete <- lit_mean_step_country_age_sex_educ %>% 
  dplyr::arrange(age) %>%
  group_by(country_code, sex, educ) %>%
  mutate(mean_skills = na.locf0(mean_skills, fromLast = T)) %>%
  ungroup() 

# fill missing value using data from previous age group
lit_mean_step_country_age_sex_educ_complete %<>% 
  dplyr::arrange(age) %>%
  group_by(country_code, sex, educ) %>%
  mutate(mean_skills = na.locf0(mean_skills, fromLast = F)) %>%
  ungroup() %>%
  dplyr::arrange(country_code, age, sex, educ)


#=> read STEP urban/rural adjustments (calculated from MICS and DHS)
urban_rural_adjustment <- read.xlsx("Input/STEP_urban_adjustment_DHS.xlsx", sheet=1) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  select(country_code, sex, adjustment_factor)

lit_mean_step_country_age_sex_educ_adjusted <- lit_mean_step_country_age_sex_educ_complete %>%
  left_join(urban_rural_adjustment) %>%
  mutate(mean_skills = mean_skills * adjustment_factor)


#=> COMBINE PIAAC AND STEP mean literacy skills
mean_skills_2015 <- bind_rows(lit_mean_piaac_country_age_sex_educ_complete %>% mutate(educ = as.numeric(as.character(educ))),
                              lit_mean_step_country_age_sex_educ_adjusted) %>%
  mutate(year = 2015) %>%
  select(year, country_code, age, sex, educ, mean_skills) 

#-----------------------------------------------------------------------------------------------------------
# b) 2020 (PIAAC Cycle 2)

#=> Removing some objects from memory
rm(list= ls()[!(ls() %in% c("piaac1_rev", "mean_skills_2015",  "oecd_standard", "oecd_countries", "wic"))])

#=> Read PIAAC Cycle 2 data
load("Input/PIAAC_Cycle2_full.RData")

#replace Austrian PUF data with Austrian SUF data 
austria <- read.spss("Input/V2_SUF_PRGAUTP2.sav",
                     to.data.frame = T, use.value.labels = F)

# convert all variables to a consistent class to allow bind_rows()
for (var in intersect(names(piaac2), names(austria))) {
  if (class(piaac2[[var]]) != class(austria[[var]])) {
    common_type <- "character"
    piaac2[[var]] <- as.character(piaac2[[var]])
    austria[[var]] <- as.character(austria[[var]])
  }
}

# add Austrian SUF data
piaac2_rev <- piaac2 %>%
  filter(CNTRYID!= 40) %>%
  bind_rows(austria)

# impute 5-year age group for those countries which only provide 10-year age groups
set.seed(123)  # for reproducibility

piaac2_rev <- piaac2_rev %>%
  rowwise() %>%
  mutate(
    AGEG5LFS = as.numeric(as.character(AGEG5LFS)),
    AGEG10LFS = as.numeric(as.character(AGEG10LFS)),
    AGEG5LFS_imputed = case_when(
      !is.na(AGEG5LFS) ~ AGEG5LFS,
      AGEG10LFS == 1 ~ sample(c(1, 2), 1),  # 16–24
      AGEG10LFS == 2 ~ sample(c(3, 4), 1),  # 25–34
      AGEG10LFS == 3 ~ sample(c(5, 6), 1),  # 35–44
      AGEG10LFS == 4 ~ sample(c(7, 8), 1),  # 45–54
      AGEG10LFS == 5 ~ sample(c(9, 10), 1), # 55–65
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

#recode age, sex, and education variable
piaac2_rev <- piaac2_rev %>%
  mutate(age = AGEG5LFS_imputed*5 + 10, 
         sex = ifelse(GENDER_R == 1, "Male", 
                      ifelse (GENDER_R == 2, "Female", NA)),
         educ = EDCAT6_TC1, 
         educ = recode(EDCAT6_TC1, 
                       "3" = "2",
                       "4" = "2",
                       "5" = "2",
                       "6" = "2"),
         educ = ifelse(educ == "N" | educ == "." | educ == ".n", NA, educ),
         B2_Q01 = as.numeric(as.character(B2_Q01)),
         educ = ifelse(CNTRYID == 208 & B2_Q01 %in% c(0:4), 1, 
                       ifelse (CNTRYID == 208 & B2_Q01 %in% c(5:30), 2, educ))) 


#=> CALCULATE 2020 LITERACY SKILLS BASED ON PIAAC CYCLE 2 DATA

#calculate literacy skills by country, 5-year age group, sex, and broad education
lit_mean_piaac_country_age_sex_educ <- piaac2_rev %>%
  filter(!is.na(sex), !is.na(age), !is.na(educ)) %>%
  piaac.mean.pv(pvlabel =  paste0("PVLIT", 1:10), by = c("CNTRYID", "sex", "age", "educ")) %>%
  rename(country_code = CNTRYID,  n = Freq, mean_skills =  Mean) %>%
  mutate(age = as.numeric(as.character(age)), 
         country_code = as.numeric(as.character(country_code))) 

# remove observations with n < 10
lit_mean_piaac_country_age_sex_educ %<>%
  filter(n>=10)

# add rows with missing observations
country_code <- unique(lit_mean_piaac_country_age_sex_educ$country_code)
educ <- unique(lit_mean_piaac_country_age_sex_educ$educ)
age <- unique(lit_mean_piaac_country_age_sex_educ$age)
sex <- unique(lit_mean_piaac_country_age_sex_educ$sex)

complete_df <- expand.grid(country_code = country_code, 
                           age = age,
                           sex = sex,
                           educ = educ)

lit_mean_piaac_country_age_sex_educ %<>% right_join(complete_df) 

# fill missing value using data from following age group
lit_mean_piaac_country_age_sex_educ_complete <- lit_mean_piaac_country_age_sex_educ %>% 
  arrange(age) %>%
  group_by(country_code, sex, educ) %>%
  mutate(mean_skills = na.locf0(mean_skills, fromLast = T)) %>%
  ungroup()

# fill missing value using data from previous age group
lit_mean_piaac_country_age_sex_educ_complete %<>%
  arrange(age) %>%
  group_by(country_code, sex, educ) %>%
  mutate(mean_skills = na.locf0(mean_skills, fromLast = F)) %>%
  ungroup %>%
  arrange(country_code, age, sex, educ)

mean_skills_2020 <- lit_mean_piaac_country_age_sex_educ_complete %>%
  mutate(year = 2020) %>%
  select(year, country_code, age, sex, educ, mean_skills)

#_______________________________________________________________________________
#_______________________________________________________________________________

#=====> STEP 3: ESTIMATE STANDARD AGEING PATTERN

#=> Removing some objects from memory
rm(list= ls()[!(ls() %in% c("piaac1_rev","piaac2_rev", "mean_skills_2015", "mean_skills_2020", "oecd_standard", "oecd_countries", "wic"))])

#=> read IALS data
ials_original<-read.spss("Input/IALS_data.sav", to.data.frame=TRUE)

#recode countries to numeric ISO codes (IALS uses survey-specific country codes)
ials <- ials_original %>%
  mutate(CNTRYID = recode(CNTRID,
                               `1` = 124,    #Canada (English)
                               `2` = 124,    #Canada (French)
                               `3` = 756,    #Switzerland (German)
                               `4` = 756,    #Switzerland (French)
                               `22` = 756,   #Switzerland (Italian)
                               `5` = 276,    #Germany
                               `6` = 840,    #USA
                               `7` = 372,    #Ireland
                               `8` = 528,    #Netherlands
                               `9` = 616,    #Poland
                               `11` = 752,   #Sweden
                               `13` = 554,   #New Zealand
                               `14` = 826,   #UK (GBR)
                               `15` = 826,   #UK (Northern Ireland)
                               `16` = 56,    #Belgium (Flanders)
                               `17` = 380,   #Italy
                               `18` = 578,   #Norway
                               `20` = 705,   #Slovenia
                               `21` = 203,   #Czechia
                               `23` = 208,   #Denmark
                               `24` = 246,   #Finland
                               `25` = 348,   #Hungary
                               `29` = 152))  #Chile
                               
# recode sex
ials <- ials %>%
  mutate(sex = ifelse(GENDER == 1, "Male",
                      ifelse(GENDER == 2, "Female", NA)))

# recode education (2 broad education groups)
ials %<>%
  mutate(A8 = ifelse(is.na(A8), A8RCD, A8),
         educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0),
         educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) %>%
  filter(educ > 0)

#=>recode age corresponding to PIAAC

#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, USA, Netherlands) - 17 years
ials_1994_2011 <- ials %>%
  filter(CNTRYID %in% c(276, 372, 752, 616, 528 , 840)) %>%
  mutate(age_piaac=ifelse (AGE>12 & AGE<18, 30, 0),
         age_piaac=ifelse (AGE>17 & AGE<23, 35, age_piaac),
         age_piaac=ifelse (AGE>22 & AGE<28, 40, age_piaac),
         age_piaac=ifelse (AGE>27 & AGE<33, 45, age_piaac),
         age_piaac=ifelse (AGE>32 & AGE<38, 50, age_piaac),
         age_piaac=ifelse (AGE>37 & AGE<43, 55, age_piaac),
         age_piaac=ifelse (AGE>42 & AGE<48, 60, age_piaac),
         age_piaac=ifelse (AGE>47 & AGE<53, 65, age_piaac),
         age_piaac=ifelse (AGE>52 & AGE<58, 70, age_piaac),
         age_piaac=ifelse (AGE>57 & AGE<64, 75, age_piaac),
         age_piaac=ifelse (AGE>63 & AGE<68, 80, age_piaac)) %>%
  filter(age_piaac > 0)

#countries with IALS in 1996 and PIAAC in 2011 (UK, Belgium) - 15 years
ials_1996_2011 <- ials %>%
  filter(CNTRYID %in% c(56, 826)) %>%
  mutate(age_piaac=ifelse (AGE>14 & AGE<20, 30, 0),
         age_piaac=ifelse (AGE>19 & AGE<25, 35, age_piaac),
         age_piaac=ifelse (AGE>24 & AGE<30, 40, age_piaac),
         age_piaac=ifelse (AGE>29 & AGE<35, 45, age_piaac),
         age_piaac=ifelse (AGE>34 & AGE<40, 55, age_piaac),
         age_piaac=ifelse (AGE>39 & AGE<45, 60, age_piaac),
         age_piaac=ifelse (AGE>44 & AGE<50, 65, age_piaac),
         age_piaac=ifelse (AGE>49 & AGE<55, 70, age_piaac),
         age_piaac=ifelse (AGE>54 & AGE<60, 75, age_piaac),
         age_piaac=ifelse (AGE>59 & AGE<66, 80, age_piaac),
         age_piaac=ifelse (AGE>65 & AGE<71, 85, age_piaac)) %>%
  filter(age_piaac > 0)

#countries with IALS in 1998 and PIAAC in 2011 (Norway, Finland, Czechia, Denmark, Italy) - 13 years
ials_1998_2011 <- ials %>%
  filter(CNTRYID %in% c(578, 246, 203, 208, 380)) %>%
  mutate(age_piaac=ifelse (AGE>11 & AGE<17, 25, 0),
         age_piaac=ifelse (AGE>16 & AGE<22, 30, age_piaac),
         age_piaac=ifelse (AGE>21 & AGE<27, 35, age_piaac),
         age_piaac=ifelse (AGE>26 & AGE<32, 40, age_piaac),
         age_piaac=ifelse (AGE>31 & AGE<37, 45, age_piaac),
         age_piaac=ifelse (AGE>36 & AGE<42, 50, age_piaac),
         age_piaac=ifelse (AGE>41 & AGE<47, 55, age_piaac),
         age_piaac=ifelse (AGE>46 & AGE<52, 60, age_piaac),
         age_piaac=ifelse (AGE>51 & AGE<57, 65, age_piaac),
         age_piaac=ifelse (AGE>56 & AGE<62, 70, age_piaac),
         age_piaac=ifelse (AGE>61 & AGE<67, 75, age_piaac)) %>%
  filter(age_piaac > 0)

#countries with IALS in 1996 and PIAAC in 2014 (New Zealand) - 18 years
ials_1996_2014 <- ials %>%
  filter(CNTRYID == 554) %>%
  mutate(age_piaac=ifelse (AGE>11 & AGE<17, 30, 0),
         age_piaac=ifelse (AGE>16 & AGE<22, 35, age_piaac),
         age_piaac=ifelse (AGE>21 & AGE<27, 40, age_piaac),
         age_piaac=ifelse (AGE>26 & AGE<32, 45, age_piaac),
         age_piaac=ifelse (AGE>31 & AGE<37, 50, age_piaac),
         age_piaac=ifelse (AGE>36 & AGE<42, 55, age_piaac),
         age_piaac=ifelse (AGE>41 & AGE<47, 60, age_piaac),
         age_piaac=ifelse (AGE>46 & AGE<52, 65, age_piaac),
         age_piaac=ifelse (AGE>51 & AGE<57, 70, age_piaac),
         age_piaac=ifelse (AGE>56 & AGE<62, 75, age_piaac),
         age_piaac=ifelse (AGE>61 & AGE<67, 80, age_piaac)) %>%
  filter(age_piaac > 0)

#countries with IALS in 1998 and PIAAC in 2014 (Chile, Slovenia) - 16 years
ials_1998_2014 <- ials %>%
  filter(CNTRYID %in% c(152, 705)) %>%
  mutate(age_piaac=ifelse (AGE>13 & AGE<19, 30, 0),
         age_piaac=ifelse (AGE>18 & AGE<24, 35, age_piaac),
         age_piaac=ifelse (AGE>23 & AGE<29, 40, age_piaac),
         age_piaac=ifelse (AGE>28 & AGE<34, 45, age_piaac),
         age_piaac=ifelse (AGE>33 & AGE<39, 50, age_piaac),
         age_piaac=ifelse (AGE>38 & AGE<44, 55, age_piaac),
         age_piaac=ifelse (AGE>43 & AGE<49, 60, age_piaac),
         age_piaac=ifelse (AGE>48 & AGE<54, 65, age_piaac),
         age_piaac=ifelse (AGE>53 & AGE<59, 70, age_piaac),
         age_piaac=ifelse (AGE>58 & AGE<64, 75, age_piaac),
         age_piaac=ifelse (AGE>63 & AGE<69, 80, age_piaac)) %>%
  filter(age_piaac > 0)

#countries with IALS in 1998 and PIAAC in 2017 (Hungary) - 19 years
ials_1998_2017 <- ials %>%
  filter(CNTRYID == 348) %>% 
  mutate(age_piaac=ifelse (AGE>10 & AGE<16, 30, 0),
         age_piaac=ifelse (AGE>15 & AGE<21, 35, age_piaac),
         age_piaac=ifelse (AGE>20 & AGE<26, 40, age_piaac),
         age_piaac=ifelse (AGE>25 & AGE<31, 45, age_piaac),
         age_piaac=ifelse (AGE>30 & AGE<36, 50, age_piaac),
         age_piaac=ifelse (AGE>35 & AGE<41, 55, age_piaac),
         age_piaac=ifelse (AGE>40 & AGE<46, 60, age_piaac),
         age_piaac=ifelse (AGE>45 & AGE<51, 65, age_piaac),
         age_piaac=ifelse (AGE>50 & AGE<56, 70, age_piaac),
         age_piaac=ifelse (AGE>55 & AGE<61, 75, age_piaac),
         age_piaac=ifelse (AGE>60 & AGE<66, 80, age_piaac),
         age_piaac=ifelse (AGE>65 & AGE<71, 85, age_piaac)) %>%
  filter(age_piaac > 0)

ials_rev <- bind_rows(ials_1994_2011, ials_1996_2011, ials_1996_2014, ials_1998_2011, ials_1998_2014, ials_1998_2017) %>%
  mutate(test = "IALS")

#=> configuration for using intsvy package with IALS data
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

#=> calculate IALS literacy mean by country, sex, and two broad education categories
lit_mean_ials <- ials_rev %>%
  intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("age_piaac", "test", "sex", "educ", "CNTRYID"), config=piaac_conf) %>%
  mutate(age_piaac = as.numeric(as.character(age_piaac))) %>%
  filter(age_piaac>25 & age_piaac<65, !is.na(sex)) %>%
  group_by(age_piaac, test, sex, educ) %>%
  reframe(mean_skills = mean(Mean, na.rm = T))

#=>keep only IALS countries in PIAAC data
piaac_ials <- piaac1_rev %>%
  filter(CNTRYID %in% ials_rev$CNTRYID) %>%
  rename(age_piaac = age)

#=> calculate PIAAC literacy mean by country, sex, and two broad education categories
lit_mean_piaac <- piaac_ials %>% 
  filter(!is.na(sex), !is.na(educ)) %>%
  piaac.mean.pv(pvlabel =  paste0("PVLIT", 1:10), by=c("age_piaac", "sex", "educ", "CNTRYID")) %>%
  mutate(test = "PIAAC",
         age_piaac = as.numeric(as.character(age_piaac))) %>%
  group_by(age_piaac, test, sex, educ) %>%
  reframe(mean_skills = mean(Mean, na.rm = T))

#=> combine IALS and PIAAC data
ials_piaac_cohorts <-bind_rows(lit_mean_ials, lit_mean_piaac) 

#=> calculate mean IALS literacy of all countries by sex and education (for period-adjustment factor)
lit_mean_ials_country <- ials %>%
  filter(CNTRYID!=124,            # remove Canada (without age information)
         CNTRYID!=756,            # remove Switzerland (not in PIAAC)
         !is.na(sex), !is.na(educ)) %>%
  intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("sex", "educ", "CNTRYID"), config=piaac_conf) %>%
  rename(mean_ials = Mean) %>%
  group_by(sex, educ) %>%
  reframe(mean_ials = mean(mean_ials, na.rm = T))

#=> calculate mean PIAAC literacy of all countries by sex (for period-adjustment factor)
lit_mean_piaac_country <- piaac_ials %>%
  filter(!is.na(sex), !is.na(educ)) %>%
  piaac.mean.pv(pvlabel=paste0("PVLIT", 1:10), by=c("sex", "educ", "CNTRYID")) %>%
  rename(mean_piaac = Mean) %>%
  group_by(sex, educ) %>%
  reframe(mean_piaac = mean(mean_piaac, na.rm = T))

#=> calculate period-adjustment factor
period_adj <- left_join(lit_mean_piaac_country, lit_mean_ials_country) %>%
  mutate(adj_factor = mean_piaac/mean_ials) %>%
  select(sex, educ, adj_factor)

#=> calculate ageing pattern
ageing_pattern <- left_join(ials_piaac_cohorts, period_adj) %>%
  mutate(adj_mean = ifelse(test=="IALS", mean_skills*adj_factor, mean_skills)) %>%
  filter(age_piaac>25 & age_piaac<65) 

# plot reverse ageing pattern by educational attainment and sex
# ggplot(ageing_pattern, aes(x = test, y = adj_mean, color = as.character(age_piaac), group = as.character(age_piaac))) +
#   geom_line() + geom_point() +
#   facet_wrap(~sex + educ) +
#   labs(x = "", y= "Period-adjusted mean literacy score", color = "Age at PIAAC")
# ggsave("ageing_pattern_by_sex_and_educ.png")

# interpolation of skill development
interpolation <- ageing_pattern %>%
  select(age_piaac, sex, educ, test, adj_mean) %>%
  pivot_wider(names_from = test, values_from = adj_mean) %>%
  mutate(age_ials = age_piaac - 15,
         point2 = IALS + (PIAAC-IALS)/3,
         point3 = point2 + (PIAAC-IALS)/3)
  
growth_ials15 <- interpolation %>%
  filter(age_ials == 15) %>%
  group_by(sex, educ) %>%
  mutate(growth15_20 = (point2-IALS)/IALS,
         growth20_25 = (point3-point2)/point2,
         growth25_30 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth15_20, growth20_25, growth25_30)

growth_ials20 <- interpolation %>%
  filter(age_ials == 20) %>%
  group_by(sex, educ) %>%
  mutate(growth20_25 = (point2-IALS)/IALS,
         growth25_30 = (point3-point2)/point2,
         growth30_35 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth20_25, growth25_30, growth30_35)

growth_ials25 <- interpolation %>%
  filter(age_ials == 25) %>%
  group_by(sex, educ) %>%
  mutate(growth25_30 = (point2-IALS)/IALS,
         growth30_35 = (point3-point2)/point2,
         growth35_40 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth25_30, growth30_35, growth35_40)

growth_ials30 <- interpolation %>%
  filter(age_ials == 30) %>%
  group_by(sex, educ) %>%
  mutate(growth30_35 = (point2-IALS)/IALS,
         growth35_40 = (point3-point2)/point2,
         growth40_45 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth30_35, growth35_40, growth40_45)

growth_ials35 <- interpolation %>%
  filter(age_ials == 35) %>%
  group_by(sex, educ) %>%
  mutate(growth35_40 = (point2-IALS)/IALS,
         growth40_45 = (point3-point2)/point2,
         growth45_50 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth35_40, growth40_45, growth45_50)

growth_ials40 <- interpolation %>%
  filter(age_ials == 40) %>%
  group_by(sex, educ) %>%
  mutate(growth40_45 = (point2-IALS)/IALS,
         growth45_50 = (point3-point2)/point2,
         growth50_55 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth40_45, growth45_50, growth50_55)

growth_ials45 <- interpolation %>%
  filter(age_ials == 45) %>%
  group_by(sex, educ) %>%
  mutate(growth45_50 = (point2-IALS)/IALS,
         growth50_55 = (point3-point2)/point2,
         growth55_60 = (PIAAC-point3)/point3) %>%
  select(sex, educ, growth45_50, growth50_55, growth55_60)

projection_function <- bind_rows(growth_ials15,
                    growth_ials20,
                    growth_ials25,
                    growth_ials30,
                    growth_ials35,
                    growth_ials40,
                    growth_ials45) %>%
  group_by(sex, educ) %>%
  reframe(`15` = mean(growth15_20, na.rm = T),
          `20` = mean(growth20_25, na.rm = T),
          `25` = mean(growth25_30, na.rm = T),
          `30` = mean(growth30_35, na.rm = T),
          `35` = mean(growth35_40, na.rm = T),
          `40` = mean(growth40_45, na.rm = T),
          `45`= mean(growth45_50, na.rm = T),
          `50` = mean(growth50_55, na.rm = T),
          `55` = mean(growth55_60, na.rm = T)) %>%
  pivot_longer(names_to = "age", values_to = "percentage_growth", cols = 3:11)

# plot ageing pattern by educational attainment and sex
# ggplot(projection_function, aes(x = age, y = percentage_growth, color = sex, group = sex)) +
#   facet_wrap(~educ) + geom_line()

growth_piaac60 <- interpolation %>%
  filter(age_piaac == 60) %>%
  group_by(sex, educ) %>%
  mutate(growth60_55 = (point3-PIAAC)/PIAAC,
         growth55_50 = (point2-point3)/point3,
         growth50_45 = (IALS-point2)/point2) %>%
  select(sex, educ, growth60_55, growth55_50, growth50_45)

growth_piaac55 <- interpolation %>%
  filter(age_piaac == 55) %>%
  group_by(sex, educ) %>%
  mutate(growth55_50 = (point3-PIAAC)/PIAAC,
         growth50_45 = (point2-point3)/point3,
         growth45_40 = (IALS-point2)/point2) %>%
  select(sex, educ, growth55_50, growth50_45, growth45_40)

growth_piaac50 <- interpolation %>%
  filter(age_piaac == 50) %>%
  group_by(sex, educ) %>%
  mutate(growth50_45 = (point3-PIAAC)/PIAAC,
         growth45_40 = (point2-point3)/point3,
         growth40_35 = (IALS-point2)/point2) %>%
  select(sex, educ, growth50_45, growth45_40, growth40_35)

growth_piaac45 <- interpolation %>%
  filter(age_piaac == 45) %>%
  group_by(sex, educ) %>%
  mutate(growth45_40 = (point3-PIAAC)/PIAAC,
         growth40_35 = (point2-point3)/point3,
         growth35_30 = (IALS-point2)/point2) %>%
  select(sex, educ, growth45_40, growth40_35, growth35_30)

growth_piaac40 <- interpolation %>%
  filter(age_piaac == 40) %>%
  group_by(sex, educ) %>%
  mutate(growth40_35 = (point3-PIAAC)/PIAAC,
         growth35_30 = (point2-point3)/point3,
         growth30_25 = (IALS-point2)/point2) %>%
  select(sex, educ, growth40_35, growth35_30, growth30_25)

growth_piaac35 <- interpolation %>%
  filter(age_piaac == 35) %>%
  group_by(sex, educ) %>%
  mutate(growth35_30 = (point3-PIAAC)/PIAAC,
         growth30_25 = (point2-point3)/point3,
         growth25_20 = (IALS-point2)/point2) %>%
  select(sex, educ, growth35_30, growth30_25, growth25_20)

growth_piaac30 <- interpolation %>%
  filter(age_piaac == 30) %>%
  group_by(sex, educ) %>%
  mutate(growth30_25 = (point3-PIAAC)/PIAAC,
         growth25_20 = (point2-point3)/point3,
         growth20_15 = (IALS-point2)/point2) %>%
  select(sex, educ, growth30_25, growth25_20, growth20_15)

reconstruction_function <- bind_rows(growth_piaac60,
                                 growth_piaac55,
                                 growth_piaac50,
                                 growth_piaac45,
                                 growth_piaac40,
                                 growth_piaac35,
                                 growth_piaac30) %>%
  group_by(sex, educ) %>%
  reframe(`55` = mean(growth60_55, na.rm = T),
          `50` = mean(growth55_50, na.rm = T),
          `45`= mean(growth50_45, na.rm = T),
          `40` = mean(growth45_40, na.rm = T),
          `35` = mean(growth40_35, na.rm = T),
          `30` = mean(growth35_30, na.rm = T),
          `25` = mean(growth30_25, na.rm = T),
          `20` = mean(growth25_20, na.rm = T),
          `15` = mean(growth20_15, na.rm = T)) %>%
  pivot_longer(names_to = "age", values_to = "percentage_growth", cols = 3:11) %>%
  mutate(age = factor(age, levels = c("55", "50", "45", "40", "35", "30", "25", "20", "15")))

# ggplot(reconstruction_function, aes(x = age, y = percentage_growth, color = sex, group = sex)) +
#   facet_wrap(~educ) + geom_line()

#-----------------------------------------------------------------------------------------------------------
#=====> STEP 4: RECONSTRUCTION OF LITERACY SKILLS 1970-2015 AND PROJECTION OF LITERACY SKILLS 2020-2025

#=> Removing some objects from memory
rm(list= ls()[!(ls() %in% c("mean_skills_2015", "mean_skills_2020", "oecd_standard", "reconstruction_function", "projection_function", "wic"))])

# a) RECONSTRUCTION

#=> calculate reconstructed PIAAC/STEP scores for countries with base year 2015
scores_over_time_wide <- mean_skills_2015 %>%
  left_join(reconstruction_function %>% mutate(age = as.numeric(as.character(age)),
                                               educ = as.numeric(as.character(educ)))) %>%
  group_by(country_code, sex, educ) %>%
  mutate(`2010`= ifelse(is.na(percentage_growth), mean_skills, lead(mean_skills) + lead(mean_skills)*percentage_growth),
         `2005`= ifelse(is.na(percentage_growth), `2010`, lead(`2010`) + lead(`2010`)*percentage_growth),
         `2000`= ifelse(is.na(percentage_growth), `2005`, lead(`2005`) + lead(`2005`)*percentage_growth),
         `1995`= ifelse(is.na(percentage_growth), `2000`, lead(`2000`) + lead(`2000`)*percentage_growth),
         `1990`= ifelse(is.na(percentage_growth), `1995`, lead(`1995`) + lead(`1995`)*percentage_growth),
         `1985`= ifelse(is.na(percentage_growth), `1990`, lead(`1990`) + lead(`1990`)*percentage_growth),
         `1980`= ifelse(is.na(percentage_growth), `1985`, lead(`1985`) + lead(`1985`)*percentage_growth),
         `1975`= ifelse(is.na(percentage_growth), `1980`, lead(`1980`) + lead(`1980`)*percentage_growth),
         `1970`= ifelse(is.na(percentage_growth), `1975`, lead(`1975`) + lead(`1975`)*percentage_growth)) %>%
  ungroup() 

scores_over_time_2015 <- scores_over_time_wide %>%
  select(-year, -percentage_growth) %>%
  rename(`2015`= mean_skills) %>%
  pivot_longer(names_to = "year", values_to = "mean_skills", cols = 5:14 ) %>%
  mutate(year = as.numeric(year))


#=> calculate reconstructed PIAAC/STEP scores for countries with base year 2015
scores_over_time_wide <- mean_skills_2020 %>%
  filter(!country_code %in% mean_skills_2015$country_code) %>%
  left_join(reconstruction_function %>% mutate(age = as.numeric(as.character(age)))) %>%
  group_by(country_code, sex, educ) %>%
  mutate(`2015`= ifelse(is.na(percentage_growth), mean_skills, lead(mean_skills) + lead(mean_skills)*percentage_growth),
         `2010`= ifelse(is.na(percentage_growth), `2015`, lead(`2015`) + lead(`2015`)*percentage_growth),
         `2005`= ifelse(is.na(percentage_growth), `2010`, lead(`2010`) + lead(`2010`)*percentage_growth),
         `2000`= ifelse(is.na(percentage_growth), `2005`, lead(`2005`) + lead(`2005`)*percentage_growth),
         `1995`= ifelse(is.na(percentage_growth), `2000`, lead(`2000`) + lead(`2000`)*percentage_growth),
         `1990`= ifelse(is.na(percentage_growth), `1995`, lead(`1995`) + lead(`1995`)*percentage_growth),
         `1985`= ifelse(is.na(percentage_growth), `1990`, lead(`1990`) + lead(`1990`)*percentage_growth),
         `1980`= ifelse(is.na(percentage_growth), `1985`, lead(`1985`) + lead(`1985`)*percentage_growth),
         `1975`= ifelse(is.na(percentage_growth), `1980`, lead(`1980`) + lead(`1980`)*percentage_growth),
         `1970`= ifelse(is.na(percentage_growth), `1975`, lead(`1975`) + lead(`1975`)*percentage_growth)) %>%
  ungroup() 

scores_over_time_2020 <- scores_over_time_wide %>%
  select(-year, -percentage_growth) %>%
  rename(`2020`= mean_skills) %>%
  pivot_longer(names_to = "year", values_to = "mean_skills", cols = 5:15 ) %>%
  mutate(year = as.numeric(year)) %>% 
  bind_rows(mean_skills_2020 %>% filter(country_code %in% mean_skills_2015$country_code))

scores_over_time <- bind_rows(scores_over_time_2015,
                              scores_over_time_2020 %>% mutate(educ = as.numeric(as.character(educ))))

# b) PROJECTIONS

# estimate linear trend of test scores of 15-year-olds

all_years <- seq(1970, 2025, by = 5)

scores_over_time %<>%
  complete(country_code, age, sex, educ,
           year = all_years)


scores15<-scores_over_time %>% 
  filter(age==15)

models <- scores15 %>% 
  group_by(country_code, age, sex, educ) %>% 
  do(model = lm(mean_skills ~ year, data = .)) %>%
  ungroup()

scores15 <- left_join(scores15, models, by = c("country_code", "age", "sex", "educ"))

# estimate linear trends for countries with latest data in 2020

scores15_2025 <-scores15 %>%
  filter(country_code %in% mean_skills_2020$country_code) %>%
  group_by(country_code, age, sex, educ) %>%
  do(modelr::add_predictions(., first(.$model))) %>% mutate(model = NULL) %>%
  filter(year>2020) %>%
  select(-mean_skills)%>%
  rename(c("mean_skills"="pred"))

# estimate linear trends for countries with latest data in 2015

scores15_2020 <-scores15 %>%
  filter(!country_code %in% mean_skills_2020$country_code) %>%
  group_by(country_code, age, sex, educ) %>%
  do(modelr::add_predictions(., first(.$model))) %>% mutate(model = NULL) %>%
  filter(year>2015) %>%
  select(-mean_skills)%>%
  rename(c("mean_skills"="pred"))

# replace NAs for 15-year-olds with predicted values from linear trends

scores_over_time %<>%
  left_join(scores15_2020, by = c("country_code", "age", "sex", "educ", "year")) %>%
  left_join(scores15_2025, by = c("country_code", "age", "sex", "educ", "year")) %>%
  mutate(mean_skills = coalesce(mean_skills.x, mean_skills.y, mean_skills)) %>%
  select(-mean_skills.x, -mean_skills.y) %>%
  arrange(country_code, year, age, sex, educ)


# Project values for 2020

projections_2020 <- scores_over_time %>%
  filter(!country_code %in% scores_over_time_2020$country_code,
         year == 2015, age < 60)%>%
  left_join(projection_function %>% mutate(age = as.numeric(as.character(age)),
                                           educ = as.numeric(as.character(educ)))) %>%
  group_by(country_code, educ, sex) %>%
  mutate(year = year + 5,
         age = age +5,
         mean_skills = mean_skills + mean_skills * percentage_growth) %>%
  ungroup() %>%
  select(-percentage_growth)
  

# Project values for 2025

projections_2025 <- scores_over_time %>%
  filter(!is.na(mean_skills)) %>%
  bind_rows(projections_2020) %>%
  filter(year == 2020, age < 60)%>%
  left_join(projection_function %>% mutate(age = as.numeric(as.character(age)),
                                           educ = as.numeric(as.character(educ)))) %>%
  group_by(country_code, educ, sex) %>%
  mutate(year = year + 5,
         age = age +5,
         mean_skills = mean_skills + mean_skills * percentage_growth) %>%
  ungroup() %>%
  select(-percentage_growth)

# combine everything

scores_over_time_final <- scores_over_time %>%
  filter(!is.na(mean_skills)) %>%
  bind_rows(projections_2020) %>%
  bind_rows(projections_2025) %>%
  arrange(country_code, age, sex, educ, year) %>%
  select(country_code, year, age, sex, educ, mean_skills)


#=> aggregate skills based on population distribution

# read WIC population data 
wic_locations <- wic_locations %>% dplyr::select(isono, name)

pop_age_sex_educ_past <- wic %>%
  filter(agest > 15 & agest < 65,
         year > 1965 & year < 2020) %>%
  mutate(educ = ifelse(edu %in% c("e1", "e2", "e3", "e4"),
                       1, 2)) %>%
  group_by(country_code, year, agest, sex, educ) %>%
  reframe(pop = sum(pop)) %>%
  mutate(sex = ifelse(sex == "f", "Female", "Male")) %>%
  rename(age = agest) %>%
  left_join(wic_locations, by=c("country_code" = "isono"))

pop_age_sex_educ_proj <- get_wcde(indicator = "epop", pop_age = "all",  pop_sex = "all", version = "wcde-v3") %>%
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
         sex != "Both", 
         education %in% c("No Education", "Incomplete Primary", "Primary", "Lower Secondary", "Upper Secondary", "Post Secondary")) %>%
  mutate(age = as.numeric(substr(age, 1, 2)),
         age_group = ifelse(age <40, "20--39", "40--64"),
         educ = ifelse(education %in% c("No Education", "Incomplete Primary", "Primary", "Lower Secondary"),
                       1, 2)) %>%
  group_by(country_code, name, year, age, sex, educ) %>%
  reframe(pop = sum(epop)) %>%
  dplyr::select(country_code, name, year, age, sex, educ, pop) 

pop_age_sex_educ <- bind_rows(pop_age_sex_educ_past, 
                              pop_age_sex_educ_proj)

#calculate population-weighted mean skill by country, year, and sex (pop 20-64)
scores_over_time_sex <- scores_over_time_final %>%
  filter(age > 15) %>%
  left_join(pop_age_sex_educ) %>%
  group_by(country_code, name, year, sex) %>%
  reframe(mean_skills = weighted.mean(mean_skills, pop)) %>%
  mutate(age_group = "20--64")

#calculate population-weighted mean skills by country and year (pop 20-64)
scores_over_time_total <- scores_over_time_final %>%
  filter(age > 15) %>%
  left_join(pop_age_sex_educ) %>%
  group_by(country_code, name, year) %>%
  reframe(mean_skills = weighted.mean(mean_skills, pop)) %>%
  mutate(sex = "Both", age_group = "20--64")

#calculate population-weighted mean skills by country and broad age group (sex = both)
scores_over_time_age <- scores_over_time_final %>%
  filter(age > 15) %>%
  mutate(age_group = ifelse(age <40, "20--39", "40--64")) %>%
  left_join(pop_age_sex_educ) %>%
  group_by(country_code, name, year, age_group) %>%
  reframe(mean_skills = weighted.mean(mean_skills, pop)) %>%
  mutate(sex = "Both")

#calculate population-weighted mean skills by country, year, sex, and broad age group
scores_over_time_age_sex <- scores_over_time_final %>%
  filter(age > 15) %>%
  mutate(age_group = ifelse(age <40, "20--39", "40--64")) %>%
  left_join(pop_age_sex_educ) %>%
  group_by(country_code, name, year, sex, age_group) %>%
  reframe(mean_skills = weighted.mean(mean_skills, pop)) 

# combine all four datasets
scores_over_time_aggregated <- bind_rows(scores_over_time_total,
                                         scores_over_time_sex,
                                         scores_over_time_age,
                                         scores_over_time_age_sex) %>%
  arrange(name, age_group, sex, year) %>%
  select(country_code, name, year, age_group, sex, mean_skills)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#=====> STEP 5: CALCULATION OF SKILL-ADJUSTMENT FACTOR (SAF)

#=> Removing some objects from memory
rm(list= ls()[!(ls() %in% c("scores_over_time_aggregated", "oecd_standard", "wic"))])

# calculate skills adjustment factor (SAF)
saf <- scores_over_time_aggregated %>%
  left_join(oecd_standard) %>%
  mutate(saf = mean_skills / oecd_standard) %>%
  select(country_code, name, year, age_group, sex, mean_skills, oecd_standard, saf)

# save data
write_csv(saf, "Output/saf_piaac_step.csv")

