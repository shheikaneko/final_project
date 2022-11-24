################################################################################
# Final Project
# Author : Shuhei Kaneko
# Part 1; Data processing
################################################################################
# Import packages
library(foreign)
library(haven)
library(tidyverse)

################################################################################
# Import data
# Main source is BRFSS conducted in 2021.
brfss <- read.xport("raw_data/LLCP2021.xpt")

# Selection of variables
brfss <- brfss %>% select(X_STATE, GENHLTH, X_HLTHPLN, PHYSHLTH, MENTHLTH,
                  DIABETE4, X_SEX, X_METSTAT, X_URBSTAT, X_IMPRACE,
                  MARITAL, EDUCA, RENTHOM1, VETERAN3, EMPLOY1, CHILDREN,
                  INCOME3, X_AGE80,
                  HTM4, WTKG3, X_BMI5) %>% 
          rename_all(tolower)
################################################################################
# Re-coding the variables to suit for analyses. 
# Refuse to answer / do not know are treated as NA.

brfss <-brfss %>%
  #genhlth
  mutate(genhlth=replace(genhlth, genhlth==7|genhlth==9, NA)) %>% 
  #x_hlthplan (Yes/No insurance)
  mutate(x_hlthpln=replace(x_hlthpln, x_hlthpln==9, NA))%>% 
  #physhlth
  mutate(physhlth=replace(physhlth, physhlth==77|physhlth==99, NA)) %>% 
  mutate(physhlth=replace(physhlth, physhlth==88, 0)) %>% 
  #menthlth
  mutate(menthlth=replace(menthlth, menthlth==77|physhlth==99, NA)) %>% 
  mutate(menthlth=replace(menthlth, menthlth==88, 0)) %>% 
  #diabete4 (diabetes)
  mutate(diabete4=replace(diabete4, diabete4==2, 3)) %>% 
  mutate(diabete4=replace(diabete4, diabete4==4, 1)) %>% 
  mutate(diabete4=replace(diabete4, diabete4==7|diabete4==9, NA)) %>% 
  mutate(diabete4=replace(diabete4, diabete4==3, 0)) %>% 
  #marital (marital status)
  mutate(marital=replace(marital, marital==9,NA)) %>% 
  #educa (educational status)
  mutate(educa=replace(educa, educa==9, NA)) %>% 
  #renthom1 (own or rent home)
  mutate(renthom1=replace(renthom1, renthom1==7|renthom1==9, NA)) %>% 
  #veteran3 (veteran status)
  mutate(veteran3=replace(veteran3, veteran3==7|veteran3==9, NA)) %>% 
  mutate(veteran3=replace(veteran3, veteran3==2, 0)) %>% 
  #employ1 (employment status)
  mutate(employ1=replace(employ1, employ1==9, NA)) %>% 
  #children (# of children < 18 yo)
  mutate(children=replace(children, children==88, 0)) %>% 
  mutate(children=replace(children, children==99, NA)) %>% 
  #income3 (HH income)
  mutate(income3=replace(income3, income3==77|income3==99,NA)) 
  #x_age80 (no need to re-code)
  #x_metstat (metoropolintan status) No need for re-code 
  #x_urbstat (urban/rural status) No need for re-code
  #x_imprace (race/ethnicity) No need for re-coding
  #x_sex (sex) No need for re-coding
  #htm4 (Height in meter) No need for re-coding
  #wtkg3 (Weight in kg) No need for re-coding
  #x_bmi5 (BMI) No need for re-coding

################################################################################
# Rename variable names to better notations
brfss <- brfss %>% 
  rename(state = x_state) %>% 
  rename(ins_dummy = x_hlthpln) %>% 
  rename(diabete = diabete4) %>% 
  rename(sex = x_sex) %>% 
  rename(metro_status = x_metstat) %>% 
  rename(urban_status = x_urbstat) %>%
  rename(race = x_imprace) %>% 
  rename(education = educa) %>% 
  rename(home_rent_own = renthom1) %>% 
  rename(veteran = veteran3) %>% 
  rename(employ = employ1) %>% 
  rename(num_children = children) %>% 
  rename(income_gr = income3) %>% 
  rename(age = x_age80) %>% 
  rename(height_meter = htm4) %>% 
  rename(weight_kg = wtkg3) %>% 
  rename(bmi = x_bmi5) 
################################################################################
# Clean up the data
brfss <- brfss %>% filter(state <= 56) %>% 
         filter(age <= 79) %>% 
         mutate(height_meter = height_meter/100) %>% 
         mutate(weight_kg = weight_kg/100) %>% 
         mutate(bmi = bmi/100)

################################################################################
# Save the final data used for prediction.
write.csv(brfss,"cleaned_data/brfss_final.csv", row.names = FALSE)
