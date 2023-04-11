library(tidyverse)


options(scipen=999)



# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Masterthesis/Data")


# Load data 1 - LCS
household_all <- read_csv("TBD/hh_final_LCS.csv")

household_all <- household_all %>% 
  select(-1)



# Select variables
household <- household_all %>%
  select(hh_id, hh_expenditures_USD_2014, exp_CO2_national, burden_CO2_national, Income_Group_5, province, urban_1, electricitycon, electricitymains,
         electricitysource, lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, education_hhh, ethnicity_hhh, gender_hhh, own_stove, own_vehicle)


## Rename variables and set levels ####

### Provinces
household <- household %>%
  mutate(province = factor(recode(province,
                                  `1` = 'Western Cape',
                                  `2` = 'Eastern Cape',
                                  `3` = 'Northern Cape',
                                  `4` = 'Free State',
                                  `5` = 'KwaZulu-Natal',
                                  `6` = 'North West',
                                  `7` = 'Gauteng',
                                  `8` = 'Mpumalanga',
                                  `9` = 'Limpopo'), 
                           levels = c("Western Cape","Eastern Cape","Northern Cape","Free State",
                                      "KwaZulu-Natal","North West", "Gauteng","Mpumalanga","Limpopo")))


### Urban
household <- household %>%
  mutate(urban_1 = factor(recode(urban_1,
                                 `1` = 'Urban formal',
                                 `2` = 'Urban informal',
                                 `4` = 'Traditional area',
                                 `5` = 'Rural formal'),
                                 levels = c('Urban formal','Urban informal','Traditional area','Rural formal')))


### Ethnicity household head
household <- household %>%
  mutate(ethnicity_hhh = factor(recode(ethnicity_hhh,
                                       `1` = 'African Black',
                                       `2` = 'Coloured',
                                       `3` = 'Indian/Asian',
                                       `4` = 'White'),
                                levels = c('African Black','Coloured','Indian/Asian','White')))

### Gender household head
household <- household %>%
  mutate(gender_hhh = factor(recode(gender_hhh,
                                       `1` = 'Male',
                                       `2` = 'Female'),
                                levels = c('Male', 'Female')))


### Education ISCED codes
household <- household %>%
  mutate(education_hhh = factor(recode(education_hhh, 
                                       `1` = 'Pre-primary Education',
                                       `2` = 'Primary Education',
                                       `3` = 'Primary Education',
                                       `4` = 'Primary Education',
                                       `5` = 'Primary Education',
                                       `6` = 'Secondary Education',
                                       `7` = 'Secondary Education',
                                       `8` = 'Secondary Education',
                                       `9` = 'Secondary Education',
                                       `10` = 'Secondary Education',
                                       `11` = 'Secondary Education',
                                       `12` = 'Secondary Education',
                                       `13` = 'Secondary Education',
                                       `14` = 'Secondary Education',
                                       `15` = 'Post-secondary Non-tertiary Education',
                                       `16` = 'Post-secondary Non-tertiary Education',
                                       `17` = 'Post-secondary Non-tertiary Education',
                                       `18` = 'Post-secondary Non-tertiary Education',
                                       `19` = 'Post-secondary Non-tertiary Education',
                                       `20` = 'Post-secondary Non-tertiary Education',
                                       `21` = 'Post-secondary Non-tertiary Education',
                                       `22` = 'Post-secondary Non-tertiary Education',
                                       `23` = 'Post-secondary Non-tertiary Education',
                                       `24` = 'Post-secondary Non-tertiary Education',
                                       `25` = 'Short-cycle Tertiary Education',
                                       `26` = 'Short-cycle Tertiary Education',
                                       `27` = 'Bachelors or Equivalent',
                                       `28` = 'Bachelors or Equivalent',
                                       `29` = 'Bachelors or Equivalent',
                                       `30` = 'Masters or Equivalent',
                                       .default = 'NA'),
                                levels = c("Secondary Education",'Pre-primary Education','Primary Education',
                                           'Post-secondary Non-tertiary Education','Short-cycle Tertiary Education',
                                           'Bachelors or Equivalent','Masters or Equivalent')))


### Ownership stove, car
household <- household %>%
  mutate(own_stove = factor(recode(own_stove,
                                   `1` = 'Ownership',
                                   `3` = 'No Access',
                                   .default = 'NA'),
                            levels = c('Ownership', 'No Access')))

household <- household %>%
  mutate(own_vehicle = factor(recode(own_vehicle,
                                     `1` = 'Ownership',
                                     `3` = 'No Access',
                                     .default = 'NA'),
                              levels = c('Ownership', 'No Access')))


### Electricity source
household <- household %>%
  mutate(electricitysource = case_when (electricitycon ==2 ~ 'No Access',
                                        electricitymains == 1 ~ 'Mains',
                                        electricitysource == 1 ~'Other', #paid
                                        electricitysource == 6 ~ 'Other',
                                        electricitysource == 3 ~'Generator/Battery',
                                        electricitysource == 5 ~'Generator/Battery',
                                        electricitysource == 4 ~'Solar Energy',
                                        electricitysource == 2 ~'NA',
                                        electricitysource > 6 ~ 'NA')) %>%
  mutate(electricitysource = factor(electricitysource, levels = c('Mains','Other','Generator/Battery','Solar Energy', 'No Access')))


### Lighting fuel
household <- household %>%
  mutate(lightingsource = factor(recode(lightingsource,
                                        `1` = 'Electricity Mains',
                                        `2` = 'Electricity Other',
                                        `3` = 'Gas/Paraffin/Candles',
                                        `4` = 'Gas/Paraffin/Candles',
                                        `7` = 'Gas/Paraffin/Candles',
                                        `9` = 'Solar Energy',
                                        .default = 'NA'),
                                 levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Candles','Solar Energy')))


### Cooking fuel
household <- household %>%
  mutate(cookingsource = factor(recode(cookingsource,
                                       `1` = 'Electricity Mains',
                                       `2` = 'Electricity Other',
                                       `3` = 'Gas/Paraffin/Coal',
                                       `4` = 'Gas/Paraffin/Coal',
                                       `6` = 'Gas/Paraffin/Coal',
                                       `5` = 'Wood/Animal Dung',
                                       `8` = 'Wood/Animal Dung',
                                       `9` = 'Solar Energy',
                                       `11` = 'No Access',
                                       .default = 'NA'),
                                levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal', 'Wood/Animal Dung',
                                           'Solar Energy','No Access')))


### Water heating fuel
household <- household %>%
  mutate(heatingsourcewater = factor(recode(heatingsourcewater,
                                            `1` = 'Electricity Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas/Paraffin/Coal',
                                            `4` = 'Gas/Paraffin/Coal',
                                            `6` = 'Gas/Paraffin/Coal',
                                            `5` = 'Wood/Animal Dung',
                                            `8` = 'Wood/Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'NA'),
                                     levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal',
                                                'Wood/Animal Dung','Solar Energy','No Access')))

### Space heating fuel
household <- household %>%
  mutate(heatingsourcespace = factor(recode(heatingsourcespace,
                                            `1` = 'Electricity Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas/Paraffin/Coal',
                                            `4` = 'Gas/Paraffin/Coal',
                                            `6` = 'Gas/Paraffin/Coal',
                                            `5` = 'Wood/Animal Dung',
                                            `8` = 'Wood/Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'NA'),
                                     levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal',
                                                'Wood/Animal Dung','Solar Energy','No Access')))




# create log expenditures variable
household$log_expenditures_hh <- log(household$hh_expenditures_USD_2014)
  


## OLS ####
ols <- lm(burden_CO2_national ~  log_expenditures_hh + province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh +
             own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
             heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household)

summary(ols)

write.csv(summary(ols)['coefficients'],file='ols_result.csv')



## LOGIT ####
# households that are more affected than 90%

# Full sample
household_0 <- subset(household)

household_0$burden_decile <- ntile(household_0$burden_CO2_national, 10)

household_0 <- household_0 %>%
  mutate(burden_decile = as.factor(case_when(burden_decile <= 9 ~ FALSE,
                                             burden_decile == 10 ~ TRUE)))

household_0$log_expenditures_hh <- log(household_0$hh_expenditures_USD_2014)

# Create new tables with expenditure quintiles
quintiles <- c(1, 2, 3, 4, 5)

for (i in quintiles) {
  name <- paste0("household_", i)
  assign(name, household %>% filter(Income_Group_5 == i))
  
  var <- get(name)
  var$burden_decile <- ntile(var$burden_CO2_national, 10)
  var <- var %>%
    mutate(burden_decile = as.factor(case_when(burden_decile <= 9 ~ FALSE,
                                               burden_decile == 10 ~ TRUE)))
  var$log_expenditures_hh <- log(var$hh_expenditures_USD_2014)
  assign(name, var)
}



# logistic regression - data: household_0 = full sample , household_1 = first quintile, ...
log_reg <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
              own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
              heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household_1, family="binomial"(link=logit))

summary(log_reg)

# Mc Faddens R-squared
with(summary(log_reg), 1 - deviance/null.deviance)

# save results as csv
results <- summary(log_reg)$coefficients
variable_name <- rownames(results)
intercept <- results[, 1]
standard_error <- results[, 2]
signif <- results[, 4]
signif <- ifelse(signif <= 0.001, "***", ifelse(signif <= 0.01, "**", ifelse(signif <= 0.05, "*", "")))

log_table <- data.frame(variable_name, intercept, standard_error, signif)
write.csv(log_table, file="log_reg_result.csv")



