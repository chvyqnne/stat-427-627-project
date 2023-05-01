library(tidyverse)
library(dplyr)
library(car)
library(leaps)
library(broom)

# LOAD DATA

data <- read_csv("./data/clean_data.csv")
data <- data %>% 
  mutate(across(c(4:5,9:22,24:28,30,32:33,35,39,41:44,46:50,52:55), as.factor))

# V1: REGRESSION WITH INDIVIDUAL YES/NO QUESTIONS (EXPENDITURES ~ .)

dataV1 <- select(data, -economy_code, -iso_code, -wbl_index,
                 -mobility, -workplace, -pay, -marriage, -parenthood,
                 -entrepreneurship, -pension, -assets, -total, -overnight, 
                 -oneday, -cruise, -country)
dataV1 <- filter(dataV1, !is.na(expenditures), !is.na(income_group), !is.na(region))

# REGRESSION

regV1 <- lm(expenditures ~ ., dataV1)
summary(regV1)
vif(regV1) # check multicollinearity

# VARIABLE SELECTION
# forward selection
regV1_null <- lm(expenditures ~ 1, dataV1)
step_out <- step(regV1_null, 
                 scope = list(lower = regV1_null, upper = regV1),
                 method = "forward")
summary(step_out)

# new model using variables chosen by forward selection

regV1_new <- lm(formula = expenditures ~ income_group + employ_harrass_legisl + 
                  immov_prop_ownership + mother_paid_leave_avail + 
                  paid_mat_leave_length + year + equal_pay + paid_parental_leave_avail + 
                  industrial_job + mother_num_days + pension_ben_childcare_absence + 
                  dismiss_preg_workers + sign_contract + region + children_inherit_assets + 
                  retire_age_partial_benefits + register_business + obtain_judgement_divorce + 
                  dangerous_job + travel_outside_country + travel_outside_home + 
                  legisl_dom_violence + retire_age_full_benefits + law_gender_discr + 
                  employ_harrass_crim_penalty + passport_apply + father_num_days + 
                  paid_pat_leave_length + father_paid_leave_avail, data = dataV1)

# take away region for multicollinearity
regV1_new_2 <- lm(formula = expenditures ~ income_group + employ_harrass_legisl + 
                  immov_prop_ownership + mother_paid_leave_avail + 
                  paid_mat_leave_length + year + equal_pay + paid_parental_leave_avail + 
                  industrial_job + mother_num_days + pension_ben_childcare_absence + 
                  dismiss_preg_workers + sign_contract + children_inherit_assets + 
                  retire_age_partial_benefits + register_business + obtain_judgement_divorce + 
                  dangerous_job + travel_outside_country + travel_outside_home + 
                  legisl_dom_violence + retire_age_full_benefits + law_gender_discr + 
                  employ_harrass_crim_penalty + passport_apply + father_num_days + 
                  paid_pat_leave_length + father_paid_leave_avail, data = dataV1)


# GOODNESS OF FIT TESTS
# Full Model
qqPlot(regV1)
print(glance(regV1))

# Reduced Model
qqPlot(regV1_new)
print(glance(regV1_new))
vif(regV1_new) # "region" has vif of 211
qqPlot(regV1_new_2)

# comparing full and reduced models with partial F-test
an_out_V1 <- anova(regV1_new, regV1)
an_out_V1 #reduced model slightly better fit

tibble(bind_rows(glance(regV1), glance(regV1_new)))

# V2: REGRESSION WITH WEIGHTED INDICATOR SCORES

dataV2 <- select(data, region, income_group, year, mobility, 
                 workplace, pay, marriage, parenthood, entrepreneurship, 
                 assets, pension, expenditures)
dataV2 <- drop_na(dataV2)

# regression

regV2 <- lm(expenditures ~ ., dataV2)
vif(regV2) # high multicollinearity present

# VARIABLE SELECTION
# Exhaustive Search
regV2_ex <- regsubsets(expenditures ~ ., dataV2)
summary(regV2_ex)
regV2_ex_summary <- summary(regV2_ex)

# ES Adjusted R Squared
which.max(regV2_ex_summary$adjr2) # 8 var
regV2_ex_summary$which[8,][-1] # region, income_group, workplace, assets, year

# ES BIC
round(regV2_ex_summary$bic,0)
which.max(regV2_ex_summary$bic) # The worst by BIC (1)
which.min(regV2_ex_summary$bic) # The best by BIC (8)
regV2_ex_summary$which[8,][regV2_ex_summary$which[8,] == TRUE][-1] # region, income_group, workplace, year, assets

# ES Mallows CP
round(regV2_ex_summary$cp, 2)
which.max(abs(regV2_ex_summary$cp - 1:8)) # The worst by Cp (1)
which.min(abs(regV2_ex_summary$cp - 1:8)) # The best by Cp (8)
names(regV2_ex_summary$which[8,][regV2_ex_summary$which[8,] == TRUE])[-1] # region, income_group, year, workplace, assets

# forward selection
step_out_V2_for <- step(regV2_null, 
                    scope = list(lower = regV2_null, upper = regV2),
                    method = "forward")

# forward selection model minus "region" variable, which is contributing to high multicollinearity
regV2_new <- lm(formula = expenditures ~ workplace + income_group + 
     marriage + pay + year + parenthood + pension + assets + mobility, 
   data = dataV2)

vif(regV2_new) # all VIFs under 3

# comparing full and reduced models with partial F-test
an_out_V2 <- anova(regV2_new, regV2)
an_out_V2
# Model 2 is a better fit (Full) with a significantly lower RSS than Model 1 (Reduced)

