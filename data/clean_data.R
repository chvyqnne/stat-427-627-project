library(tidyverse)

data <- read_csv("data/data.csv")

# change column names

colnames(data) <- c(
  "country",
  "economy_code",
  "iso_code",
  "region",
  "income_group",
  "year",
  "wbl_index",
  "mobility",
  "living_choice",
  "travel_outside_home",
  "passport_apply",
  "travel_outside_country",
  "workplace",
  "get_job",
  "law_gender_discr",
  "employ_harrass_legisl",
  "employ_harrass_crim_penalty",
  "pay",
  "equal_pay",
  "night_work",
  "dangerous_job",
  "industrial_job",
  "marriage",
  "law_married_obey",
  "head_of_hh",
  "legisl_dom_violence",
  "obtain_judgement_divorce",
  "rights_remarry",
  "parenthood",
  "mother_paid_leave_avail",
  "paid_mat_leave_length",
  "gov_benefits_mat_leave",
  "father_paid_leave_avail",
  "paid_pat_leave_length",
  "paid_parental_leave_avail",
  "shared_days",
  "mother_num_days",
  "father_num_days",
  "dismiss_preg_workers",
  "entrepreneurship",
  "credit_access_gender_discrim",
  "sign_contract",
  "register_business",
  "open_bank_acc",
  "assets",
  "immov_prop_ownership",
  "children_inherit_assets",
  "surviving_spouses_inherit_assets",
  "spouses_admin_authority_assets",
  "valuation_nonmon_contrib",
  "pension",
  "retire_age_full_benefits",
  "retire_age_partial_benefits",
  "mandatory_retire_age",
  "pension_ben_childcare_absence",
  "total",
  "overnight",
  "oneday",
  "cruise",
  "expenditures"
)

# change data types to factors
# change values represented by thousands to their actual values

data <- data %>% 
  mutate(across(c(4:5,9:22,24:28,30,32:33,35,39,41:44,46:50,52:55), as.factor),
         expenditures = as.numeric(expenditures)) %>% 
  mutate(total = total * 1000,
         overnight = overnight * 1000,
         oneday = oneday * 1000,
         cruise = cruise * 1000,
         expenditures = expenditures * 1000)

# save to new dataset
write_csv(data, "data/clean_data.csv")
