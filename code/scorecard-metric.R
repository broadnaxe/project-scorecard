##---------------------------
##
## Name: Project Scorecard Metric
##
## Author: Emma Broadnax
##
## Last Updated: 1/04/2023
##
##---------------------------
##
## Description: Creates a project score "Outcome" based on project data
##
## Warnings: 
##
## Notes: Exploratory and diagnostic data located in "zz-appendix" file
##
##---------------------------

#####--------------------------- Load Libraries -------------#####

library(openxlsx)
library(tidyverse)
library(janitor)
library(lavaan) # SEM procedures
library(mice) # imputation

#####--------------------------- Read Input -------------#####

df <- read.xlsx("T:\\Data Analytics & Insights\\project-scorecard\\data\\Scorecard KPIs - Completed Projects-2022-12-05-09-44-30.xlsx", 
                sheet = 2) %>% 
  clean_names() 

df_rfi <- read.xlsx("T:\\Data Analytics & Insights\\project-scorecard\\data\\RFIs by Project.xlsx") %>% 
  clean_names() 

df_rfi_map <- read.xlsx("T:\\Data Analytics & Insights\\project-scorecard\\data\\Oracle-Procore Project Mapping.xlsx") %>% 
  clean_names() 


#####--------------------------- Clean Input -------------#####

# clean RFI data
cln_rfi <- df_rfi %>% 
  left_join(df_rfi_map, by = c("project_number")) %>% 
  mutate(cln_project_number = str_extract(project_number, "[0-9]{8}")) %>% 
  group_by(project_number, group_project_number) %>% 
  summarise(rfi_count = n()) %>% 
  ungroup() 

# projects with RFI's
df_join_rfi <- df %>% 
  left_join(cln_rfi, by = c("oracle_project_number" = "group_project_number")) %>% 
  filter(!is.na(rfi_count))

# projects without RFI's
df_no_rfi <- df %>% 
  anti_join(cln_rfi, by = c("oracle_project_number" =  "group_project_number"))

# combine into one file
df_cln <- df_join_rfi %>%
  bind_rows(df_no_rfi) %>% 
  # remove total line
  filter(!is.na(oracle_project_number) &
           # remove projects that don't have a margin value
           current_margin_fcst_plan_sale != "#Error!" &
           # remove small projects
           recorded_project_hours > 99) %>% 
  # reformat margin forecast %, read in as character
  mutate(current_margin_fcst_plan_sale = as.numeric(current_margin_fcst_plan_sale),
         primary_og_og_name = as.factor(primary_og_og_name),
         primary_project_type = as.factor(primary_project_type)) %>% 
  # if no rfi was found in the file, set rfi count to 0
  mutate(if_else(is.na(rfi_count), 0, as.numeric(rfi_count) ) ) %>% 
  mutate(log_inj_contract = case_when(is.na(recorded_project_injuries) ~ 0,
                                      recorded_project_injuries == 0 ~ 0,
                                      TRUE ~ log(recorded_project_injuries)/log(recorded_project_hours)  ),
         log_rfi_contract = case_when(is.na(rfi_count) ~ 0,
                                      rfi_count == 0 ~ 0,
                                      TRUE ~ rfi_count/recorded_project_hours),
         log_hours = log(recorded_project_hours))  %>% 
  dplyr::select(-x3, 
                -current_margin_percent,
                -expected_total_margin_percent_at_sale, 
                -final_construction_completion,
                -primary_project_sub_type,
                -primary_project_type,
                -project_size_mw)
  
# Clean outliers; SEM is sensitive to outliers in input variables
out_rfi_contract <- boxplot(df_cln$log_rfi_contract)$out
out_hours <- boxplot(df_cln$log_hours)$out
out_injuries <- boxplot(df_cln$recorded_project_injuries)$out
out_inj_contract <- boxplot(df_cln$log_inj_contract)$out
out_margin <- boxplot(df_cln$current_margin_fcst_plan_sale)$out

mean_margin <- mean(df_cln$current_margin_fcst_plan_sale, 
                    na.rm = TRUE)
mean_contract_value <- mean(df_cln$initial_contract_value, 
                            na.rm = TRUE)

# remove outliers
df_cln <- df_cln %>% 
  # exclude additional outlying projects
  filter(!(log_rfi_contract %in% out_rfi_contract)) %>% 
  filter(!(log_hours %in% out_hours)) %>% 
 # filter(!(recorded_project_injuries %in% out_injuries)) %>% 
 # filter(!(current_margin_fcst_plan_sale %in% out_margin)) %>% 
  filter(current_margin_fcst_plan_sale <= (mean_margin + mean_margin*4) &
           current_margin_fcst_plan_sale >= (mean_margin - mean_margin*4)) %>%
  filter(initial_contract_value <= (mean_contract_value + mean_contract_value*4) &
           initial_contract_value >= (mean_contract_value - mean_contract_value*4)) %>%
  # exclude input column outliers
  # remove records with more than four missing values
  mutate(nulls = rowSums(is.na(.))) %>% 
  filter(nulls <= 4)

#####--------------------------- Impute Nulls -------------#####

# remove character & factor values
df_impute <- subset(df_cln, 
                    select = -c(oracle_project_number,
                                primary_og_og_name))

zproc <- mice(df_impute, 
               m = 5, 
               maxit = 50, 
               meth = 'cart',
               seed = 123, 
               printFlag = FALSE)

# summary(zproc)
# densityplot(zproc)

df_complete <- complete(zproc)

#####--------------------------- Transform Input -------------#####

df_cln <- df_cln %>% 
  # transform project hours (since always positive) to consolidate distribution and remove skew
  mutate(scale_cust_exp = overall_customer_experience_avg,
         #scale_inj_contract = if_else(recorded_project_injuries == 0, 0, log(recorded_project_injuries/initial_contract_value)),
         scale_inj_contract = log_inj_contract,
         scale_margin = current_margin_fcst_plan_sale,
         scale_hours = log_hours,
         scale_team_exp = overall_project_team_experience_avg,
         scale_rfi_contract = log_rfi_contract) %>% 
  mutate_at(c("scale_cust_exp",
              "scale_inj_contract",
              "scale_hours",
              "scale_margin",
              "scale_team_exp",
              "scale_rfi_contract"), 
            ~(scale(., 
                    center = TRUE, 
                    scale = TRUE) %>%
                as.vector)) 

df_cln_no_missing <- df_cln %>% 
  filter(!is.na(scale_cust_exp) )#&
     #      !is.na(scale_team_exp)) %>% 


df_complete <- df_complete %>% 
  # transform project hours (since always positive) to consolidate distribution and remove skew
  mutate(scale_cust_exp = overall_customer_experience_avg,
         scale_inj_contract =  if_else(recorded_project_injuries == 0, 0, log(recorded_project_injuries/initial_contract_value)),
         scale_margin = current_margin_fcst_plan_sale,
         scale_team_exp = overall_project_team_experience_avg,
         scale_log_rfi_count = log_rfi_count) 
  
# compute mean values to use to center & scale
# want to use center & scale for training data so model coefficients are appropriate
ref_cust_mean = mean(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)
ref_cust_sd = sd(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)

ref_inj_contract_mean = mean(df_cln_no_missing$scale_inj_contract, na.rm = TRUE)
ref_inj_contract_sd = sd(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)

ref_margin_mean = mean(df_cln_no_missing$current_margin_fcst_plan_sale, na.rm = TRUE)
ref_margin_sd = sd(df_cln_no_missing$current_margin_fcst_plan_sale, na.rm = TRUE)

ref_team_mean = mean(df_cln_no_missing$overall_project_team_experience_avg, na.rm = TRUE)
ref_team_sd = sd(df_cln_no_missing$overall_project_team_experience_avg, na.rm = TRUE)

ref_rfi_mean = mean(df_cln_no_missing$scale_log_rfi_count, na.rm = TRUE)
ref_rfi_sd = sd(df_cln_no_missing$scale_log_rfi_count, na.rm = TRUE)



# add values for scaled variables
df_complete <- df_complete %>%  
  mutate(scale_cust_exp = if_else(is.na(overall_customer_experience_avg),
                                  (overall_customer_experience_avg-ref_cust_mean)/ref_cust_sd,
                                  overall_customer_experience_avg),
         scale_inj_contract = if_else(is.na(scale_inj_contract),
                                (scale_inj_contract-ref_inj_contract_mean)/ref_inj_contract_sd,
                                scale_inj_contract),
         scale_margin = if_else(is.na(current_margin_fcst_plan_sale),
                                (current_margin_fcst_plan_sale-ref_margin_mean)/ref_margin_sd,
                                current_margin_fcst_plan_sale),
         scale_team_exp = if_else(is.na(overall_project_team_experience_avg),
                                  (overall_project_team_experience_avg-ref_team_mean)/ref_team_sd,
                                  overall_project_team_experience_avg),
         scale_rfi_count = if_else(is.na(scale_log_rfi_count),
                                   (scale_log_rfi_count-ref_rfi_mean)/ref_rfi_sd,
                                   scale_log_rfi_count)) 
  

#####--------------------------- Estimate Outcome Variable (1) -------------#####

set.seed(1234)

model_code = "outcome =~ scale_margin + scale_cust_exp + scale_team_exp + log_hours + scale_inj_contract"

# use data without imputations to create equation
# maximum likelihood error with robust estimation; experience scores are skewed (despite scaling and centering)
fit = cfa(model_code, 
          data = df_cln_no_missing, 
          estimator = "MLM")

summary(fit, fit.measures = TRUE)

#####--------------------------- Estimate Outcome Variable (new) -------------#####

set.seed(1234)

model_code = "outcome =~ scale_margin + scale_cust_exp + scale_team_exp + log_hours + scale_inj_contract + scale_log_rfi_count"

model_code = "outcome =~ scale_margin + scale_cust_exp + scale_hours + scale_inj_contract + scale_rfi_contract "


#model_code = "outcome =~ scale_margin  + log_hours + scale_inj_contract + scale_log_rfi_count"


# use data without imputations to create equation
# maximum likelihood error with robust estimation; experience scores are skewed (despite scaling and centering)
fit = cfa(model_code, 
          data = df_cln_no_missing, 
          estimator = "MLM")


summary(fit, fit.measures = TRUE)


#####--------------------------- Compute Outcome Estimate -------------#####

estimates <- parameterestimates(fit)
est_customer_team <-  estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_customer_team"]
est_rfi_count <-  estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_log_rfi_count"]

est_margin <- estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_margin"]
est_cust_exp <- estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_cust_exp"]
est_team_exp <- estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_team_exp"]
est_hours <- estimates$est[estimates$lhs == "outcome" & estimates$rhs == "log_hours"]
est_inj_contract <- estimates$est[estimates$lhs == "outcome" & estimates$rhs == "scale_inj_contract"]

df_complete <- df_complete %>% 
  mutate(outcome = 
           est_margin*scale_margin + 
           est_cust_exp*scale_cust_exp +
           est_team_exp*scale_team_exp +
           est_hours*log_hours +
           est_inj_contract*scale_inj_contract +
           est_rfi_count*scale_log_rfi_count)

df_outcome <- df_cln_no_missing %>% 
  mutate(outcome = 
           est_margin*scale_margin + 
           est_cust_exp*scale_cust_exp +
           est_team_exp*scale_team_exp +
           est_hours*log_hours +
           est_inj_contract*scale_inj_contract +
           est_rfi_count*log_rfi_count)

#####--------------------------- Create Output ---------------------------#####

write.xlsx(df_complete, "~\\Code\\project-scorecard\\data\\scorecard_output_new.xlsx")
