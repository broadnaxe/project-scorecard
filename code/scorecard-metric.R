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

df <- read.xlsx("~\\Code\\project-scorecard\\data\\Scorecard KPIs - Completed Projects-2022-12-05-09-44-30.xlsx", 
                sheet = 2) %>% 
  clean_names() 

#####--------------------------- Clean Input -------------#####

df_cln <- df %>% 
  # remove total line
  # remove very small projects
  filter(!is.na(oracle_project_number) &
           recorded_project_hours > 99) %>% 
  # reformat margin forecast %, read in as character
  mutate(current_margin_fcst_plan_sale = as.numeric(current_margin_fcst_plan_sale),
         primary_og_og_name = as.factor(primary_og_og_name),
         primary_project_type = as.factor(primary_project_type)) %>% 
  dplyr::select(-x3, 
                -current_margin_percent,
                -expected_total_margin_percent_at_sale, 
                -final_construction_completion,
                -primary_project_sub_type,
                -project_size_mw)
  
# calculate mean values to use as benchmarks to remove outliers
# SEM is sensitive to outliers in input variables
mean_injuries <- mean(df_cln$recorded_project_injuries, 
                      na.rm = TRUE)
mean_margin <- mean(df_cln$current_margin_fcst_plan_sale, 
                    na.rm = TRUE)
mean_trir <- mean(df_cln$trir, 
                  na.rm = TRUE)
mean_contract_value <- mean(df_cln$initial_contract_value, 
                            na.rm = TRUE)
mean_hours <- mean(df_cln$recorded_project_hours, 
                   na.rm = TRUE)

# remove outliers
df_cln <- df_cln %>% 
  # exclude additional outlying projects
  filter(initial_contract_value <= (mean_contract_value + mean_contract_value*4) & 
           initial_contract_value >= (mean_contract_value - mean_contract_value*4)) %>% 
  # filter(trir <= (mean_trir + mean_trir*4) &
  #          trir >= (mean_trir - mean_trir*4)) %>%
   filter(recorded_project_hours <= (mean_hours + mean_hours*4) &
            recorded_project_hours >= (mean_hours - mean_hours*4)) %>%
  # exclude input column outliers
  filter(current_margin_fcst_plan_sale <= (mean_margin + mean_margin*4) & 
           current_margin_fcst_plan_sale >= (mean_margin - mean_margin*4)) %>% 
  filter(recorded_project_injuries <= (mean_injuries + mean_injuries*4) &
           recorded_project_injuries >= (mean_injuries - mean_injuries*4)) %>% 
  # remove records with more than four missing values
  mutate(nulls = rowSums(is.na(.))) %>% 
  filter(nulls <= 4)

#####--------------------------- Impute Nulls -------------#####

# remove character & factor values
df_impute <- subset(df_cln, 
                    select = -c(oracle_project_number,
                                primary_og_og_name,
                                primary_project_type))

zproc <- mice(df_impute, 
               m = 5, 
               maxit = 50, 
               meth = 'cart',
               seed = 123, 
               printFlag = FALSE)

#summary(zproc)
#densityplot(zproc)

df_complete <- complete(zproc)

#####--------------------------- Transform Input -------------#####

df_cln <- df_cln %>% 
  # transform project hours (since always positive) to consolidate distribution and remove skew
  mutate(log_hours = if_else(recorded_project_hours == 0, 0, log(recorded_project_hours)),
         scale_cust_exp = overall_customer_experience_avg,
         scale_inj_contract = if_else(recorded_project_injuries == 0, 0, log(recorded_project_injuries/initial_contract_value)),
         scale_margin = current_margin_fcst_plan_sale,
         scale_team_exp = overall_project_team_experience_avg) %>% 
  mutate_at(c("scale_cust_exp",
              "scale_inj_contract",
              "scale_margin",
              "scale_team_exp"), 
            ~(scale(., 
                    center = TRUE, 
                    scale = TRUE) %>%
                as.vector)) 

df_cln_no_missing <- df_cln %>% 
  filter(!is.na(scale_cust_exp) &
           !is.na(scale_team_exp)) 

df_complete <- df_complete %>% 
  # transform project hours (since always positive) to consolidate distribution and remove skew
  mutate(log_hours = if_else(recorded_project_hours == 0, 0, log(recorded_project_hours)),
         scale_cust_exp = overall_customer_experience_avg,
         scale_inj_contract =  if_else(recorded_project_injuries == 0, 0, log(recorded_project_injuries/initial_contract_value)),
         scale_margin = current_margin_fcst_plan_sale,
         scale_team_exp = overall_project_team_experience_avg) 
  
# compute mean values to use to center & scale
ref_cust_mean = mean(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)
ref_cust_sd = sd(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)

ref_inj_contract_mean = mean(df_cln_no_missing$scale_inj_contract, na.rm = TRUE)
ref_inj_contract_sd = sd(df_cln_no_missing$overall_customer_experience_avg, na.rm = TRUE)

ref_margin_mean = mean(df_cln_no_missing$current_margin_fcst_plan_sale, na.rm = TRUE)
ref_margin_sd = sd(df_cln_no_missing$current_margin_fcst_plan_sale, na.rm = TRUE)

ref_team_mean = mean(df_cln_no_missing$overall_project_team_experience_avg, na.rm = TRUE)
ref_team_sd = sd(df_cln_no_missing$overall_project_team_experience_avg, na.rm = TRUE)



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
                                  overall_project_team_experience_avg)) 
  

#####--------------------------- Estimate Outcome Variable (1) -------------#####

set.seed(1234)

model_code = "outcome =~ scale_margin + scale_cust_exp + scale_team_exp + log_hours + scale_inj_contract"

# use data without imputations to create equation
# maximum likelihood error with robust estimation; experience scores are skewed (despite scaling and centering)
fit = cfa(model_code, 
          data = df_cln_no_missing, 
          estimator = "MLM")

summary(fit, fit.measures = TRUE)

#####--------------------------- Compute Outcome Estimate -------------#####

estimates <- parameterestimates(fit)
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
           est_inj_contract*scale_inj_contract)

#####--------------------------- Create Output ---------------------------#####

write.xlsx(df_complete, "~\\Code\\project-scorecard\\data\\scorecard_output.xlsx")
