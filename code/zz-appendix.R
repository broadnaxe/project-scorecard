##---------------------------
##
## Name: Project Scorecard Metric
##
## Author: Emma Broadnax
##
## Last Updated: 1/05/2023
##
##---------------------------
##
## Description: EDA and diagnostic data for scorecard metric work
##
## Warnings:
##
## Notes:
##
##---------------------------

#####--------------------------- Load Additional Libraries ---------------------------#####

library(naniar) # null data
library(semPlot) # visualize SEM 
library(PerformanceAnalytics)
library(car)

#####--------------------------- Visualized Model ---------------------------#####

# create a visual representation of the model created
semPaths(object = fit,
         what = "path",
         whatLabels = "par",
         style = "ram",
         layout = "tree",
         rotation = 2,
         sizeMan = 7,
         sizeLat = 7,
         color = "lightgray",
         edge.label.cex = 1.2,
         label.cex = 1.3)

#####--------------------------- Evaluate Outcome Estimate  -------------#####

hist(df_complete$outcome)
summary(df_complete$outcome)

df_complete %>% 
  ggplot(aes(x = recorded_project_hours, y = outcome, color = recorded_project_injuries)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = log_hours, y = outcome, color = recorded_project_injuries)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = recorded_project_injuries, y = outcome)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = square_footage, y = outcome, color = recorded_project_injuries)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = initial_contract_value, y = outcome, color = recorded_project_injuries)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = current_final_contract_value, y = outcome, color = recorded_project_injuries)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = trir, y = outcome, color = current_margin_fcst_plan_sale)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = current_margin_fcst_plan_sale, y = outcome)) +
  geom_point()

df_complete %>% 
  ggplot(aes(x = as.factor(overall_customer_experience_avg), y = outcome)) +
  geom_boxplot()

df_complete %>% 
  ggplot(aes(x = as.factor(overall_project_team_experience_avg), y = outcome)) +
  geom_boxplot()

df_complete %>% 
  ggplot(aes(x = recorded_project_hours, y = as.factor(overall_customer_experience_avg))) +
  geom_point()

# team experience decreases, on average, as project hours increase
df_complete %>% 
  mutate(grp_project_hours = cut(recorded_project_hours, 15)) %>%
  group_by(grp_project_hours) %>% 
  summarise(avg_experience = round(mean(overall_project_team_experience_avg, na.rm = TRUE),1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = grp_project_hours, y = as.factor(avg_experience))) +
  geom_bar(stat = "identity") +
  coord_flip()

# though inconsistently, customer experience also tends to decrease as project hours increase
df_complete %>% 
  mutate(grp_project_hours = cut(recorded_project_hours, 15)) %>%
  group_by(grp_project_hours) %>% 
  summarise(avg_experience = round(mean(overall_customer_experience_avg, na.rm = TRUE),1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = grp_project_hours, y = as.factor(avg_experience))) +
  geom_bar(stat = "identity") +
  coord_flip()

#####--------------------------- Additional Diagnostics -------------#####

# check for consistent variances (~factor of 10)
var(df_cln_no_missing$scale_margin)
var(df_cln_no_missing$scale_cust_exp)
var(df_cln_no_missing$scale_team_exp)
var(df_cln_no_missing$log_hours)
var(df_cln_no_missing$scale_inj_contract)

# check distributions of features used
# because of skew persistent (particularly in log & survey data that can't be removed), adjusted estimator to robust option
hist(df_cln_no_missing$scale_margin)
hist(df_cln_no_missing$scale_cust_exp)
hist(df_cln_no_missing$scale_team_exp)
hist(df_cln_no_missing$log_hours)
hist(df_cln_no_missing$scale_inj_contract)


# COMPUTE CORRELATIONS
df_corr <- df_cln_no_missing %>% 
  filter(!is.na(trir)) %>% 
  filter(!is.na(initial_contract_value)) %>% 
  dplyr::select(scale_margin, 
                scale_cust_exp, 
                scale_team_exp, 
                log_hours,
                initial_contract_value,
                recorded_project_injuries,
                scale_inj_contract)

# graph correlation matrix
chart.Correlation(cor(df_corr))

# compute partial correlations
pcor(df_corr)

# compute the covariance matrix
cov(df_corr)


# COMPUTE VIF
# check for consistent VIFs
set.seed(1234)

model <- lm(outcome ~ scale_margin + 
              scale_cust_exp + 
              scale_team_exp + 
              log_hours +
              scale_inj_contract, data = df_complete)

vif(model)


# EVALUATE MISSINGNESS
# need to evaluate type of missing-ness within the data to use mice imputation
n_miss(df_cln)
prop_miss(df_cln)
miss_var_summary(df_cln)
gg_miss_var(df_cln)

# Where are missings located?
vis_miss(df_cln) + 
  theme(axis.text.x = element_text(angle=75))

gg_miss_upset(df_cln)

explanatory <- c("overall_project_team_experience_avg", "overall_customer_experience_avg")


cln_df %>% 
  dplyr::select(explanatory) %>% 
  MissMech::TestMCARNormality()

#####--------------------------- Create Clusters ---------------------------#####

library(factoextra)
library(cluster)

df_cluster <- df_cln_no_missing %>% 
  select(-square_footage) %>% 
  select(is.numeric)

km <- kmeans(df_cluster, 2, iter.max = 10, nstart = 1)

fviz_cluster(km, data = df_cluster)
aggregate(df_cluster, by=list(cluster=km$cluster), mean)



#####--------------------------- Not Used ---------------------------#####

# Additional converging model that has less of a penalty for large projects, 
# but may not accurately capture size by using square footage.  
#  In this model, the value of team experience is over-valued compared to customer experience
# model_code = "outcome =~ scale_margin + scale_cust_exp + scale_team_exp + recorded_project_injuries + scale_sqrt_sqft"

# Computed a non-scaled version of the final model; assumptions prefer that data be centered and variances be
# relatively consistent, however raw data is also preferred.  For ongoing use, scaling the data is the most
# maintainable option for this; these models don't differ significantly, so feel it is appropriate to scale the data in 
# this case.
# model_code = "outcome =~ current_margin_fcst_plan_sale + overall_customer_experience_avg + 
#                           overall_project_team_experience_avg + log_hours + injury_contract_value"


# as hours increase, project team experience tends to decrease
df_cln_no_missing %>% 
  mutate(group = cut(recorded_project_hours, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# as hours increase, customer team experience tends to decrease (though less consistently)
df_cln_no_missing %>% 
  mutate(group = cut(recorded_project_hours, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_customer_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# No clear relationship with contract value and team experience
df_cln_no_missing %>% 
  mutate(group = cut(current_margin_fcst_plan_sale, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# unclear relationship between contract value and customer experience; may be unrelated
df_cln_no_missing %>% 
  mutate(group = cut(square_footage, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(current_margin_fcst_plan_sale, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# No clear relationship with contract value and team experience
df_cln_no_missing %>% 
  mutate(group = cut(recorded_project_hours, 10)) %>% 
  group_by(group, primary_og_og_name) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~primary_og_og_name)

# unclear relationship between contract value and customer experience; may be unrelated
df_cln_no_missing %>% 
  mutate(group = cut(recorded_project_injuries, 10)) %>% 
  group_by(group, primary_og_og_name) %>% 
  summarise(avg_value = mean(overall_customer_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~primary_og_og_name)

in_eda <- read.xlsx("~\\Code\\project-scorecard\\data\\TCH Data.xlsx", 
                    sheet = "Data Pivot") %>% 
  clean_names() 

df_eda <- in_eda %>% 
  left_join(df_cln_no_missing, by = c("row_labels"="oracle_project_number"))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_ot_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_customer_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_ot_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_dt_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_customer_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_dt_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_nt_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_customer_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

df_eda %>% 
  mutate(group = cut(average_of_act_craft_nt_percent, 10)) %>% 
  group_by(group) %>% 
  summarise(avg_value = mean(overall_project_team_experience_avg, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = group, y = avg_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

anti_join(in_eda, df_cln_no_missing, by = c("row_labels" = "oracle_project_number"))

anti_join(df_cln_no_missing, in_eda, by = c("oracle_project_number" ="row_labels"))


