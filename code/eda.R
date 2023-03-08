##---------------------------
##
## Name: Exploratory Analysis
##
## Author: Emma Broadnax
##
## Last Updated: 03/01/2023
##
##---------------------------
##
## Description:
##
## Warnings:
##
## Notes:
##
##---------------------------

#####--------------------------- Add Libraries ---------------------------#####

library(ggridges)

#####--------------------------- Correlations ---------------------------#####

zproc <- df_cln %>% 
  select_if(is.numeric)

PerformanceAnalytics::chart.Correlation(zproc)

#####--------------------------- RFI ---------------------------#####

hist(df_cln$rfi_count)

# the number of RFI's has a relationship with contract size in terms of value and hours
df_cln %>% 
  mutate(is_outlier = if_else(rfi_count %in% out_rfi, 1, 0)) %>% 
  ggplot(aes(x = initial_contract_value, y = rfi_count, color = is_outlier)) +
  geom_point() 

df_cln %>% 
  ggplot(aes(x = recorded_project_hours, y = rfi_count)) +
  geom_point()

# the relationship to square footage is much weaker
df_cln %>% 
  ggplot(aes(x = square_footage, y = rfi_count)) +
  geom_point()

# moderate relationship of injuries and RFI count
df_cln %>% 
  ggplot(aes(x = recorded_project_injuries, y = rfi_count)) +
  geom_point()

# there isn't a particularly strong trend in customer experience and RFI count
df_cln %>% 
  ggplot(aes(x = as.factor(floor(overall_customer_experience_avg)), y = rfi_count)) +
  geom_boxplot()

# however, there is a stronger trend among team experience and RFI count
df_cln %>% 
  ggplot(aes(x = as.factor(floor(overall_project_team_experience_avg)), y = rfi_count)) +
  geom_boxplot()

#####--------------------------- Diagnostics: Outcome ---------------------------#####

df_outcome %>% 
  ggplot(aes(x = log_rfi_count, y = outcome)) +
  geom_point()

# is this relationship skewed because the underlying data has such a strong skew?
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = outcome)) +
  geom_point()

df_outcome %>% 
  ggplot(aes(x = log_hours, y = outcome)) +
  geom_point()

# large portion of zeros due to E&I projects
df_outcome %>% 
  ggplot(aes(x = square_footage, y = outcome)) +
  geom_point()

df_outcome %>% 
  ggplot(aes(x = current_final_contract_value, y = outcome)) +
  geom_point()

# this relationship has weakened, but maybe that is what we want.
df_outcome %>% 
  ggplot(aes(x = scale_margin, y = outcome)) +
  geom_point()

df_outcome %>% 
  ggplot(aes(x = recorded_project_injuries, y = outcome)) +
  geom_point()

df_outcome %>% 
  ggplot(aes(x = as.factor(overall_customer_experience_avg), y = outcome)) +
  geom_boxplot()

# potentially related to team experience?
df_outcome %>% 
  ggplot(aes(x = as.factor(floor(overall_project_team_experience_avg)), y = outcome)) +
  geom_boxplot()

df_outcome %>% 
  ggplot(aes(x = as.factor(floor(customer_team)), y = outcome)) +
  geom_boxplot()

df_outcome %>% 
  # the large number of projects with 0 injuries (good) confuses the trend here, which is not necessarily notable
  filter(trir > 0) %>% 
  # single outlier
  filter(trir < 40) %>% 
  ggplot(aes(x = trir, y = outcome)) +
  geom_point()

#####--------------------------- Diagnostics: Margin Deviation ---------------------------#####

# not noticeable trend in magnitude of deviation
# however, the negative points do tend to cluster as expected
df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = margin_deviation, y = outcome, color = is_positive)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = margin_deviation, y = outcome, color = current_margin_fcst_plan_sale)) +
  geom_point()

# margin deviations span project hours, with a few outliers (no change on log hours)
df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = recorded_project_hours, y = margin_deviation)) +
  geom_point()


df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = recorded_project_injuries, y = margin_deviation)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = initial_contract_value, y = margin_deviation)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = square_footage, y = margin_deviation)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = rfi_count, y = margin_deviation)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = customer_team, y = margin_deviation)) +
  geom_point()

df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = as.factor(overall_customer_experience_avg), y = margin_deviation)) +
  geom_boxplot()

# some relationship between margin and team experience, but odd behavior @ 1
df_outcome %>% 
  mutate(margin_deviation = abs(current_margin_fcst_plan_sale)) %>% 
  mutate(is_positive = if_else(current_margin_fcst_plan_sale > 0, 1, 0)) %>% 
  ggplot(aes(x = as.factor(floor(overall_project_team_experience_avg)), y = margin_deviation)) +
  geom_boxplot()

#####--------------------------- Project Size ---------------------------#####

# projects with higher hours or higher trir do have a lower outcome score
# which begs the question if they are lower in other areas
df_outcome %>% 
  filter(trir < 40) %>% 
  filter(recorded_project_hours < 7500000) %>% 
  ggplot(aes(x = recorded_project_hours, y = trir, color = outcome)) +
  geom_point()

df_outcome %>% 
  filter(trir < 40) %>% 
  filter(recorded_project_hours < 7500000) %>% 
  ggplot(aes(x = log_hours, y = trir, color = outcome)) +
  geom_point()

df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = current_margin_fcst_plan_sale)) +
  geom_point()

# has a loose linear trend
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = rfi_count)) +
  geom_point()

# no trend in rfi count by hours
df_outcome %>% 
  ggplot(aes(x = log_hours, y = log_rfi_count)) +
  geom_point()

# injuries follows an expected linear trend
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = recorded_project_injuries)) +
  geom_point()

# small projects tend to have higher customer experience) slightly)
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = as.factor(floor(overall_customer_experience_avg) ))) +
  geom_boxplot() +
  coord_flip()

# similarly, they tend to have a lower team experience
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = as.factor(floor(overall_project_team_experience_avg) ))) +
  geom_boxplot() +
  coord_flip()

# similarly, they tend to have a lower team experience
df_outcome %>% 
  mutate(grp_team_exp = floor(overall_project_team_experience_avg)) %>% 
  ggplot(aes(x = recorded_project_hours, y = overall_project_team_experience_avg, color = grp_team_exp)) +
  geom_area() 



df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = as.factor(floor(overall_project_team_experience_avg) ))) +
  geom_density_ridges_gradient()

df_outcome %>% 
  mutate(floor_rating = as.factor(floor(overall_project_team_experience_avg))) %>% 
  group_by(floor_rating) %>% 
  summarise(avg_hours = mean(recorded_project_hours, na.rm = TRUE),
            med_hours = median(recorded_project_hours, na.rm = TRUE)) %>% 
  ungroup()

df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = as.factor(floor(overall_customer_experience_avg) ))) +
  geom_density_ridges_gradient()

df_outcome %>% 
  mutate(floor_rating = as.factor(floor(overall_customer_experience_avg))) %>% 
  group_by(floor_rating) %>% 
  summarise(avg_hours = mean(recorded_project_hours, na.rm = TRUE),
            med_hours = median(recorded_project_hours, na.rm = TRUE)) %>% 
  ungroup()



# small projects tend to have higher customer experience) slightly)
df_outcome %>% 
  ggplot(aes(x = recorded_project_hours, y = scale_inj_contract )) +
  geom_point()

