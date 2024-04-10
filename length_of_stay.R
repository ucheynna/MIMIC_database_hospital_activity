## @knitr num3
#Extract data from MIMIC for length of stay calculation(admission and ICU data)
#Inner join to capture only common patients
mimic_los <- dbGetQuery(con,'SELECT 
                        a.dischtime,
                        a.admittime,
                        a.subject_id,
                        i.los as icu_los
FROM admissions a
INNER JOIN icustays i on a.subject_id=i.subject_id')
# Group by to take only 1st occurrence of subject_id
mimic_los <- mimic_los %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()
# Calculate total length of stay by taking the difference between admit and discharge time in days
# Adjust for possible negative values by conditioning disch time to always come after admit time
mimic_los <- mimic_los %>%
  mutate(
    total_los=as.numeric(difftime(dischtime,admittime,units = 'days'))
  )
mimic_los <- mimic_los %>%
  mutate(
    admittime=if_else(total_los<0,
                      dischtime,admittime),
    dischtime=if_else(total_los<0,
                      admittime,dischtime),
    total_los=as.numeric(difftime(
      dischtime,admittime,units='days'
    ))
  )
#Merge with ODK_data, Inner join to capture only subjects in both data bases
los_data <-inner_join(ODK_main, mimic_los, by = "subject_id")
#Create function for calculating summary stats of total and ICU length of stay
calculate_summary_stats <- function(data, stay_column, stratification_label) {
  stay_col_name <- deparse(substitute(stay_column))
  data %>%
    summarise(
      median = median({{ stay_column }}, na.rm = TRUE),
      quantile_25 = quantile({{ stay_column }}, 0.25, na.rm = TRUE),
      quantile_75 = quantile({{ stay_column }}, 0.75, na.rm = TRUE),
      min = min({{ stay_column }}, na.rm = TRUE),
      max = max({{ stay_column }}, na.rm = TRUE)
    ) %>%
    mutate(
      stratification = stratification_label,
      stay_type = stay_col_name
    )
}
# total_los stratification by Gender
summary_gender_all <- calculate_summary_stats(los_data, total_los, "All Genders")
summary_gender_female <- calculate_summary_stats(los_data %>% filter(Gender == "Female"), total_los, "Female")
summary_gender_male <- calculate_summary_stats(los_data %>% filter(Gender == "Male"), total_los, "Male")
# total_los stratification by Vaccination Status
summary_vaccinated_all <- calculate_summary_stats(los_data, total_los, "All Vaccination doses")
summary_vaccinated_yes <- calculate_summary_stats(los_data %>% filter(Vaccinated == "YES"), total_los, "1+ doses")
summary_vaccinated_no <- calculate_summary_stats(los_data %>% filter(Vaccinated == "NO"), total_los, "0 dose")
# total_los stratification by Age
summary_age_all <- calculate_summary_stats(los_data, total_los, "All Ages")
summary_age_under_60 <- calculate_summary_stats(los_data %>% filter(Age < 60), total_los, "< 60")
summary_age_60_plus <- calculate_summary_stats(los_data%>% filter(Age >= 60), total_los, "> 60")
# total_los combined summary by binding rows
combined_total_los <- bind_rows(
  summary_gender_all, summary_gender_female, summary_gender_male,
  summary_vaccinated_all, summary_vaccinated_yes, summary_vaccinated_no,
  summary_age_all, summary_age_under_60, summary_age_60_plus
)

# icu_los stratification by Gender
summary_gender_all1 <- calculate_summary_stats(los_data, icu_los, "All Genders")
summary_gender_female1 <- calculate_summary_stats(los_data %>% filter(Gender == "Female"), icu_los, "Female")
summary_gender_male1 <- calculate_summary_stats(los_data %>% filter(Gender == "Male"), icu_los, "Male")
# icu_los stratification by Vaccination Status
summary_vaccinated_all1 <- calculate_summary_stats(los_data, icu_los, "All Vaccination doses")
summary_vaccinated_yes1 <- calculate_summary_stats(los_data %>% filter(Vaccinated == "YES"), icu_los, "1+ doses")
summary_vaccinated_no1 <- calculate_summary_stats(los_data %>% filter(Vaccinated == "NO"), icu_los, "0 dose")
# icu_los stratification by Age
summary_age_all1 <- calculate_summary_stats(los_data, icu_los, "All Ages")
summary_age_under_601 <- calculate_summary_stats(los_data %>% filter(Age < 60), icu_los, "< 60")
summary_age_60_plus1 <- calculate_summary_stats(los_data%>% filter(Age >= 60), icu_los, "> 60")
# icu_los combined summary by binding rows
combined_icu_los1 <- bind_rows(
  summary_gender_all1, summary_gender_female1, summary_gender_male1,
  summary_vaccinated_all1, summary_vaccinated_yes1, summary_vaccinated_no1,
  summary_age_all1, summary_age_under_601, summary_age_60_plus1
)

#Drop columns 'stay_type' and reorder columns
combined_total_los <- combined_total_los %>%
  select(-stay_type)
combined_icu_los1 <- combined_icu_los1 %>%
  select(-stay_type)
combined_total_los<- combined_total_los[, c("stratification", "median", "quantile_25","quantile_75","min","max")]
combined_icu_los1<- combined_icu_los1[, c("stratification", "median", "quantile_25","quantile_75","min","max")]
#Round numbers to 1 decimal place  
combined_total_los <- combined_total_los %>%
  mutate_if(is.numeric, ~round(., 1))
combined_icu_los1 <- combined_icu_los1 %>%
  mutate_if(is.numeric, ~round(., 1))
#Tables for total and ICU lengths of stay
kable(combined_total_los,caption = 'Summary of total length of stay(days) across stratifications', longtable = T,
      col.names = c("Stratification",
                    "Median",
                    "25% Quantile",
                    "75% Quantile", "Minimum","Maximum")) %>%
  kable_styling(font_size = 9) %>% row_spec(0, font_size=9)
kable(combined_icu_los1, caption = 'Summary of ICU length of stay(days) across stratifications', longtable = T,
      col.names = c("Stratification",
                    "Median",
                    "25% Quantile",
                    "75% Quantile", "Minimum","Maximum")) %>%
  kable_styling(font_size = 9) %>% row_spec(0, font_size=9)


