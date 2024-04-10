## @knitr num5
# select those that died in ICU plus cardiac device from previously extracted data frame (odk_mimic_icu)
icu_death <- odk_mimic_icu[odk_mimic_icu$icu_outcome == "Died", ]
#Drop unneded columns
icu_death<- icu_death %>%
  select(-icu_outcome,-Vaccinated)
#Round length of stay to 1dp
icu_death <- icu_death %>%
  mutate(los = round(los, 1))
# Using str_trunc, limit character length of icd9 codes and diagnosis
icu_death$short_title <- stringr::str_trunc(icu_death$short_title, 45)
icu_death$icd9 <- stringr::str_trunc(icu_death$icd9, 20)
#Table for patients that died in ICU with a cardiac device
kable(icu_death,caption = 'Patients with a cardiac device that died in ICU', longtable = T,
      col.names = c("ID",
                    "Admit age",
                    "Gender",
                    "ICD9 code",
                    "Diagnosis", "First Care Unit","ICU stay(days)")) %>%
  kable_styling(font_size = 6) %>% row_spec(0, font_size=6)

