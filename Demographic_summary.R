## @knitr num2
#Extract needed demographic data from mimic
demography_mimic <- dbGetQuery(con,'SELECT DISTINCT ON (p.subject_id) p.subject_id, 
                                 a.ethnicity,
                                 a.marital_status
                                 FROM patients p
                                 INNER JOIN admissions a on p.subject_id=a.subject_id')
#Pull ODK dataframe to environment
ODK_data <- odata_submission_get()
#Create dataframe containing only needed columns from ODK
ODK_main <- ODK_data %>%
  select(subject_id,admitage_yrs,vaccinated, sex)

#Transform subject_id and age from ODK to integer datatypes and remove '_' from subject_id
ODK_main <- ODK_main %>%
  mutate(subject_id = gsub('_', '', subject_id))
ODK_main$subject_id <- as.integer(ODK_main$subject_id)

#Rename variables from ODK for easier interpretation
#Admit_age renamed to Age and sex to Gender
ODK_main <- ODK_main %>%
  rename(Vaccinated = vaccinated,
         Age = admitage_yrs,
         Gender=sex)

#inner_join ODK and mimic data on subject_id to select only patients on both databases    
odk_mimic_demography <-inner_join(ODK_main, demography_mimic, by = "subject_id")
#Divide ethnicity into white and others, the former makes up >70% of the population
odk_mimic_demography$ethnicity_group <- ifelse(odk_mimic_demography$ethnicity ==  "WHITE", "WHITE","OTHERS" ) 
#Summary of demographic details
odk_mimic_demography%>% select(Age,Vaccinated,Gender,ethnicity_group) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n}({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = ethnicity_group ~ "Ethnicity",
    type = all_dichotomous() ~ "categorical"
  )
  
  

 

