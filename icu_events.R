## @knitr num4
#Extract data of patients that died in ICU as defined by time interval, class as icu_outcome
icu_event <- dbGetQuery(con,"SELECT DISTINCT ON (a.subject_id) a.subject_id, 
                                   string_agg(DISTINCT a.icd9_code, ', ') as icd9, 
                                   string_agg(DISTINCT b.short_title, ', ') as short_title,
                                   CASE WHEN MAX(c.deathtime) BETWEEN MAX(d.intime)-interval'6 hours' AND MAX(d.outtime)+interval'6 hours' THEN 'Died'
	                                 ELSE 'Survived' END AS icu_outcome, 
	                                 d.first_careunit, d.los
                                   FROM
                                   diagnoses_icd a
                            INNER JOIN d_icd_diagnoses b ON a.icd9_code=b.icd9_code
                            INNER JOIN admissions c ON a.subject_id = c.subject_id
                            INNER JOIN icustays d ON a.subject_id = d.subject_id
                            GROUP BY a.subject_id, d.outtime, c.deathtime, d.first_careunit, d.los
                            ORDER BY a.subject_id, d.outtime DESC, c.deathtime DESC")
#Note that icu spells per admission~d.los
# Filter those that have a cardiac device
icu_event <- icu_event %>% filter(str_detect(icd9, 'V450'))
#Join with odk data
odk_mimic_icu <-inner_join(ODK_main, icu_event, by = "subject_id")
# Create box plots for each categorical variable in one line
y_limits <- c(0, 10)
par(mfrow = c(1, 3))
boxplot(los ~ Gender, data = odk_mimic_icu, ylim = y_limits,ylab = "ICU staytime(days)", xlab = "Gender")
boxplot(los ~ Vaccinated, data = odk_mimic_icu, ylim = y_limits,ylab = "ICU staytime(days)", xlab = "Vaccinated")
boxplot(los ~ icu_outcome, data = odk_mimic_icu, ylim = y_limits,ylab = "ICU staytime(days)", xlab = "ICU outcome")
