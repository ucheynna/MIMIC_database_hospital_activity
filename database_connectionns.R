## @knitr num1
#Load required packages
library(knitr)
library(kableExtra)
library(DBI)
library(RPostgres)
library(tidyverse)
library(dbplyr)
library(ruODK)
library(gtsummary)
library(ggplot2)
library(getPass)

# mimic password and username security
mimic <- getPass(msg = 'Please enter your MIMIC database password')
mimicuser <- getPass(msg = 'Please enter your MIMIC database username')

#Create connection to mimic
con = dbConnect(RPostgres::Postgres(),
                dbname= "mimic",
                host = "healthdatascience.lshtm.ac.uk",
                port = 5432,
                user = mimicuser,
                password = mimic)

#Create connection to ODK
options(repos = c(ropensci = 'https://ropensci.r-universe.dev',
                  CRAN = 'https://cloud.r-project.org'))
#odk password and username security
odk <- getPass(msg = 'Please enter your ODK server password')
odkuser <- getPass(msg = 'Please enter your ODK server username')
ruODK::ru_setup( 
  svc = "https://odk-survey.lshtm.ac.uk/v1/projects/80/forms/HDM_Assessment.svc",
  un = odkuser,
  pw = odk)
fq_svc <- ruODK::odata_service_get() 
form_tables <- ruODK::odata_service_get() 
