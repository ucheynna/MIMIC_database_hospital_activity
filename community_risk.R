## @knitr num6
#API to extract covid-19 hospital cases data from gov.uk website
covid_cases_gov<- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=hospitalCases&format=csv&release=2023-12-14')
#filter cases that happened in March 2021
March_covid <- covid_cases_gov %>% filter(str_detect(date, '2021-03'))
March_covid$date <- as.Date(March_covid$date)

# Create a line plot showing hospital cases over the month of March
plot(March_covid$date, March_covid$hospitalCases, 
     type = "l",  # "l" for line plot
     xlab = "Date Interval", ylab = "Hospital Cases", lwd=2, col='red',
     main = "UK covid-19 Hospital Cases in March 2021")
grid()
