#Obtaining data from IPEDS survey, following Urban Institute Github 
#https://github.com/UrbanInstitute/education-data-package-r
#https://educationdata.urban.org/documentation/
library(devtools)
library(data.table)
library(dplyr)
library(educationdata)
library(data.table)
library()

inst_char <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'institutional-characteristics', 
  filters = list(year = 2016),
  add_labels = TRUE)

directory <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'directory', 
  filters = list(year = 2016),
  add_labels = TRUE)

adm_enroll <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'admissions-enrollment', 
  filters = list(year = 2016),
  add_labels = TRUE)

n <- 1995:2019




adm_enroll=data.frame()
for (i in n) {
  try (
    test <- get_education_data(
      level = 'college-university', 
      source = 'ipeds', 
      topic = 'admissions-enrollment',
      by= list('year'),
      add_labels = TRUE),
    adm_enroll <- rbind(adm_enroll, test)
  )
}



