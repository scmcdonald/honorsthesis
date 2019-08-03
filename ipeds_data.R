#Obtaining data from IPEDS survey, following Urban Institute Github 
#https://github.com/UrbanInstitute/education-data-package-r
#https://educationdata.urban.org/documentation/
library(devtools)
library(data.table)
library(dplyr)
library(educationdata)

n<- c(1990:2019)

#available 1984-2017, this takes 1990-2017
inst_char <- list()
for(i in 1:length(n)){
  try(
    inst_char[[i]] <- get_education_data(
      level = 'college-university', 
      source = 'ipeds', 
      topic = 'institutional-characteristics',
      filters = list(year = n[i]),
      add_labels = TRUE)
  )
}
inst_char <- rbindlist(inst_char)

#available 1984-2017, this takes 1990-2017
direct <- list()
for(i in 1:length(n)){
  try(
    direct[[i]] <- get_education_data(
      level = 'college-university', 
      source = 'ipeds', 
      topic = 'directory', 
      filters = list(year = n[i]),
      add_labels = TRUE)
  )
}
direct <- rbindlist(direct)

#available 1996-2016
test <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'enrollment-headcount', 
  filters = list(year = 2001),
  add_labels = TRUE)

#Only available 2001-2017
#this is new admittances/enrollments
adm_enroll <- list()
for(i in 1:length(n)){
  try(
    adm_enroll[[i]] <- get_education_data(
      level = 'college-university', 
      source = 'ipeds', 
      topic = 'admissions-enrollment',
      filters = list(year = n[i]),
      add_labels = TRUE)
  )
}
adm_enroll <- rbindlist(adm_enroll)


#grad rates avilabile for 1996–2016
#enrollment can also be pulled by race/sex, for 1986-2017
#