#Obtaining data from IPEDS survey, following Urban Institute Github 
#https://github.com/UrbanInstitute/education-data-package-r
#https://educationdata.urban.org/documentation/
library(devtools)
library(data.table)
library(dplyr)

,

devtools::install_github('UrbanInstitute/education-data-package-r')


library(educationdata)
df <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'institutional-characteristics', 
  filters = list(year = 2016),
  add_labels = TRUE)

colnames(df)
"religious_affiliation" %in% colnames(df)
"inst_affiliation" %in% colnames(df)


df <- df %>% select(
  "id", "year", "fips", "unitid", "religious_affiliation", "inst_affiliation"
)

df2 <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'directory', 
  filters = list(year = 2016),
  add_labels = TRUE)

df2 <- df2 %>% select(
  "year", "fips", "unitid", "inst_name", "address", "state_abbr", "zip", "county_name", "offering_highest_level", "latitude", "longitude"
)

aff <- merge(df,df2, by=c("year", "fips", "unitid"))

aff %>% group_by(religious_affiliation) %>% summarise(count= n())


cath <- aff %>% filter(religious_affiliation == "Roman Catholic")


df3 <- get_education_data(
  level = 'college-university', 
  source = 'ipeds', 
  topic = 'admissions-enrollment', 
  filters = list(year = 2016),
  add_labels = TRUE)

aff <- merge(aff, df3, by=c("unitid", "year", "fips"))


cath <- aff %>% filter(religious_affiliation == "Roman Catholic")
preliminary <- cath %>% filter(ftpt=="Full-time", sex == "Total")

st <- cath %>% group_by(fips) %>% summarise(count=(n_distinct(inst_name)))

