library(gtools)
library(dplyr)

getIPEDSData <- function(year, survey_file, capital_survey_file, extra = "", 
  capital_extra = ""){
  temp <- tempfile()
  download.file(paste("https://nces.ed.gov/ipeds/datacenter/data/", 
    capital_survey_file, year, capital_extra, ".zip", sep = ""), temp)
  data <- read.csv(unz(temp, paste(survey_file, year, extra, ".csv", sep = "")))
  unlink(temp)
  data
}

survey_file <- "hd"
capital_survey_file <- "HD"
extra <- ""
capital_extra <- ""

for (year in 1980:2017){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
      getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

# Editing column names that are lowercase to uppercase
names(hd2002)<- toupper(names(hd2002))
names(hd2003)<- toupper(names(hd2003))
names(hd2004)<- toupper(names(hd2004))
names(hd2005)<- toupper(names(hd2005))
names(hd2009) <- toupper(names(hd2009))

# Adding "year" column. I am sure there is a way to do this programmatically, not sure how though.
hd2002$YEAR = "2002"
hd2003$YEAR = "2003"
hd2004$YEAR = "2004"
hd2005$YEAR = "2005"
hd2006$YEAR = "2006"
hd2007$YEAR = "2007"
hd2008$YEAR = "2008"
hd2009$YEAR = "2009"
hd2010$YEAR = "2010"
hd2011$YEAR = "2011"
hd2012$YEAR = "2012"
hd2013$YEAR = "2013"
hd2014$YEAR = "2014"
hd2015$YEAR = "2015"
hd2016$YEAR = "2016"
hd2017$YEAR = "2017"

# List of Data frames. I need help doing this programammatically...
hd <- list(hd2002, hd2003, hd2004, hd2005, hd2006, hd2007, hd2008, hd2009, hd2010, hd2011, hd2012, hd2013, hd2014, hd2015, hd2016, hd2017)

# I got my variables from my google sheets that has all of the variables I will be choosing
hd_var <- list("UNITID", "INSTNM", "ADDR", "CITY", "STABBR", "ZIP", "FIPS", "OBEREG", "SECTOR", "ICLEVEL", "CONTROL", "HLOFFER", "DEGGRANT", "LOCALE", "OPENPUBL", "NEWID",  "CLOSEDAT", "CYACTIVE", "CARNEGIE", "CBSA", "CBSATYPE", "LONGITUD", "LATITUDE", "YEAR")


hd_df <- do.call("smartbind", hd)

# I want to select hd_var here, but I get an error, so I had to copy all the names
hd_df <- hd_df %>% select("UNITID", "INSTNM", "ADDR", "CITY", "STABBR", "ZIP", "FIPS", "OBEREG", "SECTOR", "ICLEVEL", "CONTROL", "HLOFFER", "DEGGRANT", "LOCALE", "OPENPUBL", "NEWID",  "CLOSEDAT", "CYACTIVE", "CARNEGIE", "CBSA", "CBSATYPE", "LONGITUD", "LATITUDE", "YEAR")





survey_file <- "effy"
capital_survey_file <- "EFFY"
extra <- ""
capital_extra <- ""


for (year in 1980:2017){
  try(
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "a"
capital_extra <- "A"

for (year in 1980:2017){
  try(
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "_a"
capital_extra <- "_A"

for (year in 1980:2017){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "_a"
capital_extra <- "_A"

#this gets 1990
for (year in 90){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "cp"
capital_extra <- "CP"

for (year in 1980:2017){
  try(
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "ic"
capital_survey_file <- "IC"
extra <- ""
capital_extra <- ""

for (year in 1980:2017){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

