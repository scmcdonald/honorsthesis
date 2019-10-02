library(gtools)
library(dplyr)
library(stringr) #to use str_trim
library(stringi)
variables <- read.csv("updated-variables.csv")

#https://nces.ed.gov/ipeds/pdf/NPEC/data/NPEC_Paper_IPEDS_History_and_Origins_2018.pdf


# The following will convert to upper case and remove characters that R can't read
Upper_Converter <- function(strings){
  toupper(str_trim(iconv(strings, "ASCII", "UTF-8", sub="")))
}

getIPEDSData <- function(year, survey_file, capital_survey_file, extra = "", 
                         capital_extra = ""){
  dataset <- NULL
  temp <- tempfile()
  download.file(paste("https://nces.ed.gov/ipeds/datacenter/data/", 
                      capital_survey_file, year, capital_extra, ".zip", sep = ""), temp)
  dataset <- read.csv(unz(temp, paste(survey_file, year, extra, ".csv", sep = "")))
  unlink(temp)
  names(dataset) <- toupper(names(dataset))
  # This next line only takes the factor variables and makes them upper case
  dataset <- dataset %>% mutate_if(is.factor, Upper_Converter)
  # This puts the year variable in
  dataset$YEAR <- year
  dataset
}


######################HD####################
survey_file <- "hd"
capital_survey_file <- "HD"
extra <- ""
capital_extra <- ""

# getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)
for (year in 1990:2017){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
      getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

survey_file <- "fa"
capital_survey_file <- "FA"
extra <- "hd"
capital_extra <- "HD"

for (year in 2000:2001){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}


survey_file <- "ic"
capital_survey_file <- "IC"
extra <- "_hd"
capital_extra <- "_HD"

for (year in 99){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}
ic99_hd$YEAR <- "1999"



survey_file <- "ic"
capital_survey_file <- "IC"
extra <- "hdac"
capital_extra <- "HDAC"

for (year in 98){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}
ic98_hd$YEAR <- "1998"

#AHHHHH dates are actually 1998-1999 etc. does this matter....

survey_file <- "ic"
capital_survey_file <- "ic"
extra <- "_hdr"
capital_extra <- "_HDR"

for (year in 9798){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}
ic9798_hdr$YEAR <- "1997"

survey_file <- "ic"
capital_survey_file <- "ic"
extra <- "_a"
capital_extra <- "_A"

for (year in c(9596,9697)){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}
ic9596_a$YEAR <- "1995"
ic9697_a$YEAR <- "1996"





# List of Data frames
HD <- mget(ls(pattern = c("hd|ic")))


levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("HD"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
HD_var <- variables[variables$Table == "HD", 3]

# Merge and and only use the columns in our variable list
HD_df <- do.call("smartbind", HD)
HD_df <- HD_df[ ,colnames(HD_df) %in% HD_var]

# Will need to come up with CBSA and Lat/Long for missing values in earlier years



################EFFY#######################3
survey_file <- "effy"
capital_survey_file <- "EFFY"
extra <- ""
capital_extra <- ""


for (year in 2002:2017){
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

colnames(effy2002)[colnames(effy2002)=="FYRACE21"] <- "EFYHISPT"
colnames(effy2003)[colnames(effy2003)=="FYRACE21"] <- "EFYHISPT"
colnames(effy2004)[colnames(effy2004)=="FYRACE21"] <- "EFYHISPT"
colnames(effy2005)[colnames(effy2005)=="FYRACE21"] <- "EFYHISPT"
colnames(effy2006)[colnames(effy2006)=="FYRACE21"] <- "EFYHISPT"
colnames(effy2007)[colnames(effy2007)=="FYRACE21"] <- "EFYHISPT"


#2008 has two counts, old and new, the number of missing values is the same for both, so I am choosing the new
length(is.na(effy2008$FYRACE21))
length(is.na(effy2008$EFYHISPT))

length(is.na(effy2009$FYRACE21))
length(is.na(effy2009$EFYHISPT))

length(is.na(effy2010$FYRACE21))
length(is.na(effy2010$EFYHISPT))

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "d1"
capital_extra <- "D1"

for (year in 2001){
  try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

#No "EFFYLEV" value

#Grand Total
colnames(ef2001d1)[colnames(ef2001d1)=="FYRACE17"] <- "EFYTOTLT"
 
#Total Men
colnames(ef2001d1)[colnames(ef2001d1)=="FYRACE15"] <- "EFYTOTLM"

#Total Hispanic - I can add total hispanic women and total hispanic men to get grand total hispanic 

#prior to 2001, data is divided by undergraduates and graduates (rather by race/gender)


# List of Data frames
EFFY <- mget(ls(pattern = "effy|^ef\\d{4}d\\d?$"))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("EFFY"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
EFFY_var <- variables[variables$Table == "EFFY", 3]

# Merge and and only use the columns in our variable list
EFFY_df <- do.call("smartbind", EFFY)
EFFY_df <- EFFY_df[ ,colnames(EFFY_df) %in% EFFY_var]

####################################efa###########################################

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "a"
capital_extra <- "A"

for (year in 2000:2017){
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

#2000-2017 (file is too big)
EFA <- mget(ls(pattern = c("^ef\\d{4}a$")))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("EF_A"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
EFA_var <- variables[variables$Table == "EF_A", 3]

# Merge and and only use the columns in our variable list
EFA_df <- do.call("smartbind", EFA)
EFA_df <- EFA_df[ ,colnames(EFA_df) %in% EFA_var]



#Before 2000
survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "_a"
capital_extra <- "_A"

for (year in 1980:1993){try(
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

#this gets 1990
for (year in 90){
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "_anr"
capital_extra <- "_ANR"

for (year in 95:99){
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

for (year in 1994){ 
 assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

ef90_a$year <- "1990"
ef95_anr$year <- "1995"
ef96_anr$year <- "1996"
ef97_anr$year <- "1997"
ef98_anr$year <- "1998"
ef99_anr$year <- "1999"


survey_file <- "ef"
capital_survey_file <- "EF"
extra <- ""
capital_extra <- ""

for (year in 1984:1985){
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

#1980, 1986-1999 (file is too big)
EFA2 <- mget(ls(pattern = c("^ef\\d{2}\\d?\\d?(\\d$|\\d_an?r?|_an?r?)$")))

# Merge and and only use the columns in our variable list
EFA2_df <- do.call("smartbind", EFA2)
EFA2_df <- EFA2_df[ ,colnames(EFA2_df) %in% EFA_var]



################################efCP#####################################################3



survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "cp"
capital_extra <- "CP"

for (year in 2016){
  try(
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}



######################IC#################

survey_file <- "ic"
capital_survey_file <- "IC"
extra <- ""
capital_extra <- ""

#2000-2012. Available from 1980
for (year in 2000:2017){
    assign(paste(survey_file, year, extra, sep = ""), 
           getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

IC <- mget(ls(pattern = "ic\\d{4}"))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("IC"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
IC_var <- variables[variables$Table == "IC", 3]

# Merge and and only use the columns in our variable list
IC_df <- do.call("smartbind", IC)
IC_df <- IC_df[ ,colnames(IC_df) %in% IC_var]
