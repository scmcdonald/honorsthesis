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
for (year in 2002:2017){
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

HD <- lapply(HD, function(x) x[(names(x)) %in% HD_var])
HD_df <- do.call("smartbind", HD)



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

#total Hispanic men
colnames(ef2001d1)[colnames(ef2001d1)=="FYRACE09"] <- "EFYHISPM"
colnames(ef2001d1)[colnames(ef2001d1)=="FYRACE10"] <- "EFYHISPW"

#total hispanic men and women
ef2001d1$EFYHISPT <- ef2001d1$EFYHISPW + ef2001d1$EFYHISPM

#prior to 2001, data is divided by undergraduates and graduates (rather by race/gender)


# List of Data frames
EFFY <- mget(ls(pattern = "effy|^ef\\d{4}d\\d?$"))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("EFFY"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
EFFY_var <- variables[variables$Table == "EFFY", 3]

# Merge and and only use the columns in our variable list

EFFY <- lapply(EFFY, function(x) x[(names(x)) %in% EFFY_var])
EFFY_df <- do.call("smartbind", EFFY)

####################################efa###########################################

survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "a"
capital_extra <- "A"

for (year in 2000:2017){
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra))
}

#fix 2000
#2000 total men
colnames(ef2000a)[colnames(ef2000a)=="EFRACE15"] <- "EFTOTLM"
#2000 total women
colnames(ef2000a)[colnames(ef2000a)=="EFRACE16"] <- "EFTOTLW"
#total men and women
ef2000a$EFTOTLT <- ef2000a$EFTOTLM + ef2000a$EFTOTLW


#2000 total Hispanic men
colnames(ef2000a)[colnames(ef2000a)=="EFRACE09"] <- "EFHISPM"
colnames(ef2000a)[colnames(ef2000a)=="EFRACE10"] <- "EFHISPW"

#total hispanic men and women
ef2000a$EFHISPT <- ef2000a$EFHISPW + ef2000a$EFHISPM


#fix 2001
#2001 total men
colnames(ef2001a)[colnames(ef2001a)=="EFRACE15"] <- "EFTOTLM"
#2001 total women
colnames(ef2001a)[colnames(ef2001a)=="EFRACE16"] <- "EFTOTLW"
#total men and women
ef2001a$EFTOTLT <- ef2001a$EFTOTLM + ef2001a$EFTOTLW

#2001 total Hispanic 
colnames(ef2001a)[colnames(ef2001a)=="EFRACE09"] <- "EFHISPM"
colnames(ef2001a)[colnames(ef2001a)=="EFRACE10"] <- "EFHISPW"

#total hispanic men and women
ef2001a$EFHISPT <- ef2001a$EFHISPW + ef2001a$EFHISPM

#fix 2002
#grand total
colnames(ef2002a)[colnames(ef2002a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2002a)[colnames(ef2002a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2002a)[colnames(ef2002a)=="EFRACE21"] <- "EFHISPT"

#fix 2003
#grand total
colnames(ef2003a)[colnames(ef2003a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2003a)[colnames(ef2003a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2003a)[colnames(ef2003a)=="EFRACE21"] <- "EFHISPT"

#fix 2004
#grand total
colnames(ef2004a)[colnames(ef2004a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2004a)[colnames(ef2004a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2004a)[colnames(ef2004a)=="EFRACE21"] <- "EFHISPT"

#fix 2005
#grand total
colnames(ef2005a)[colnames(ef2005a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2005a)[colnames(ef2005a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2005a)[colnames(ef2005a)=="EFRACE21"] <- "EFHISPT"

#fix 2006
#grand total
colnames(ef2006a)[colnames(ef2006a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2006a)[colnames(ef2006a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2006a)[colnames(ef2006a)=="EFRACE21"] <- "EFHISPT"

#fix 2007
#grand total
colnames(ef2007a)[colnames(ef2007a)=="EFRACE24"] <- "EFTOTLT"
#total men
colnames(ef2007a)[colnames(ef2007a)=="EFRACE15"] <- "EFTOTLM"
# total hispanic men and women
colnames(ef2007a)[colnames(ef2007a)=="EFRACE21"] <- "EFHISPT"


#2000-2017 (file is too big)
EFA <- mget(ls(pattern = c("^ef\\d{4}a$")))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("EF_A"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
EFA_var <- variables[variables$Table == "EF_A", 3]

# Merge and and only use the columns in our variable list

EFA <- lapply(EFA, function(x) x[(names(x)) %in% EFA_var])
EFA_df <- do.call("smartbind", EFA)



######Before 2000
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



################################efCP#####################################################



survey_file <- "ef"
capital_survey_file <- "EF"
extra <- "cp"
capital_extra <- "CP"

for (year in 2000:2016){
  try(
  assign(paste(survey_file, year, extra, sep = ""), 
         getIPEDSData(year, survey_file, capital_survey_file, extra, capital_extra)))
}

#2006 
#Grand total
colnames(ef2006cp)[colnames(ef2006cp)=="EFRACE24"] <- "EFTOTLT"

# total men
colnames(ef2006cp)[colnames(ef2006cp)=="EFRACE15"] <- "EFTOTLM"

# total hispanic
colnames(ef2006cp)[colnames(ef2006cp)=="EFRACE21"] <- "EFHISPT"

#2004 
#Grand total

colnames(ef2004cp)[colnames(ef2004cp) == "EFRACE24"] <- "EFTOTLT"

#total men

colnames(ef2004cp)[colnames(ef2004cp) == "EFRACE15"] <- "EFTOTLM"

#total hispanic

colnames(ef2004cp)[colnames(ef2004cp)=="EFRACE21"] <- "EFHISPT"

#2002
colnames(ef2002cp)[colnames(ef2002cp) == "EFRACE24"] <- "EFTOTLT"

#total men

colnames(ef2002cp)[colnames(ef2002cp) == "EFRACE15"] <- "EFTOTLM"

#total hispanic

colnames(ef2002cp)[colnames(ef2002cp)=="EFRACE21"] <- "EFHISPT"

#2000

#2000 Total Men
colnames(ef2000cp)[colnames(ef2000cp)=="EFRACE15"] <- "EFTOTLM"
#2000 Total Women
colnames(ef2000cp)[colnames(ef2000cp)=="EFRACE16"] <- "EFTOTLW"
#total men and women
ef2000cp$EFTOTLT <- ef2000cp$EFTOTLM + ef2000cp$EFTOTLW

#2001 total Hispanic 
colnames(ef2000cp)[colnames(ef2000cp)=="EFRACE09"] <- "EFHISPM"
colnames(ef2000cp)[colnames(ef2000cp)=="EFRACE10"] <- "EFHISPW"

#total hispanic men and women
ef2000cp$EFHISPT <- ef2000cp$EFHISPW + ef2000cp$EFHISPM



EFC <- mget(ls(pattern = "^ef\\d{4}cp$"))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("EF_CP"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
EFC_var <- variables[variables$Table == "EF_CP", 3]

# Merge and and only use the columns in our variable list
EFC <- lapply(EFC, function(x) x[(names(x)) %in% EFC_var])
EFC_df <- do.call("smartbind", EFC)




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

IC <- mget(ls(pattern = "^ic\\d{4}$"))

levels(variables$Variable.Name) <- c(levels(variables$Variable.Name),"YEAR") 
variables[nrow(variables) + 1,] = list(Table= factor("IC"),Variable.Number= 9999, Variable.Name= factor("YEAR"))
IC_var <- variables[variables$Table == "IC", 3]

# Merge and and only use the columns in our variable list
IC <- lapply(IC, function(x) x[(names(x)) %in% IC_var])
IC_df <- do.call("smartbind", IC)

