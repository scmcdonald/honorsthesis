#Hispanic Population counts
#https://seer.cancer.gov/popdata/singleages.html
#https://seer.cancer.gov/popdata/popdic.html

library(dplyr)
library(tidyr)
library(sf)
his <- read.table("us.1990_2017.19ages.adjusted.txt")

#extracting needed variables
his$Year <- substr(his$V1, 1,4)
his$State<- substr(his$V1, 5,6)
his$FIPS<- substr(his$V1, 7,11)
his$Origin <- substr(his$V1, 15,15)
his$Population <-substr(his$V1, 19, 26)

#change population to numeric
his$Population <- as.numeric(his$Population)

#labeling for readability
his$Origin[his$Origin=="1"] <- "Hispanic"
his$Origin[his$Origin=="0"] <- "Non-Hispanic"

#counts of population for year, state, fips, origin, and hispanic/nonhispanic
his <- his %>% group_by(Year, FIPS, Origin) %>%
  summarize(Population= sum(Population))

#hispanic and nonhispanic columns
his <- his %>% spread(key = Origin, value = Population)

#total population
his$total_pop = his$Hispanic + his$`Non-Hispanic`

#percent hispanic
his$his_prop = (his$Hispanic/his$total_pop)

his <- his[, c(1,2,6)]

########################################################################
#annual personal income by county 1969-2017
#https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas
income <- read.csv("CAINC1__ALL_STATES_1969_2017.csv")

income<- income[, -c(3:6, 8)]


#remove notes at last three observations
levels(income$Description)
income %>% filter(Description == "")
income[9595:9597,]

income <- income[-c(9595:9597),]


colnames(income) <- sub("X", "", colnames(income))

income <- income %>% 
  gather(`1969`, `1970`, `1971`, `1972`, `1973`, `1974`, `1975`, `1976`, `1977`, `1978`, `1979`, `1980`, `1981`, `1982`, `1983`, `1984`, `1985`, `1986`, `1987`, `1988`, `1989`, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, key = "year", value = "amount")

income$Description <- as.character(as.factor(income$Description))

income$Description[income$Description=="Personal income (thousands of dollars)"] <- "personal_income"
income$Description[income$Description=="Population (persons) 1/"] <- "population"
income$Description[income$Description=="Per capita personal income (dollars) 2/"] <- "income_per_capita"

income <- income %>% spread(key = Description, value = amount)
income$year <- as.numeric(income$year)

income <- income[!(income$year < 1990),]
income$year <- as.character(income$year)
income <- income[!grepl("\\d\\d000", income$GeoFIPS),]
income <- income[, c(1,3,4)]
income$GeoFIPS <- as.character(as.factor(income$GeoFIPS))

income$GeoFIPS <- trimws(income$GeoFIPS, "left")

##################################################################################################

pop_dens <- st_read("tl_2017_us_county/tl_2017_us_county.shp")
pop_dens <- pop_dens %>% select(STATEFP, COUNTYFP, GEOID, ALAND)
pop_dens<-st_set_geometry(pop_dens, NULL)

pop_dens <- merge(x = pop_dens, y = his[, c("FIPS", "total_pop", "Year")], by.x = "GEOID", by.y = "FIPS", all.x = TRUE)

pop_dens$pop_dens <- pop_dens$total_pop/pop_dens$ALAND

pop_dens <- pop_dens[,c(1,6,7)]
pop_dens$GEOID <- as.character(as.factor(pop_dens$GEOID))

socio <- merge(his, income, by.x = c("FIPS", "Year"), by.y = c("GeoFIPS", "year"), all.x = TRUE, all.y = TRUE)

socio <- merge(socio, pop_dens, by.x = c("FIPS", "Year"), by.y = c("GEOID", "Year"), all.x = TRUE, all.y = TRUE)


write.csv(socio, "socioeconomic.csv")




#########################################3
#religion 1980,1990,2000,2010
#http://www.thearda.com/Archive/Files/Descriptions/RCMSMGCY.asp
library(readxl)

religion <- read_excel("ARDA.XLSX")

religion$FIPSMERG <- ifelse(nchar(religion$FIPSMERG) != 5 ,gsub(" ", "", paste("0",religion$FIPSMERG), fixed = TRUE), religion$FIPSMERG)

religion <- religion[, c(1, 4, 5, 7, 8)]

religion <- religion %>% filter(GRPNAME == "Catholic Church")

religion$cath_prop <- religion$ADHERENT/religion$TOTPOP
religion <- religion[!(religion$YEAR < 1990), c(1,2,6)]
religion$YEAR <- as.character(religion$YEAR)



#Catholic Hierarchy Scrape
library(rvest)
library(dplyr)
library(stringr)

dioceses<- read.csv("dioceses.csv")

url <- paste("http://www.catholic-hierarchy.org/diocese/", "dlitt", ".html", sep="")
webpage <- read_html(url)
xpath='//*[@id="d5"]/table'

tbl <- webpage %>%
  html_nodes(xpath=xpath) %>%
  html_table() 
tbl <- as.data.frame(tbl)

tbl <- tbl[, c(1, 4)]


for (i in 1:length(dioceses$abbr)){
  i <- dioceses$abbr[i]
  url <- paste("http://www.catholic-hierarchy.org/diocese/", i, ".html", sep="")
  webpage <- read_html(url)
  tbls <- html_nodes(webpage, xpath=xpath)
  
try(  
  tbl_i <- webpage %>%
    html_nodes(xpath=xpath) %>%
    html_table() 
   )
  tbl_i <- as.data.frame((tbl_i))
  tbl_i <- tbl_i[, c(1, 4)]
  tbl <- rbind(tbl, tbl_i[-1, -ncol(tbl_i)])
}



dioceses <- read.csv("dioceses.txt", header = FALSE)
dioceses[] <- lapply(dioceses, function(x) gsub("Â", "", x))

dioceses <- dioceses %>% filter(str_detect(V1, "Diocese") == TRUE)
dioceses$V1 <- trimws(dioceses$V1, "right")
dioceses <- dioceses[-c(36, 133, 84), ]

dioceses <- gsub("Diocese of ", "", dioceses)
dioceses<-as.data.frame(dioceses)
dioceses$abbr <- substr(dioceses$dioceses,1, 4)
dioceses$abbr <- tolower(dioceses$abbr)
dioceses$abbr <- paste0("d", dioceses$abbr)
write.csv("dioceses.csv")
