library(rvest)
url <- paste("http://www.catholic-hierarchy.org/diocese/", "d", "albn", ".html", sep="")
webpage <- read_html(url)

tbl <- webpage %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

tbl <- as.data.frame((tbl))

tbl$Percent.Catholic <- gsub("%", "", tbl$Percent.Catholic)
tbl$Percent.Catholic <- as.numeric(tbl$Percent.Catholic)
tbl$Dioceses <- "albn"

dioceses  <- read.csv("C:/Users/Sarah McDonald/Documents/honorsthesis/manipulated_data/dioceses_from_geo.csv")
dioceses$abbr <- tolower(substr(dioceses$x, 1, 4))





dioceses[dioceses$x == "Los Angeles", "abbr"] <- "losa" 
dioceses[dioceses$x == "Des Moines", "abbr"] <- "desm" 
dioceses[dioceses$x == "El Paso", "abbr"] <- "elpa" 
dioceses[dioceses$x == "Kansas City KS", "abbr"] <- "kcks" 
dioceses[dioceses$x == "Kansas City-St. Joseph", "abbr"] <- "kcmo" 
dioceses[dioceses$x == "La Crosse", "abbr"] <- "lacr" 
dioceses[dioceses$x == "Las Cruces", "abbr"] <- "lasc" 
dioceses[dioceses$x == "Las Vegas", "abbr"] <- "lasv" 
dioceses[dioceses$x == "New Orleans", "abbr"] <- "newo" 
dioceses[dioceses$x == "New York", "abbr"] <- "newy" 
dioceses[dioceses$x == "New Ulm", "abbr"] <- "newu" 
dioceses[dioceses$x == "San Angelo", "abbr"] <- "sang" 
dioceses[dioceses$x == "San Antonio", "abbr"] <- "snan" 
dioceses[dioceses$x == "San Bernardino", "abbr"] <- "snbe" 
dioceses[dioceses$x == "San Diego", "abbr"] <- "sndi" 
dioceses[dioceses$x == "San Francisco", "abbr"] <- "snfr" 
dioceses[dioceses$x == "San Jose", "abbr"] <- "snjo" 

dioceses[dioceses$x == "Santa Fe", "abbr"] <- "snfe" 
dioceses[dioceses$x == "Santa Rosa", "abbr"] <- "snro" 
dioceses[dioceses$x == "Springfield-Cape Girardeau", "abbr"] <- "spmo" 
dioceses[dioceses$x == "Springfield in Illinois", "abbr"] <- "spil" 
dioceses[dioceses$x == "Springfield in Massachusetts", "abbr"] <- "spma" 


dioceses[dioceses$x == "St. Augustine", "abbr"] <- "stau" 
dioceses[dioceses$x == "St. Petersburg", "abbr"] <- "stpe" 
dioceses[dioceses$x == "St. Cloud", "abbr"] <- "stcl" 
dioceses[dioceses$x == "St. Paul &amp; Minneapolis", "abbr"] <- "stpa" 
dioceses[dioceses$x == "St. Louis", "abbr"] <- "stlo" 

list <- list()
for(i in 1:length(dioceses$abbr)){
  i <- dioceses$abbr[i]
  url <-  paste("http://www.catholic-hierarchy.org/diocese/", "d", i, ".html", sep="")
  webpage <- try(read_html(url))
  try(
   list[i] <-webpage %>%
      html_nodes("table") %>%
      .[3] %>%
      html_table(fill = TRUE)
    )
  

  
  
}

dioceses %>% select(abbr) %>% filter(!(abbr %in% names(list)) == TRUE)


issues <- c()
for(i in 1:length(list)) {
  ifelse(
  
    !(c("Year", "Percent Catholic")   %in% names(list[i][[1]])),
  
  issues <- append(issues, names(list[i])), print("good")
  
  )
  
  
}


list <- within(list, rm(anch, wino, kcks, bato))
#anchorage

url <-  paste("http://www.catholic-hierarchy.org/diocese/", "d", "anch", ".html", sep="")
webpage <- read_html(url)

  anch <-webpage %>%
    html_nodes("table") %>%
    .[4] %>%
    html_table(fill = TRUE)

#kansas city KS
url <-  paste("http://www.catholic-hierarchy.org/diocese/", "d", "kcks", ".html", sep="")
webpage <- read_html(url)

kcks <-webpage %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table(fill = TRUE)

#Baton Rouge
url <-  paste("http://www.catholic-hierarchy.org/diocese/", "d", "bato", ".html", sep="")
webpage <- read_html(url)

bato <-webpage %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table(fill = TRUE)


#WINONA does not have data due to recent merge to WINONA ROCHESTER



for(i in 1:length(list)) {
  list[i][[1]]$Dioceses <- names(list[i])
  list[i][[1]] <- list[i][[1]] %>% select("Year", "Percent Catholic", "Dioceses")
  
}

list <- do.call("smartbind", list)

anch[[1]]$Dioceses <- "anch"
bato[[1]]$Dioceses <- "bato"
kcks[[1]]$Dioceses <- "kcks"

anch[[1]] <- anch[[1]] %>% select("Year", "Percent Catholic", "Dioceses")
bato[[1]] <- bato[[1]] %>% select("Year", "Percent Catholic", "Dioceses")
kcks[[1]] <- kcks[[1]] %>% select("Year", "Percent Catholic", "Dioceses")



anch <- as.data.frame(anch)
bato <- as.data.frame(bato)
kcks <- as.data.frame(kcks)

colnames(anch)[colnames(anch)== "Percent.Catholic"] <- "Percent Catholic"
colnames(bato)[colnames(bato)== "Percent.Catholic"] <- "Percent Catholic"
colnames(kcks)[colnames(kcks)== "Percent.Catholic"] <- "Percent Catholic"
list <- smartbind(list, anch, bato, kcks)



#removes blank values and where the name was printed in cell instead of year
list <- list %>% filter(nchar(Year)==4)
list$`Percent Catholic` <- gsub("%", "", list$`Percent Catholic`)
list$`Percent Catholic` <- as.numeric(list$`Percent Catholic`)

list <- list %>% filter(is.na(`Percent Catholic`)==FALSE)


#zeros are missing values
list <-list %>% filter(`Percent Catholic` != 0)

list$Year <- as.numeric(list$Year)

i <- unique(list$Dioceses)[1]
tbl_i <- list %>% filter( list$Dioceses== i)

app_i <- approx(tbl_i$Year, tbl_i$`Percent Catholic`, xout= 1990:2017,method = "linear")


y <- c(30, 40, 60)
x <- c(1950, 1978, 1990)

approx(x, y, xout = c(1950:1991), method = "linear")

catholic <- data.frame("Year"=numeric(0), "Percent Catholic"=numeric(0), "Dioceses"=numeric(0))

for( i in 1:length(unique(list$Dioceses))){
  i <- unique(list$Dioceses)[i]
  tbl_i <- list %>% filter( list$Dioceses== i)
  app_i <- approx(tbl_i$Year, tbl_i$`Percent Catholic`, xout= 1990:2017,method = "linear", rule = 2)
  app_i <- as.data.frame(app_i)
  app_i$Dioceses <- i
  colnames(app_i)[colnames(app_i)=="x"] <- "Year"
  colnames(app_i)[colnames(app_i)=="y"] <- "Percent Catholic"
  catholic <- rbind(catholic, app_i)
}

#rule ==an integer describing how interpolation is to take place outside the interval [min(x), max(x)]. If rule is 1 then NAs are returned for such points and if it is 2, the value at the closest data extreme is used.

catholic <- merge(catholic, dioceses[, c("x", "abbr")], by.x="Dioceses", by.y = "abbr")
catholic <- catholic %>% select("Year", "Percent Catholic", "x")
colnames(catholic)[colnames(catholic)=="x"] <- "Dioceses"
