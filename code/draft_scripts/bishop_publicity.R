#Bishop Accountability Scrape
library(rvest)
library(dplyr)
library(stringr)

url <- paste("http://origin.bishop-accountability.org/priestdb/PriestDBbylastName-", "A", ".html", sep="")
webpage <- read_html(url)

tbl <- webpage %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table(fill = TRUE)

tbl <- as.data.frame((tbl))
names(tbl) = tbl[1, ] # the first row will be the header
tbl <- tbl[-1,]

for (i in 2:length(LETTERS)){
  i <- LETTERS[i]
  url <- paste("http://origin.bishop-accountability.org/priestdb/PriestDBbylastName-", i, ".html", sep="")
  webpage <- read_html(url)
  
  tbls <- html_nodes(webpage, "table")
  
  tbl_i <- webpage %>%
    html_nodes("table") %>%
    .[4] %>%
    html_table(fill = TRUE)
  
  tbl_i <- as.data.frame((tbl_i))
  
  names(tbl_i) = tbl_i[1, ] # the first row will be the header
  tbl <- rbind(tbl, tbl_i[-1,])
}

tbl <-tbl[!apply(tbl == "", 1, all),]


#hand classify these
tbl1 <- tbl %>% filter(str_detect(tbl$Notes, "died|retired"))

tbl2 <- tbl %>% filter(str_detect(tbl$Notes, "died|retired", negate = TRUE))

# Select source only
tbl2$Source = str_extract_all(tbl2$`Source/Assignments` , "(Source:.+)(?=Assignments)|(Source:.+$)")
# Find dates only
tbl2$Source <- str_extract_all(tbl2$Source, "\\d{1,}\\.\\d{1,}\\.\\d{1,}")

tbl2$public <- ""

for (i in 1: length(tbl2$Source)){
  y <- tbl2$Source[[i]]
  y <- str_extract_all(y, "\\d{1,2}$")
  y <- as.integer(y)
  y <- ifelse(y>22, y+1900, xy+2000)
  y<- min(y)
  
  tbl2$public[i] <- y
}


tbl2$public <- as.numeric(tbl2$public)

library(ggplot2)
ggplot(data=tbl2, aes(x=public)) +
  geom_bar(stat="count")


library(tidyr)
tbl2 <- separate(tbl2, col = "Diocese", into = c("Diocese", "State"), sep = ",")

tbl2$ID <- as.character(seq.int(nrow(tbl2)))

x <-tbl2 %>% select(Diocese, public, ID) %>% group_by(Diocese, public) %>% summarise(count = length(ID))


dioceses <- read.csv("dioceses.csv")
dioceses1 <- as.character(dioceses$dioceses)
dioceses1 <- str_trim(dioceses1, side = "r")

dioceses2 <- str_trim(unique(tbl2$Diocese))

s <- read.csv("dioceses_from_geo.csv")
dioceses<- s$x
subset(dioceses2, !(dioceses2 %in% dioceses))
subset(dioceses, !(dioceses %in% dioceses2))




