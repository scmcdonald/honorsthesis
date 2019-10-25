library(rvest)
library(dplyr)
library(stringr)

url <-"http://www.bishop-accountability.org/AtAGlance/USCCB_Yearly_Data_on_Accused_Priests.htm"

webpage <- read_html(url)

tbls <- html_nodes(webpage, "table")


tbl <- webpage %>%
  html_nodes("table") %>%
  .[6] %>%
  html_table(fill = TRUE)

tbl <- as.data.frame((tbl))

names(tbl) = tbl[1, ] # the first row will be the header
tbl = tbl[-c(1, 17:24), -c(5:7)] 

colnames(tbl) <- gsub("[\r\n]", "", colnames(tbl))
colnames(tbl) <- str_squish(colnames(tbl))

tbl[] <- lapply(tbl, function(x) gsub("[\r\n]", "", x))
tbl[] <- lapply(tbl, function(x) str_squish(x))

tbl <- na_if(tbl, "Not Available Not Available")

tbl[,2] <- sapply(strsplit(tbl[,2], split= " "), "[", 1)
tbl[,3] <- sapply(strsplit(tbl[,3], split= " "), "[", 1)
tbl[,4] <- sapply(strsplit(tbl[,4], split= " "), "[", 1)

tbl[15, 2] <-sapply(strsplit(tbl[15, 2], split= "[a-z]+"), "[", 1)
tbl[15, 3] <-sapply(strsplit(tbl[15, 3], split= "[a-z]+"), "[", 1)
tbl[15, 4] <-sapply(strsplit(tbl[15, 4], split= "[a-z]+"), "[", 1)

tbl[14, 2] <-sapply(strsplit(tbl[14, 2], split= "[a-z]+"), "[", 1)
tbl[14, 3] <-sapply(strsplit(tbl[14, 3], split= "[a-z]+"), "[", 1)
tbl[14, 4] <-sapply(strsplit(tbl[14, 4], split= "[a-z]+"), "[", 1)

