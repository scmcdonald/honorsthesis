## ---- echo = FALSE, warning= FALSE, message = FALSE----------------------
data <- read.csv("C:/Users/Sarah McDonald/Documents/honorsthesis/manipulated_data/data_bishop_account.csv")
bishop <- read.csv("C:/Users/Sarah McDonald/Documents/honorsthesis/manipulated_data/bishop_pub.csv")

library(ggplot2)
library(dplyr)
library(tidyr)


## ---- echo = FALSE-------------------------------------------------------
ggplot(data, aes(y = bishop_pub)) +
  geom_boxplot() +
  theme(
    axis.ticks.x=element_blank(),
    axis.text.x = element_blank()
  )
print(summary(data$bishop_pub))


## ---- echo = FALSE-------------------------------------------------------
bishop_sum <- bishop %>% select(public, bishop_pub) %>% filter(public >1989 & public <2018) %>%  group_by(public) %>% summarise_each(funs = sum)

ggplot(bishop_sum, aes(x = public, y = bishop_pub))+
  geom_bar(stat = "identity")



## ----code = readLines(knitr::purl("EDA.Rmd", documentation = 1)), echo = T, eval = F----
## NA

