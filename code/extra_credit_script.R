table <- read.csv("tbl1.csv")

table2 <- read.csv("Bishop_Accountability - Bishop_Accountability.csv")



tbl <- merge(table, table2[, c("Last", "First", "Ord", "Year_of_First_Publicity")], by = c("Last", "First", "Ord"), all.x = TRUE)

write.csv(tbl, "extra_credit.csv")

