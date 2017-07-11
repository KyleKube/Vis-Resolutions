library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(xlsx)

# setwd("C:/Users/Chris/Desktop/Kyle")
setwd("C:/Users/christine/Desktop/Kyle")

# QBO stuff, register file was altered in excel before loading(not the same structure as downloaded)
qbo <- read.csv("Register.csv", stringsAsFactors = FALSE)

# m <- as.matrix(qbo[2,])
# m2 <- matrix(m, ncol = ncol(qbo[2,]), dimnames = NULL)
# names(qbo) <- m2
# qbo <- qbo[3:nrow(qbo),]


qbo$i..Date <- mdy(qbo$ï..Date)
qbo$Payment <- as.numeric(qbo$Payment)
qbo$Deposit <- as.numeric(qbo$Deposit)

qbo <- qbo %>%
  mutate(Total = str_replace_all(Total, ",", ""))

qbo$Total <- as.numeric(qbo$Total)



qbo_feb <- qbo %>%
  filter(i..Date <= mdy("02-28-2017")) %>%
  filter(Reconciliation.Status != "Reconciled") %>%
  mutate(File_Type = "qbo")

qbo_feb4 <- select(qbo_feb, 1, 3, 8, 16)
qbo_feb4$ï..Date <- mdy(qbo_feb4$ï..Date)



# Hubdoc stuff
hubdoc <- read.csv("hubdoc_Feb2017.csv", stringsAsFactors = FALSE)

h_names <- c("Date", "Amount", "Star", "Number", "Description")
names(hubdoc) <- h_names
hubdoc[,1] <- mdy(hubdoc[,1])

True_val <- "Withdrawal"
False_val <- "Deposit"
hubdoc <- mutate(hubdoc, Type = if_else(Amount < 0, True_val, False_val))

hubdoc_feb4 <- hubdoc %>%
  mutate(File_Type = "bank") %>%
  select(1,2,5,6,7)


in_qbo_not_hubdoc <- anti_join(qbo_feb4, hubdoc_feb4, by = c("Total" = "Amount"))
in_bank_not_qbo <- anti_join(hubdoc_feb4, qbo_feb4, by = c("Amount" = "Total"))

write.xlsx(in_bank_not_qbo, "To Be Entered.xlsx")

