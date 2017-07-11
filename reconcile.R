library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(xlsx)

# setwd("C:/Users/Chris/Desktop/Kyle")
setwd("C:/Users/christine/Desktop/Kyle")

# qbo_file <- "register.csv"
 hubdoc_file <- "hubdoc_Feb2017.csv"

# QBO stuff, register file was altered in excel before loading(not the same structure as downloaded)
# qbo <- read.csv(qbo_file, stringsAsFactors = FALSE)

# m <- as.matrix(qbo[2,])
# m2 <- matrix(m, ncol = ncol(qbo[2,]), dimnames = NULL)
# names(qbo) <- m2
# qbo <- qbo[3:nrow(qbo),]
Register2 <- read_excel("C:/Users/christine/Desktop/KYLE/Register2.xls",
                        col_types = c("text", "text", "text",
                                      "blank", "numeric", "numeric", "numeric",
                                      "text", "numeric", "text", "text",
                                      "numeric", "numeric"), skip = 1)

Register2$Date <- mdy(Register2$Date)
qbo <- Register2

qbo <- qbo %>%
  mutate(Payment = coalesce(Payment, 0), Deposit = coalesce(Deposit, 0))

qbo$Total <- qbo$Deposit - qbo$Payment

qbo_feb <- qbo %>%
  filter(Date <= mdy("02-28-2017")) %>%
  filter(`Reconciliation Status` != "Reconciled") %>%
  mutate(File_Type = "qbo")

# qbo_feb4 <- select(qbo_feb, 1, 3, 8, 16)
# qbo_feb4$Date <- mdy(qbo_feb4$Date)



# Hubdoc stuff
hubdoc <- read.csv(hubdoc_file, stringsAsFactors = FALSE)

h_names <- c("Date", "Amount", "Star", "Number", "Description")
names(hubdoc) <- h_names
hubdoc[,1] <- mdy(hubdoc[,1])

True_val <- "Withdrawal"
False_val <- "Deposit"
hubdoc <- mutate(hubdoc, Type = if_else(Amount < 0, True_val, False_val))

# hubdoc_feb4 <- hubdoc %>%
#   mutate(File_Type = "bank") %>%
#   select(1,2,5,6,7)

# by <- c("Amount" = "Total")
in_qbo_not_hubdoc <- anti_join(qbo_feb, hubdoc, by = c("Total" = "Amount"))
in_bank_not_qbo <- anti_join(hubdoc, qbo_feb, by = c("Amount" = "Total"))

iqnh_withdrawals <- in_qbo_not_hubdoc %>%
  filter(Total <= 0)
iqnh_deposits <- in_qbo_not_hubdoc %>%
  filter(Total >= 0)

ibnq_withdrawals <- in_bank_not_qbo %>%
  filter(Amount <= 0)
ibnq_deposits <- in_bank_not_qbo %>%
  filter(Amount >= 0)

hd <- inner_join(hubdoc_feb4, qbo_feb4, by = by)
hd2 <- filter(hd, hd$Type == "Withdrawal")
total <- sum(hd2$Amount)


# anti_join == all rows in qbo_feb4 that do not match in_qbo_not_hubdoc
test <- anti_join(qbo_feb4, in_qbo_not_hubdoc, by = "Total")
withdrawals <- filter(test, Total <= 0)
(withdrawals_total <- sum(withdrawals$Total))

# semi_join == all rows in qbo_feb4 that match in_qbo_not_hubdoc
test2 <- semi_join(qbo_feb4, in_qbo_not_hubdoc, by = "Total")
withdrawals2 <- filter(test2, Total <= 0)
(withdrawals_total2 <- sum(withdrawals2$Total))




write.xlsx(in_bank_not_qbo, "To Be Entered.xlsx")




