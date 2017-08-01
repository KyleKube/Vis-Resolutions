library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(readxl)

#################################################################################################################
## set the client and month folder in the file path
qbo_file <- "C:/Users/Chris/Desktop/Kyle/HSquared/2017/May/Register.xls"

## set the client and month folder in the file path
hubdoc_file <- "C:/Users/Chris/Desktop/Kyle/HSquared/2017/May/Hubdoc.csv"
# hubdoc_file2 <- "C:/Users/Chris/Desktop/Kyle/HSquared/2017/January/hubdoc.csv"

## set the end date for the statement
statement_end_date <- mdy("06-30-2017")
statement_begin_date <- mdy("06-01-2017")
################################################################################################################
qbo <- read_excel(qbo_file, 
                       col_types = c("text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "text", "numeric", "text", "text", 
                                     "numeric", "text"), skip = 1)
qbo$Date <- mdy(qbo$Date)

qbo <- qbo %>%
  mutate(Deposit = coalesce(Deposit, 0), Payment = coalesce(Payment, 0)) %>%
  mutate(`Reconciliation Status` = coalesce(`Reconciliation Status`, "")) %>%
  mutate(Total = Deposit - Payment)

qbo_month <- qbo %>%
  filter(Date <= statement_end_date) %>%
  filter(`Reconciliation Status` != "Reconciled") %>%
  mutate(file_type = "qbo")

qbo_withdrawals <- qbo_month %>%
  filter(Total < 0)
qbo_deposits <- qbo_month %>%
  filter(Total > 0)

# Hubdoc stuff

hubdoc <- read_csv(hubdoc_file, col_names = FALSE)
names(hubdoc) <- c("Date", "Amount", "Star", "Number", "Memo")
hubdoc$Number <- as.numeric(hubdoc$Number)
hubdoc <- hubdoc %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Total = Amount) %>%
  mutate(Number = coalesce(Number, 0))

hubdoc_withdrawals <- hubdoc %>%
  filter(Total < 0)

hubdoc_deposits <- hubdoc %>%
  filter(Total > 0)

#################################### Start here
## Tells you if there are any duplicate values, False = yes
length(unique(qbo_month$Total)) == nrow(qbo_month)
length(unique(hubdoc$Total)) == nrow(hubdoc)

## Tells you which values occured at what frequencies
dup_q <- qbo_month[duplicated(qbo_month$Total),] %>%
  filter(Date >= statement_begin_date & Date <= statement_end_date)

total_counts_qbo <- qbo_month %>% 
  group_by(Total) %>% 
  mutate(count = n()) %>%
  mutate(File = "qbo")

n_q_1 <- total_counts_qbo[total_counts_qbo$count > 1,] %>%
  filter(Total != is.na(Total)) %>%
  select(Date, Payee, Memo, Total, count, File)


total_counts_h <- hubdoc %>%
  group_by(Total) %>%
  mutate(count = n()) %>%
  filter(Total != is.na(Total)) %>%
  mutate(File = "hubdoc")

n_h_1 <- total_counts_h[total_counts_h$count > 1,] %>%
  filter(Total != is.na(Total)) %>%
  select(Date, Memo, Total, count, File)

duplicates <- rbind(n_q_1, n_h_1)


## Deposits
by = "Total"
deposits_need_to_be_entered <- anti_join(hubdoc_deposits, qbo_deposits, by = by)
View(deposits_need_to_be_entered)
deposits_need_to_be_taken_out <- anti_join(qbo_deposits, hubdoc_deposits, by = by)
View(deposits_need_to_be_taken_out)

# setwd("C:/Users/Chris/Desktop/Kyle/HSquared/2017/March")
# write.csv(deposits_need_to_be_entered, file = "March deposits need to be entered.csv")

hw_count <- hubdoc_withdrawals %>%
  summarize(count = n())

### Withdrawals
withdrawals_need_to_be_entered <- anti_join(hubdoc_withdrawals, qbo_withdrawals, by = by)
withdrawals_need_to_be_taken_out <- anti_join(qbo_withdrawals, hubdoc_withdrawals, by = by)




## Returns the records with more than one occurence
qbo_duplicates <- qbo_month[qbo_month$Total %in% n_occur_qbo$Var1[n_occur_qbo$Freq > 1],]
hubdoc_duplicates <- hubdoc[hubdoc$Total %in% n_occur_hubdoc$Var1[n_occur_hubdoc$Freq > 1],]




