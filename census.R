################################################################################
##
##   2013 - USA CENSUS 
##   kaggle: https://www.kaggle.com/c/2013-american-community-survey
##
##   Download the four data files from Kaggle and save to your working directory
##
################################################################################

library(data.table)
library(sqldf)

## Create database and connection
dcon <- dbConnect(SQLite(), dbname="census.sqlite")

## read the first table into memory, write to database, remove from memory
houseA <- fread("ss13husa.csv")
dbWriteTable(conn = dcon, name = "houseA", value = houseA, row.names = FALSE)
rm(houseA)

## read the 2nd table into memory, write to database, remove from memory
houseB <- fread("ss13husb.csv")
dbWriteTable(conn = dcon, name = "houseB", value = houseB, row.names = FALSE)
rm(houseB)

## read the 3rd table into memory, write to database, remove from memory
peopleA <- fread("ss13pusa.csv")   ### possible error for column 126 #####
dbWriteTable(conn = dcon, name = "peopleA", value = peopleA, row.names = FALSE)
rm(peopleA)

## read the 4th table into memory, write to database, remove from memory
peopleB <- fread("ss13pusb.csv")
dbWriteTable(conn = dcon, name = "peopleB", value = peopleB, row.names = FALSE)
rm(peopleB)

## Check to see that all tables are in the database
dbListTables(dcon)

## check to see that all tables have the correct column names
dbListFields(dcon, "houseA")
identical(dbListFields(dcon, "houseA"), dbListFields(dcon, "houseB"))

dbListFields(dcon, "peopleA")
identical(dbListFields(dcon, "peopleA"), dbListFields(dcon, "peopleB"))

## Close Connection
dbDisconnect(dcon)

#################################################################
## 
## Exploring College Graduates and Specifically Stats Majors
##
################################################################

library(ggplot2)

## Create connection with the database
dcon <- dbConnect(SQLite(), dbname="census.sqlite")

## Selecting all colege graduates
res <- dbSendQuery(conn=dcon, "
SELECT SCHL 
FROM peopleA
WHERE SCHL IN ('20', '21' , '22', '23', '24')
UNION ALL
SELECT SCHL 
FROM peopleB
WHERE SCHL IN ('20', '21' , '22', '23', '24');
")
data <- fetch(res,-1)
dbClearResult(res)

## HISTOGRAM of college grads
ggplot(data = data) +
  aes(as.factor(SCHL)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Educational Attainment \n For Census Respondents") +
  scale_x_discrete(name ="" , labels=c("20" = "Associates degree", "21" = "Bachelor's degree", "22" = "Master's degree", 
                                        "23" = "Professional degree", "24"= "Doctorate degree")) +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Selecting All of the Statistics Majors FOD1P (Field of First Degree) AND FOD2P (2nd Degree)   
res <- dbSendQuery(conn=dcon, "
SELECT SCHL, SCIENGP, SCIENGRLP       
FROM peopleA
WHERE FOD1P  IN ('3702' , '6212')
UNION ALL
SELECT SCHL, SCIENGP, SCIENGRLP 
FROM peopleB
WHERE FOD1P IN ('3702' , '6212');
")
data <- fetch(res,-1)
dbClearResult(res)

## HISTORGRAM OF STATS Graduates
ggplot(data = data) +
  aes(as.factor(SCHL)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Educational Attainment \n For Statistics Majors") +
  scale_x_discrete(name = "" , labels=c("20" = "Associates degree", "21" = "Bachelor's degree", "22" = "Master's degree", 
                                                   "23" = "Professional degree", "24"= "Doctorate degree")) +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Disconnect from the Database
dbDisconnect(dcon)
