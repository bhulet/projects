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

## UNION houseA and houseB
dbSendQuery(dcon,"
            CREATE TABLE house AS
            SELECT* 
            FROM houseA
            UNION ALL
            SELECT* 
            FROM houseB;
            ")

## UNION peopleA and peopleB
dbSendQuery(dcon,"
            CREATE TABLE people AS
            SELECT* 
            FROM peopleA
            UNION ALL
            SELECT* 
            FROM peopleB;
            ")

## Checking that the Union tables were created correctly
dbListTables(dcon)
identical(dbListFields(dcon, "house"), dbListFields(dcon, "houseB"))
identical(dbListFields(dcon, "people"), dbListFields(dcon, "peopleB"))

res<-dbSendQuery(dcon,"
SELECT COUNT(RT)
FROM house;
")
ans <- fetch(res,-1)
dbClearResult(res)

res<-dbSendQuery(dcon,"
SELECT COUNT(RT) 
FROM houseA;
")
ans1 <- fetch(res,-1)
dbClearResult(res)

res <- dbSendQuery(dcon,"
SELECT COUNT(RT) 
FROM houseB;
")
ans11 <- fetch(res,-1)
dbClearResult(res)

## checking that the number of rows for the houseUnion = houseA + houseB
identical(ans, ans1 + ans11)

res<-dbSendQuery(dcon,"
SELECT COUNT(RT)
FROM people;
")
ansP <- fetch(res,-1)
dbClearResult(res)

res<-dbSendQuery(dcon,"
SELECT COUNT(RT)
FROM peopleA;
")
ans2 <- fetch(res,-1)
dbClearResult(res)

res<-dbSendQuery(dcon,"
SELECT COUNT(RT)
FROM peopleB;
")
ans22 <- fetch(res,-1)
dbClearResult(res)

## checking that the number of rows for the peopleUnion = peopleA + peopleB
identical(ansP, ans2+ans22)

## There are a different number of rows between tableshouse and tablepeople
identical(ans, ansP)

## There are approximately 2x as many entries for People as Houses
ansP/ans

## Removing the old tables
dbRemoveTable(dcon, "houseA")
dbRemoveTable(dcon, "houseB")
dbRemoveTable(dcon, "peopleA")
dbRemoveTable(dcon, "peopleB")

## Checking to see if the tables were removed
dbListTables(dcon)

## Close Connection
dbDisconnect(dcon)

## Clean the Environment
rm(ans, ans1, ans11, ans2, ans22, ansP, res, dcon)

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
FROM people
WHERE SCHL IN ('20', '21' , '22', '23', '24');
")
data <- fetch(res,-1)
dbClearResult(res)

numberDegree <- nrow(data)
cat("In 2013 there were", numberDegree, "people who reported having a college degree")

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
SELECT SCHL     
FROM people
WHERE FOD1P  IN ('3702' , '6212')
OR FOD2P IN ('3702' , '6212');
")
data <- fetch(res,-1)
dbClearResult(res)

numberStats <- nrow(data)

cat("In 2013 there were", numberStats, "people who have a stats or stats related degree")
cat("This is", round(numberStats/numberDegree, 4), "% of the total number of College Degrees")

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

## Clean Enviornment 
rm(data, res, dcon, numberStats, numberDegree)
