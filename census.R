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

## Disconnect from the Database
dbDisconnect(dcon)
