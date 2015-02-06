library("RSQLite")
con = dbConnect(drv=SQLite(),dbname="C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\exercise01\\exercise01.sqlite")
alltables = dbListTables(con)
alltables
