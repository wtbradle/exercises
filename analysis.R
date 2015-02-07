# Analysis for exercise 01 for RTI
# By Tyler Bradley

# Code for working with SQLite
# From http://www.r-bloggers.com/using-sqlite-in-r/
library("RSQLite")
con = dbConnect(drv=SQLite(),dbname="C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\exercise01\\exercise01.sqlite")
allTables = dbListTables(con)
for(name in allTables){
  print(paste(name,":",paste(dbListFields(con,name),collapse=", ")))
}
recordsTable<-dbGetQuery(con,"SELECT * from records")
names(recordsTable)
head(recordsTable)
nrow(recordsTable)

tableNames <- paste(allTables,collapse=", ")

bigTable <- dbGetQuery(con, paste("SELECT records.*, countries.name as country, education_levels.name as education_level, marital_statuses.name as marital_status,
                                  occupations.name as occupation, relationships.name as relationship, races.name as race, sexes.name as sex, workclasses.name as class 
                                  FROM", tableNames, "WHERE 
                                  records.country_id = countries.id and records.education_level_id = education_levels.id and
                                  records.marital_status_id = marital_statuses.id and records.occupation_id = occupations.id and 
                                  records.relationship_id = relationships.id and
                                  records.race_id = races.id and records.sex_id = sexes.id and records.workclass_id = workclasses.id ") )
head(bigTable)
nrow(bigTable) == nrow(recordsTable)

# Test it out
dbGetQuery(con,"SELECT * from relationships")
unique(cbind(bigTable$relationship_id,bigTable$relationship))

dbGetQuery(con,"SELECT * from workclasses")
unique(cbind(bigTable$workclass_id,bigTable$class))
#Looks good

#Remove redunant  "_id"s

bigTable<-bigTable[,substr(names(bigTable),nchar(names(bigTable))-2,nchar(names(bigTable)))!='_id']
names(bigTable)
nrow(bigTable)

# Save the data someplace for later use
write.csv(bigTable,"C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\flattened.csv",row.names=FALSE)

# Read in to make sure it was saved correctly
data<-read.csv("C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\flattened.csv")
head(data)
nrow(data)
names(data)
sum(data!=bigTable)
# Looks good

# Investigate the data
max(data$id)

mean(data$over_50k)

summary(data$age)
hist(data$age)

summary(data$education_num)
hist(data$education_num)
boxplot(data$education_num ~ data$education_level)
prop.table(table(data$education_num,data$over_50k),1)
# So pick one of these, probably education_num for interpretation

summary(data$capital_gain)
hist(data$capital_gain)
# Probably toss this or separate this people out
sum(data$capital_gain!=0)
#Maybe switch to binary
prop.table(table(data$capital_gain!=0,data$over_50k))

summary(data$capital_loss)
hist(data$capital_loss)
# Probably toss this or separate this people out
sum(data$capital_loss!=0)
#Maybe switch to binary
prop.table(table(data$capital_loss!=0,data$over_50k))
prop.table(table(data$capital_loss!=0,data$capital_gain!=0))
plot(data$capital_loss,data$capital_gain,col=c("black","red")[data$over_50k+1])

summary(data$hours_week)
hist(data$hours_week)
# no zeros?  Even for unemployeed?
boxplot(data$hours_week ~ data$occupation)
boxplot(data$hours_week ~ data$class)
# That's a lot of hours for never-worked
table(data$occupation,data$over_50k)
# Might need to drop some of these for small sample size
# Looks like ? is a decent indicator for under 50k
table(data$class,data$over_50k)
# Toss never-worked

table(data$country,data$over_50k)
# Toss out small sample size
# Maybe combine into regions?
prop.table(table(data$country,data$over_50k),1)

prop.table(table(data$marital_status,data$over_50k),1)
prop.table(table(data$relationship,data$over_50k),1)
table(data$marital_status,data$relationship)

mean(data$sex=="Female")
# No even close to 50%?
prop.table(table(data$sex,data$over_50k))
prop.table(table(data$sex,data$over_50k),1)

table(data$race,data$over_50k)
prop.table(table(data$race,data$over_50k),1)
# no hispanic?
table(data$race,data$country)
# apparently hispanic go under white

# Split into different training, vaildation, and testing.
# Make sure prop of 0ver_50k is similar, probably need sampling package
