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
sapply(data,function(x)is.factor(x))
data$over_50k<-factor(data$over_50k)
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
sum(data$capital_loss!=0)
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
data<-data[data$class!="Never-worked",]
data$class<-factor(data$class)


table(data$country,data$over_50k)
# Toss out small sample size
data<-data[data$country!='Holand-Netherlands',]
data$country<-factor(data$country)
# Maybe combine into regions?
# What is 'South'?
prop.table(table(data$country,data$over_50k),1)

prop.table(table(data$marital_status,data$over_50k),1)
prop.table(table(data$relationship,data$over_50k),1)
table(data$marital_status,data$relationship)

mean(data$sex=="Female")
# Not even close to 50%?
prop.table(table(data$sex,data$over_50k))
prop.table(table(data$sex,data$over_50k),1)

table(data$race,data$over_50k)
prop.table(table(data$race,data$over_50k),1)
# no hispanic?
table(data$race,data$country)
# apparently hispanic go under white

# Split into different training, vaildation, and testing.
# Make sure prop of 0ver_50k is similar, probably need sampling package
set.seed(prod(strtoi(charToRaw('RTI'))))

data<-data[,names(data)!=c('id','education_level')]
parts<-sample(c('train','valid','test'),size=nrow(data),replace=TRUE,prob=c(.60,.20,.20))
# Glance at the difference
dataTrain<-data[parts=='train',]
summary(dataTrain)
dataValid<-data[parts=='valid',]
summary(dataValid)
dataTest<-data[parts=='test',]
summary(dataTest)
# Looks pretty similar, good to go

table(dataTrain$occupation,dataTrain$over_50k==1)
summary(dataTrain[dataTrain$over_50k==0,])

## Build some models
modelLog<-glm(over_50k ~ ., data=dataTrain, family=binomial)
summary(modelLog)
#anova(modelLog,test="Chisq")
#drop1(modelLog) # No Difference
preds<-predict(modelLog,newdata=dataValid,type='response')
head(preds)

# http://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package
library('ROCR')
pred<-prediction(preds,dataValid$over_50k)
perf<-performance(pred,"tpr","fpr")
plot(perf)
# Ok, but the warnings are concerning

# http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
library(glmnet)
?glmnet
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataTrain[,names(dataTrain)[toStandardize]]
stand<-scale(toStandardize)
head(stand)
dataTrain2<-dataTrain[,names(dataTrain)[sapply(data,function(x)is.factor(x))]]
dataTrain2<-cbind(dataTrain2,stand)
head(dataTrain2)
x <- model.matrix(over_50k ~ .,data=dataTrain2)[,-1]
head(x)
y<-as.matrix(dataTrain2$over_50k)
modelLog2<-glmnet(x,y,alpha=1,family='binomial')
modelLog2
plot(modelLog2,xvar="lambda")
# 25 variables?
abline(v=log(modelLog2$lambda[37]))

names(as.data.frame(x))[as.vector(coef(modelLog2)[,37])[-1]!=0]
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataValid[,names(dataValid)[toStandardize]]
standVal<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
head(xValid)

preds<-predict(modelLog2,newx=xValid,type='response',s=modelLog2$lambda[37])
head(preds)

pred2<-prediction(preds,dataValid$over_50k)
perf2<-performance(pred2,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
# Not to shabby

# Random forest, just because
library(randomForest)
modelRF<-randomForest(over_50k~.,dataTrain)
plot(modelRF)
# 100 trees looks good
preds<-predict(modelRF,newdata=dataValid,type="prob")
head(preds)

pred3<-prediction(preds[,2],dataValid$over_50k)
perf3<-performance(pred3,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="blue")
# Not as good

# LASSO is best

# Get cutoff
preds<-predict(modelLog2,newx=xValid,type='response',s=modelLog2$lambda[37])
head(preds)
compare<-data.frame("actual"=dataValid$over_50k,preds=as.vector(preds))
head(compare)

cutoff<-.5
cutoffMisclass<-function(cutoff){
  cuts<-rep(0,nrow(compare))
  cuts<-ifelse(compare$preds<cutoff,0,1)
  misclass<-mean(compare$actual!=cuts)
  return(misclass)
}

cutoffs<-seq(0.01,.99,by=.01)
misclasses<-unlist(lapply(cutoffs,FUN=cutoffMisclass))
plot(cutoffs,misclasses)
abline(h=min(misclasses),col="red")
cutoffs[misclasses==min(misclasses)]
# .5 looks good

# Now to get testing dataset



#If Class=Never-worked, or in holland then 0.