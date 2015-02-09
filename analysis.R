# Analysis for exercise 01 for RTI
# By Tyler Bradley
# Note: This really should be separated into different files

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
par(mfrow=c(2,1))
hist(data$age[data$over_50k==0])
hist(data$age[data$over_50k==1])
par(mfrow=c(1,1))

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
par(mfrow=c(2,1))
hist(data$hours_week[data$over_50k==0])
hist(data$hours_week[data$over_50k==1])
par(mfrow=c(1,1))
boxplot(data$hours_week ~ data$occupation)
boxplot(data$hours_week ~ data$class)
# That's a lot of hours for never-worked
table(data$occupation,data$over_50k)
# Might need to drop some of these for small sample size
# Looks like ? is a decent indicator for under 50k
prop.table(table(data$occupation,data$over_50k),1)
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

data<-data[,!names(data)%in%c('id')]
parts<-sample(c('train','valid','test'),size=nrow(data),replace=TRUE,prob=c(.60,.20,.20))
# Glance at the difference
dataTrain<-data[parts=='train',]
summary(dataTrain)
dataValid<-data[parts=='valid',]
summary(dataValid)
dataTest<-data[parts=='test',]
summary(dataTest)
# Looks pretty similar, good to go


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
# play around with this
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

# use glm with lasso selection
vars<-names(as.data.frame(x))[as.vector(coef(modelLog2)[,25])[-1]!=0]
xNew <- as.data.frame(x)
names(xNew)
xNew<-xNew[,names(xNew)%in%vars]
head(xNew)
dataNew<-cbind(xNew,over_50k=dataTrain$over_50k)
names(dataNew)
modelLog3<-glm(over_50k ~ .,data=dataNew,family="binomial")

toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataValid[,names(dataValid)[toStandardize]]
standVal<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
head(xValid)
xNewValid <- as.data.frame(xValid)
xNewValid<-xNewValid[,names(xNewValid)%in%vars]
head(xNewValid)
dataNewValid<-cbind(xNewValid,over_50k=dataValid$over_50k)
names(dataNewValid)

preds<-predict(modelLog3,newdata=dataNewValid,type='response')
head(preds)

pred3<-prediction(preds,dataValid$over_50k)
perf3<-performance(pred3,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="green")
# 10 is not good
# 20 is still good
# Go with 20
# Not to shabby

# Check is standarization is needed
vars<-names(as.data.frame(x))[as.vector(coef(modelLog2)[,33])[-1]!=0]
x<-data.frame(model.matrix(over_50k ~ .,data=dataTrain)[,-1])
head(x)
x<-x[,names(x)%in%vars]
head(x)
dataNew<-cbind(x,over_50k=dataTrain$over_50k)
names(dataNew)
head(dataNew)
modelLog4<-glm(over_50k ~ .,data=dataNew,family="binomial")

xValid <- data.frame(model.matrix(over_50k ~ .,data=dataValid)[,-1])
head(xValid)
xValid<-xValid[,names(xValid)%in%vars]
head(xValid)
dataNewValid<-cbind(xValid,over_50k=dataValid$over_50k)
names(dataNewValid)

preds<-predict(modelLog4,newdata=dataNewValid,type='response')
head(preds)

pred4<-prediction(preds,dataValid$over_50k)
perf4<-performance(pred4,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="green")
plot(perf4,add=TRUE,col="purple")
# Yes, standarization is needed

# Random forest, just because
library(randomForest)
?randomForest
modelRF<-randomForest(over_50k~.,dataTrain,ntree=150)
plot(modelRF)
# 75 trees looks good
modelRF<-randomForest(over_50k~.,dataTrain,ntree=75)
preds<-predict(modelRF,newdata=dataValid,type="prob")
head(preds)

pred5<-prediction(preds[,2],dataValid$over_50k)
perf5<-performance(pred5,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="green")
plot(perf4,add=TRUE,col="purple")
plot(perf5,add=TRUE,col="blue")
# Not as good

# Standarized GLM with Lasso selection is best

# Get cutoff
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataValid[,names(dataValid)[toStandardize]]
standVal<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
head(xValid)
xNewValid <- as.data.frame(xValid)
xNewValid<-xNewValid[,names(xNewValid)%in%vars]
head(xNewValid)
dataNewValid<-cbind(xNewValid,over_50k=dataValid$over_50k)
names(dataNewValid)

preds<-predict(modelLog3,newdata=dataNewValid,type='response')
head(preds)

preds<-predict(modelLog3,newdata=dataNewValid,type='response')
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
#  Rebuild model

newData<-data[parts!='test',]
head(newData)
toStandardize<-sapply(newData,function(x)!is.factor(x))
toStandardize<-newData[,names(newData)[toStandardize]]
stand<-scale(toStandardize)
head(stand)
newData2<-newData[,names(newData)[sapply(data,function(x)is.factor(x))]]
newData2<-cbind(newData2,stand)
head(newData2)
x <- model.matrix(over_50k ~ .,data=newData2)[,-1]
head(x)
y<-as.matrix(newData2$over_50k)
modelLog6<-glmnet(x,y,alpha=1,family='binomial')
modelLog6
plot(modelLog6,xvar="lambda")
# 20 variables
# 37 again
abline(v=log(modelLog6$lambda[32]))

# use glm with lasso selection
vars<-names(as.data.frame(x))[as.vector(coef(modelLog6)[,32])[-1]!=0]
xNew <- as.data.frame(x)
names(xNew)
xNew<-xNew[,names(xNew)%in%vars]
head(xNew)
dataNew<-cbind(xNew,over_50k=newData$over_50k)
names(dataNew)
modelLog7<-glm(over_50k ~ .,data=dataNew,family="binomial")


toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataTest[,names(dataTest)[toStandardize]]
standTest<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
head(standTest)
dataTest2<-dataTest[,names(dataTest)[sapply(data,function(x)is.factor(x))]]
dataTest2<-cbind(dataTest2,data.frame(standTest))
head(dataTest2)
xTest <- model.matrix(over_50k ~ .,data=dataTest2)[,-1]
head(xTest)
xNewTest <- as.data.frame(xTest)
xNewTest<-xNewTest[,names(xNewTest)%in%vars]
head(xNewTest)
dataNewTest<-cbind(xNewTest,over_50k=dataTest$over_50k)
names(dataNewTest)

preds<-predict(modelLog7,newdata=dataNewTest,type='response')
head(preds)

predTest<-prediction(preds,dataTest$over_50k)
perfTest<-performance(predTest,"tpr","fpr")
plot(perfTest)
unlist(slot(performance(predTest,"auc"),"y.values"))
# Looks good
predictions<-data.frame(preds=preds,actual=dataTest$over_50k)
# .5 comes from the previous cutoff selection
predictions$predsRound<-ifelse(predictions$preds<.5,0,1)
head(predictions)
table(predictions$predsRound,predictions$actual,dnn=c("Pred","Act"))
prop.table(table(predictions$predsRound,predictions$actual,dnn=c("Pred","Act")))
mean(predictions$predsRound!=predictions$actual)
# Misclassification rate of 14.86%

# Make a chart
library(ggplot2)
vars
# Education

max(by(data$age,data$education_level,min))

names(data)
unique(data[,c("education_level","education_num")])[1]
education<-data.frame("education_level"=unique(data[,c("education_level","education_num")])[1])
education$education_num<-unique(data[,c("education_level","education_num")])[2]
education<-education[order(education$education_num),]
education$education_level<-factor(education$education_level,levels=education$education_level[order(education$education_num)])
education$under50k<-table(data$education_num,data$over_50k)[,1]
education$over50k<-table(data$education_num,data$over_50k)[,2]
education$prop<-education$over50k/(education$over50k+education$under50k)
head(education)
qplot(x=education_level,y=prop,data=education,geom="bar",stat="identity",ylim=c(0,1))

# take out young people
# It's not really far to keep student that haven't reached a level yet
# ie, you don't want to compare preschoolers to doctorate student because of the age difference
max(by(data$age,data$education_level,min))
dataOld<-data[data$age>25,]

names(data)
unique(dataOld[,c("education_level","education_num")])[1]
education<-data.frame("education_level"=unique(dataOld[,c("education_level","education_num")])[1])
education$education_num<-unique(dataOld[,c("education_level","education_num")])[2]
education<-education[order(education$education_num),]
education$education_level<-factor(education$education_level,levels=education$education_level[order(education$education_num)])
education$under50k<-table(dataOld$education_num,dataOld$over_50k)[,1]
education$over50k<-table(dataOld$education_num,dataOld$over_50k)[,2]
education$prop<-education$over50k/(education$over50k+education$under50k)
head(education)

qplot(x=education_level,y=prop,data=education,geom="bar",stat="identity",ylim=c(0,1),
      ylab="Proportion of over $50k/year",xlab="Highest level of Education",
      main="Proportion over $50/year by Education Level \n for 25 years old and older") + 
      theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))


#If Class=Never-worked, or in holland then 0.