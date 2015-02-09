##########----------##########
##########----------##########
# Analysis for exercise 01 for RTI
# By Tyler Bradley
# Note: This really should be separated into different files, but
# it is kept into one file for storage and possible print out.
##########----------##########
##########----------##########

##########----------##########
### Working with SQLite
##########----------##########

# Code for working with SQLite
# From http://www.r-bloggers.com/using-sqlite-in-r/
library("RSQLite")
con = dbConnect(drv=SQLite(),dbname="C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\exercise01\\exercise01.sqlite")
allTables = dbListTables(con)
# Look at the tables
for(name in allTables){
  print(paste(name,":",paste(dbListFields(con,name),collapse=", ")))
}
#recordsTable<-dbGetQuery(con,"SELECT * from records")
#names(recordsTable)
#head(recordsTable)
#nrow(recordsTable)

# Collapse everything into one big table
tableNames <- paste(allTables,collapse=", ")
bigTable <- dbGetQuery(con, paste("SELECT records.*, countries.name as country, education_levels.name as education_level, marital_statuses.name as marital_status,
                                  occupations.name as occupation, relationships.name as relationship, races.name as race, sexes.name as sex, workclasses.name as class 
                                  FROM", tableNames, "WHERE 
                                  records.country_id = countries.id and records.education_level_id = education_levels.id and
                                  records.marital_status_id = marital_statuses.id and records.occupation_id = occupations.id and 
                                  records.relationship_id = relationships.id and
                                  records.race_id = races.id and records.sex_id = sexes.id and records.workclass_id = workclasses.id ") )
# Check it out
#head(bigTable)
#nrow(bigTable) == nrow(recordsTable)

# Double check a little bit
#dbGetQuery(con,"SELECT * from relationships")
#unique(cbind(bigTable$relationship_id,bigTable$relationship))

#dbGetQuery(con,"SELECT * from workclasses")
#unique(cbind(bigTable$workclass_id,bigTable$class))
# Looks good

# Remove redunant  "_id"s
bigTable<-bigTable[,substr(names(bigTable),nchar(names(bigTable))-2,nchar(names(bigTable)))!='_id']
#names(bigTable)
#nrow(bigTable)

# Save the data someplace for later use
write.csv(bigTable,"C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\exercise01\\flattened.csv",row.names=FALSE)

# Read in to make sure it was saved correctly
data<-read.csv("C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\exercise01\\flattened.csv")
#head(data)
#nrow(data)
#names(data)
#sum(data!=bigTable)
# Looks good

# Check Factors
sapply(data,function(x)is.factor(x))
data$over_50k<-factor(data$over_50k)

##########----------##########
### Investigate the data
##########----------##########

# Missing?
sum(is.na(data))
# Nope

# Every row is an individual
max(data$id)

# Base-line prop
mean(as.numeric(data$over_50k)-1)

# Look at age 
summary(data$age)
hist(data$age)
par(mfrow=c(2,1))
hist(data$age[data$over_50k==0])
hist(data$age[data$over_50k==1])
par(mfrow=c(1,1))
# Looks like it might be important

# Look at Education
summary(data$education_num)
hist(data$education_num)
# One to one relationship between education number and education level
boxplot(data$education_num ~ data$education_level)
prop.table(table(data$education_num,data$over_50k),1)
# Looks like this might be important
# Maybe only pick one of these, probably education_num for interpretation

# Look at Capital
summary(data$capital_gain)
hist(data$capital_gain)
# Maybe switch to binary 0 or not
sum(data$capital_gain!=0)
prop.table(table(data$capital_gain!=0,data$over_50k))

summary(data$capital_loss)
hist(data$capital_loss)
sum(data$capital_loss!=0)
prop.table(table(data$capital_loss!=0,data$over_50k))
prop.table(table(data$capital_loss!=0,data$capital_gain!=0))
# No both none zero
plot(data$capital_loss,data$capital_gain,col=c("black","red")[as.numeric(data$over_50k)])
# ok?

# Look at Working factors
summary(data$hours_week)
hist(data$hours_week)
# no zeros?  Even for unemployeed?
# 99 is a lot, but I guess doable
par(mfrow=c(2,1))
hist(data$hours_week[data$over_50k==0])
hist(data$hours_week[data$over_50k==1])
par(mfrow=c(1,1))
boxplot(data$hours_week ~ data$occupation)
boxplot(data$hours_week ~ data$class)
# That's a lot of hours for never-worked
table(data$occupation,data$over_50k)
# Might need to drop some of these for small sample size
# Looks like ? is a decent indicator for under 50k, so don't drop it
prop.table(table(data$occupation,data$over_50k),1)
table(data$class,data$over_50k)
# Toss never-worked to avoid separation
data<-data[data$class!="Never-worked",]
data$class<-factor(data$class)

# Look into countries
table(data$country,data$over_50k)
# Toss out small sample size
data<-data[data$country!='Holand-Netherlands',]
data$country<-factor(data$country)
# Maybe combine into regions, ex into continents or countries with other similar aspects?
# What is 'South'?
prop.table(table(data$country,data$over_50k),1)

# Look into relationships
prop.table(table(data$marital_status,data$over_50k),1)
prop.table(table(data$relationship,data$over_50k),1)
table(data$marital_status,data$relationship)

# Look into sex
mean(data$sex=="Female")
# Not even close to 50%?
prop.table(table(data$sex,data$over_50k))
prop.table(table(data$sex,data$over_50k),1)

table(data$race,data$over_50k)
prop.table(table(data$race,data$over_50k),1)
# no hispanic?
table(data$race,data$country)
# apparently hispanic go under white

##########----------##########
### Split into different training, vaildation, and testing.
### Make sure prop of Over_50k and other variables are similar
##########----------##########

# Set seed for reproducability
set.seed(prod(strtoi(charToRaw('RTI'))))

# Remove ID variable since it is not needed
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



##########----------##########
### Build some models
##########----------##########


# Basic Logistic model
modelLog<-glm(over_50k ~ ., data=dataTrain, family=binomial)
# Warning about separation
summary(modelLog)
# Try to remove factors
#anova(modelLog,test="Chisq")
#drop1(modelLog) # No Difference
#step(modelLog, direction="both")
# None of those worked, but took a while, so they are commented out
preds1<-predict(modelLog,newdata=dataValid,type='response')
# Warning because of redundant factors, (see na's in summary(modelLog))
#head(preds1)
#sum(is.na(preds))

# http://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package
library('ROCR')
pred1<-prediction(preds1,dataValid$over_50k)
perf1<-performance(pred,"tpr","fpr")
plot(perf1)
# Ok, but the warnings are concerning

# Use LASSO to reduce variables since other selection techniques didn't work
# http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
library(glmnet)
#?glmnet
# Standardize non factor variables
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataTrain[,names(dataTrain)[toStandardize]]
stand<-scale(toStandardize)
#head(stand)
dataTrain2<-dataTrain[,names(dataTrain)[sapply(data,function(x)is.factor(x))]]
dataTrain2<-cbind(dataTrain2,stand)
#head(dataTrain2)
# Conver factors to indicator variables
x <- model.matrix(over_50k ~ .,data=dataTrain2)[,-1]
#head(x)
y<-as.matrix(dataTrain2$over_50k)
modelLog2<-glmnet(x,y,alpha=1,family='binomial')
modelLog2
plot(modelLog2,xvar="lambda")
# Tried 10, 20, and 25 variables
# 20 worked well, and was reduced
abline(v=log(modelLog2$lambda[33]))

# Validate the model
#names(as.data.frame(x))[as.vector(coef(modelLog2)[,33])[-1]!=0]
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataValid[,names(dataValid)[toStandardize]]
standVal<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
#head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
#head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
#head(xValid)

preds2<-predict(modelLog2,newx=xValid,type='response',s=modelLog2$lambda[37])
# No warnings
#head(preds)
#sum(is.na(preds))

pred2<-prediction(preds2,dataValid$over_50k)
perf2<-performance(pred2,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")

# use glm with lasso selection
# Get variables
vars<-names(as.data.frame(x))[as.vector(coef(modelLog2)[,33])[-1]!=0]
xNew <- as.data.frame(x)
#names(xNew)
# Reduce the data down to the variables chosen by LASSO
xNew<-xNew[,names(xNew)%in%vars]
#head(xNew)
dataNew<-cbind(xNew,over_50k=dataTrain$over_50k)
#names(dataNew)
modelLog3<-glm(over_50k ~ .,data=dataNew,family="binomial")

toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataValid[,names(dataValid)[toStandardize]]
standVal<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
#head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
#head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
#head(xValid)
xNewValid <- as.data.frame(xValid)
xNewValid<-xNewValid[,names(xNewValid)%in%vars]
#head(xNewValid)
dataNewValid<-cbind(xNewValid,over_50k=dataValid$over_50k)
#names(dataNewValid)

preds3<-predict(modelLog3,newdata=dataNewValid,type='response')
#head(preds3)
#sum(is.na(preds3))

pred3<-prediction(preds3,dataValid$over_50k)
perf3<-performance(pred3,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="green")
# 10 is not good
# 20 is still good
# Go with 20
# Not to shabby

# Check is standarization is needed
# Rerun the previous code, but don't standarize the variables
vars<-names(as.data.frame(x))[as.vector(coef(modelLog2)[,33])[-1]!=0]
x<-data.frame(model.matrix(over_50k ~ .,data=dataTrain)[,-1])
#head(x)
x<-x[,names(x)%in%vars]
#head(x)
dataNew<-cbind(x,over_50k=dataTrain$over_50k)
#names(dataNew)
#head(dataNew)
modelLog4<-glm(over_50k ~ .,data=dataNew,family="binomial")

xValid <- data.frame(model.matrix(over_50k ~ .,data=dataValid)[,-1])
#head(xValid)
xValid<-xValid[,names(xValid)%in%vars]
#head(xValid)
dataNewValid<-cbind(xValid,over_50k=dataValid$over_50k)
#names(dataNewValid)

preds4<-predict(modelLog4,newdata=dataNewValid,type='response')
#head(preds4)
#sum(is.na(preds4))

pred4<-prediction(preds4,dataValid$over_50k)
perf4<-performance(pred4,"tpr","fpr")
plot(perf)
plot(perf2,add=TRUE,col="red")
plot(perf3,add=TRUE,col="green")
plot(perf4,add=TRUE,col="mediumorchid3")
# Yes, standarization is needed

# Random forest, incase there are interesting relationship regression isn't catching
library(randomForest)
?randomForest
# Default ntree is 500, which takes forever and is not needed
modelRF<-randomForest(over_50k~.,dataTrain,ntree=150)
plot(modelRF)
# 75 trees looks good
modelRF<-randomForest(over_50k~.,dataTrain,ntree=75)
preds5<-predict(modelRF,newdata=dataValid,type="prob")
head(preds5)

pred5<-prediction(preds5[,2],dataValid$over_50k)
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
#head(standVal)
dataValid2<-dataValid[,names(dataValid)[sapply(data,function(x)is.factor(x))]]
dataValid2<-cbind(dataValid2,data.frame(standVal))
#head(dataValid2)
xValid <- model.matrix(over_50k ~ .,data=dataValid2)[,-1]
#head(xValid)
xNewValid <- as.data.frame(xValid)
xNewValid<-xNewValid[,names(xNewValid)%in%vars]
#head(xNewValid)
dataNewValid<-cbind(xNewValid,over_50k=dataValid$over_50k)
#names(dataNewValid)

preds<-predict(modelLog3,newdata=dataNewValid,type='response')
#head(preds)

compare<-data.frame("actual"=dataValid$over_50k,preds=as.vector(preds))
#head(compare)

# Function used to find the misclassifcation rate for an individual cutoff
cutoffMisclass<-function(cutoff){
  cuts<-rep(0,nrow(compare))
  cuts<-ifelse(compare$preds<cutoff,0,1)
  misclass<-mean(compare$actual!=cuts)
  return(misclass)
}

# Plot the different cutoff options with their corresponding misclassification rate
cutoffs<-seq(0.01,.99,by=.01)
misclasses<-unlist(lapply(cutoffs,FUN=cutoffMisclass))
plot(cutoffs,misclasses)
abline(h=min(misclasses),col="red")
cutoffs[misclasses==min(misclasses)]
# .5 looks good, round for ease of use

# Now to get testing scores
#  Rebuild model

newData<-data[parts!='test',]
#head(newData)
toStandardize<-sapply(newData,function(x)!is.factor(x))
toStandardize<-newData[,names(newData)[toStandardize]]
stand<-scale(toStandardize)
#head(stand)
newData2<-newData[,names(newData)[sapply(data,function(x)is.factor(x))]]
newData2<-cbind(newData2,stand)
#head(newData2)
x <- model.matrix(over_50k ~ .,data=newData2)[,-1]
#head(x)
y<-as.matrix(newData2$over_50k)
modelLog6<-glmnet(x,y,alpha=1,family='binomial')
modelLog6
plot(modelLog6,xvar="lambda")
# 20 variables
# 32 again
abline(v=log(modelLog6$lambda[32]))

# use glm with lasso selection
vars2<-names(as.data.frame(x))[as.vector(coef(modelLog6)[,32])[-1]!=0]
sum(!vars==vars2)
# Looks like the same variables are used, which is great
vars<-vars2
xNew <- as.data.frame(x)
#names(xNew)
xNew<-xNew[,names(xNew)%in%vars]
#head(xNew)
dataNew<-cbind(xNew,over_50k=newData$over_50k)
#names(dataNew)
modelLog7<-glm(over_50k ~ .,data=dataNew,family="binomial")

toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-dataTest[,names(dataTest)[toStandardize]]
standTest<-scale(toStandardize,center=attr(stand,"scaled:center"),scale=attr(stand,"scaled:scale"))
#head(standTest)
dataTest2<-dataTest[,names(dataTest)[sapply(data,function(x)is.factor(x))]]
dataTest2<-cbind(dataTest2,data.frame(standTest))
#head(dataTest2)
xTest <- model.matrix(over_50k ~ .,data=dataTest2)[,-1]
#head(xTest)
xNewTest <- as.data.frame(xTest)
xNewTest<-xNewTest[,names(xNewTest)%in%vars]
#head(xNewTest)
dataNewTest<-cbind(xNewTest,over_50k=dataTest$over_50k)
#names(dataNewTest)

preds<-predict(modelLog7,newdata=dataNewTest,type='response')
#head(preds)
#sum(is.na(preds))

predTest<-prediction(preds,dataTest$over_50k)
perfTest<-performance(predTest,"tpr","fpr")
plot(perfTest)
# Area under curve
unlist(slot(performance(predTest,"auc"),"y.values"))
# Looks good
predictions<-data.frame(preds=preds,actual=dataTest$over_50k)
# .5 comes from the previous cutoff selection
predictions$predsRound<-ifelse(predictions$preds<.5,0,1)
#head(predictions)
table(predictions$predsRound,predictions$actual,dnn=c("Pred","Act"))
prop.table(table(predictions$predsRound,predictions$actual,dnn=c("Pred","Act")))
mean(predictions$predsRound!=predictions$actual)
# Misclassification rate of 14.86%

# Final model using all the data for precise model parameter estimates
# Redo, again
toStandardize<-sapply(data,function(x)!is.factor(x))
toStandardize<-data[,names(data)[toStandardize]]
stand<-scale(toStandardize)
#head(stand)
data2<-data[,names(data)[sapply(data,function(x)is.factor(x))]]
data2<-cbind(data2,stand)
#head(data2)
x <- model.matrix(over_50k ~ .,data=data2)[,-1]
#head(x)
y<-as.matrix(data2$over_50k)
modelLogLASSO<-glmnet(x,y,alpha=1,family='binomial')
modelLogLASSO
plot(modelLog6,xvar="lambda")
# 20 variables
# 33 again
abline(v=log(modelLog6$lambda[33]))

# use glm with lasso selection
vars2<-names(as.data.frame(x))[as.vector(coef(modelLog6)[,33])[-1]!=0]
sum(!vars==vars2)
# Looks like the same variables are used, which is great
vars<-vars2
xNew <- as.data.frame(x)
#names(xNew)
xNew<-xNew[,names(xNew)%in%vars]
#head(xNew)
dataNew<-cbind(xNew,over_50k=data$over_50k)
#names(dataNew)
modelLogFinal<-glm(over_50k ~ .,data=dataNew,family="binomial")
summary(modelLogFinal)

##########----------##########
### Make a chart
##########----------##########
library(ggplot2)
vars
# Education sounds good

# Organize the data
# Pair up education level and education number
unique(data[,c("education_level","education_num")])
# Make a dataframe for those levels
education<-data.frame("education_level"=unique(data[,c("education_level","education_num")])[1])
education$education_num<-unique(data[,c("education_level","education_num")])[2]
# Need to order education_level by education number for the plot at the end
education<-education[order(education$education_num),]
education$education_level<-factor(education$education_level,levels=education$education_level[order(education$education_num)])
# Add in frequency stats
education$under50k<-table(data$education_num,data$over_50k)[,1]
education$over50k<-table(data$education_num,data$over_50k)[,2]
# Get the proportion to finish
education$prop<-education$over50k/(education$over50k+education$under50k)
#head(education)

# Plot the data
qplot(x=education_level,y=prop,data=education,geom="bar",stat="identity",ylim=c(0,1),
      ylab="Proportion of over $50k/year",xlab="Highest level of Education",
      main="Proportion over $50/year by Education Level \n for 25 years old and older") + 
      theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))

# take out young people
# It's not really far to keep students that haven't reached a level yet
# eg, you don't want to compare high schoolers to doctorate students because of the age difference
max(by(data$age,data$education_level,min))
dataOld<-data[data$age>25,]

# Make sure each education level isn't really young
# eg, don't only look at high school level if that only includes high schoolers
min(by(data$age,data$education_level,max))

# Same code as above, but with a different dataset
# Organize the data
# Pair up education level and education number
unique(dataOld[,c("education_level","education_num")])[1]
# Make a dataframe for those levels
education<-data.frame("education_level"=unique(dataOld[,c("education_level","education_num")])[1])
education$education_num<-unique(dataOld[,c("education_level","education_num")])[2]
# Need to order education_level by education number for the plot at the end
education<-education[order(education$education_num),]
education$education_level<-factor(education$education_level,levels=education$education_level[order(education$education_num)])
# Add in frequency stats
education$under50k<-table(dataOld$education_num,dataOld$over_50k)[,1]
education$over50k<-table(dataOld$education_num,dataOld$over_50k)[,2]
# Get the proportion to finish
education$prop<-education$over50k/(education$over50k+education$under50k)
#head(education)

qplot(x=education_level,y=prop,data=education,geom="bar",stat="identity",ylim=c(0,1),
      ylab="Proportion of over $50k/year",xlab="Highest level of Education",
      main="Proportion over $50/year by Education Level \n for 25 years old and older") + 
      theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))

# Save the final image
jpeg(file="C:\\Users\\Tyler\\Desktop\\RTI\\exercises\\educationPlot.jpeg",width=1000,height=700,quality=90)
qplot(x=education_level,y=prop,data=education,geom="bar",stat="identity",ylim=c(0,1),
      ylab="Proportion of over $50k/year",xlab="Highest level of Education",
      main="Proportion over $50/year by Education Level \n for 25 years old and older") + 
      theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))
dev.off()

# Notes:
# If Class=Never-worked, or in holland then 0.
# A ton of this code is repeated and could probably be put into a function
