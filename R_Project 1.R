install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("lmtest", repos = "http://cran.us.r-project.org")
install.packages("readxl")
install.packages("plyr", repos = "http://cran.us.r-project.org")
install.packages("tidyverse")
library(readxl)
library(dplyr)
library(psych)
library(SnowballC)
library(tidyverse)
library(tm)
library(wordcloud)
library(ggplot2)
library(caTools)
library(rgl)
library(DMwR)
traindata <- read_excel("Sem-6/traindata.xlsx")
View(traindata)
is.na(traindata)

#PREPROCESSING OF DATA    
#DEALING WITH THE NULL VALUES
traindata$`Average Delay Hours`<- ifelse(is.na(traindata$`Average Delay Hours`), 
                                         ave(traindata$`Average Delay Hours`, FUN = function(x) 
                                           as.integer( median(x, na.rm = TRUE))), 
                                         traindata$`Average Delay Hours`)

traindata$Stopage<- ifelse(is.na(traindata$Stopage), 
                                         ave(traindata$Stopage, FUN = function(x) 
                                           as.integer( median(x, na.rm = TRUE))), 
                                         traindata$Stopage)

glimpse(traindata)
structure(traindata)#WE CAN SAY STRUCTURE IS A VERTICAL VIEW OF GLIMPSE
describe(traindata)
?set.seed()
set.seed(344)
split= sample.split(traindata$`Average Delay Hours`,SplitRatio = 0.8)
split

Training_set= subset(traindata,split ==TRUE)
Test_set = subset(traindata,split ==FALSE)

#feature scaling
Training_set[,7:8] = scale(Training_set[,7:8])
Test_set[,7:8] = scale(Test_set[,7:8])

#TEXT PREPROCESSING 

#use of distinct
a<-traindata%>% distinct(Destination)
a
a<-traindata%>% distinct(Source)
a
#DEALING WITH NULL VALUE
ifelse(is.na(traindata$Source))  
b<-traindata%>%mutate(Source = replace(Source, is.na(Source), "UNAVAILABLE"))
traindata$Source
View(b)

#filtering out data
y<-b%>% filter(`Average Delay Hours`=="0")%>%
  select(`Train Name`)
y



#VISUALISATION 
ggplot(data = b)
#defining aesthetics
z<-ggplot(data = b, aes(x=`Average Delay Hours`, y= Stopage))
#adding geompoint
z+geom_point()
#geom_boxplot
z+geom_boxplot()

#adding points
z+geom_boxplot()+geom_jitter(alpha=0.3,colour="blue")


#adding another layer
z+geom_point(alpha=0.3,colour="red")+ geom_abline(intercept = 0)

#zooming into certain ranges
z+geom_point(alpha=0.3,colour="red")+ geom_abline(intercept = 0)+
  scale_x_continuous(limits = c(0,0.1))+
  scale_y_continuous(limits = c(0,0.1))

#using geom_col
z1<-ggplot(data = b,aes(x=`Train Name`,y=`Average Delay Hours`))+geom_col()
z1
#if i want to flip the x and y axis
z1+coord_flip()

#mapping values to different colours
ggplot(data=b,aes(`Average Delay Hours`)) + geom_bar(aes(fill=Stopage)) + coord_flip()
#filtering the trains having maximum delay hours
R<-max(b$`Average Delay Hours`)
z1<-ggplot(data = b,aes(x=b$`Train Name`,y=R))+geom_col()
z1
z1+coord_flip()



#dev.off()
#par("mar")
#par(mar=c(1,1,1,1))
#LINEAR REGRESSION  
scatter.smooth(x=b$`Average Delay Hours`, y=b$Stopage, main="delay ~ Stopage")

#build linear model
linearmod<- lm(`Average Delay Hours` ~ Stopage, data = b)
print(linearmod)
summary(linearmod)
modelsummary<- summary(linearmod)
modelcoeffs<- modelsummary$coefficients
modelcoeffs
#std.error<- modelcoeffs["Average Delay","std.error"]
#beta.estimate<- modelcoeffs["stopage","Estimate"] #Error

#Step 1: Create the training (development)
#and test (validation) data samples from original data.
set.seed(100)
#row indices for training data
trainingrow<- sample(1:nrow(b),0.8*nrow(b))
trainingrow
#model training data
trainingdata<- b[trainingrow, ]
#test data
testdata<-b[-trainingrow, ]

#Step 2: Develop the model on the training data
#and use it to predict the delay on test data
# Build the model on training data 
lmMod<- lm(`Average Delay Hours` ~ Stopage, data = trainingdata)
#predict delay 
delayPred<-predict(lmMod, testdata)
delayPred

#Step 3: Review diagnostic measures.
summary(lmMod)#modelsummary
# Calculate akaike information criterion
AIC (lmMod,k=2)
BIC(lmMod)

?AIC
?BIC
#Step 4: Calculate prediction accuracy and error rates
actual_pred<- data.frame(cbind(actuals=testdata$`Average Delay Hours`, predicteds=delayPred))
correlation_accuracy<- cor(actual_pred)
correlation_accuracy
head(actual_pred)
#Now lets calculate the Min Max accuracy and MAPE:
#  MinMaxAccuracy=mean(min(actuals,predicteds)max(actuals,predicteds))



#MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds???actuals)actuals)
min_max_accuracy<- mean(apply(actual_pred, 1,min)/ apply(actual_pred,1, max))
min_max_accuracy

mape<- mean(abs((actual_pred$predicteds - actual_pred$actuals))/actual_pred$actuals)
mape




#detecting outliers
#LOF (Local Outlier Factor): Proximity (density) Based Outlier Detection Technique
#The LOF method is based on scoring outliers on the basis of the density in the neighborhood. 
#This technique is based on a parameter known as outlier score.
b1<-b[ ,7:8]
head(b1)
outlier.scores<- lofactor(b1, k=6)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]

print(outliers)

#You should now be able to identify the outliers.

#The five outliers obtained in the output are the row numbers in the b1 data derived from the b data set.

#To visualize the outliers with a biplot of the first two principal components use the following commands provided below:

n<- nrow(b1)
labels<- 1:n
labels[-outliers]<-"."
biplot(prcomp(b1),cex=.8,xlabs=labels)


#Now we can use the pairs plot to visualize the outliers which are marked with a "+" sign in red by typing in the following commands:
  
  pch <- rep(".", n)

pch[outliers] <- "+"

col <- rep("black", n)


col[outliers] <- "red"

pairs(b1, pch=pch, col=col)

plot3d(b1$`Average Delay Hours`,b1$Stopage, type="s", col=col) 


