
# SET YOUR WORKING DIRECTORY TO 
# setwd("TranslatingViromes")

library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(dplyr)

management_data_generated <- read_csv("MLN_Wamaitha18_and-management_data_generated.csv")

set1<-management_data_generated[,c(2,5:13)] #cleaning up data set
set1<-as.matrix(set1)

noise_size<-100 #how much error is there? modulate this number to create different decision trees
#the test set below is 200 data points, so the noise will determine what percent of the data is flawed
#where noise_size=100 means that 1/3 of the data is flawed, noise_size=50 means that 1/5 of the data is flawed, etc....

noise<-matrix(0,ncol=10,nrow=noise_size) #creating errors
colnames(noise)<-colnames(set1)
for(i in 1:10){
  s<-sample(1:244,noise_size,replace=FALSE)
  noise[,i]<-set1[s,i]
} #randomizing errors

set<-sample(1:244,244,replace=FALSE) #randomly sampling from true data set
train<-set1[set[1:200],] 
train<-rbind(noise,train) #combining true data with noise
#validate<-set1[set[201:244],] #using the rest of the true data to validate the model

train<-as.data.frame(train,stringsAsFactors = TRUE)

#creating the decision trees
tree<-ctree(VMU~vector+Genus+transmission+seedtr+resistance+sanitizing+antifeedant+insecticide,data=train)
plot(tree,type="simple")
