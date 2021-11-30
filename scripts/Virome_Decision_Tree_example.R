library(tidyverse)
library(dplyr)

# SET YOUR WORKING DIRECTORY TO 
# setwd("TranslatingViromes")
virome1 <- read.csv("data/MLN_Wamaitha18_and-management_data_generated.csv", as.is = T, 
                   stringsAsFactors = T)
virome = virome1[c(8,5,6,7,9,10,11,12,13)]

virome <- 
  virome |>
  mutate(across(where(is.character), as.factor))
str(virome)

# Subsetting data for training and validation
train<-virome[1:180,]
validate<-virome[181:244,]

#install.packages("party")
#install.packages("partykit")
library(party)
library(partykit)

# Objective 
# Use of decision tree to recreate the Virome Management Units (VMU)
# Broadly, we have the taxonomic classification, and we have information about
# management strategies for each taxonomic group. 
# The goal would be to (re)create the VMU based on those shared managemets.

tree <- ctree(VMU+Genus+transmission+vector+seedtr+resistance+sanitizing+antifeedant+insecticide, data=train)
plot(tree,type="simple")
predict(tree,validate,type="prob")
predict(tree,validate)

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tr<-rpart(VMU~Genus+transmission+vector+seedtr+resistance+sanitizing+antifeedant+insecticide, data=train)
rpart.plot(tr)

#-------------
# Data generation by B. Etherton
virome<-matrix(0,nrow=1000,ncol=4)
colnames(virome)<-c("genus","vector","transmission","VMU")

#true data options
option1<-c("Potyvirus","aphids","mechanical","VMU1")
option2<-c("Polerovirus","aphids","vector-only","VMU1")
option3<-c("Mastrevirus","grasshopper","vector-only","VMU2")
option4<-c("Machlomovirus","beetles","mechanical","VMU1")
option5<-c("Polerovirus","aphids","mechanical","VMU1")#less likely
option6<-c("Potyvirus","aphids","vector-only","VMU1")#less likely 

#noise / false data options
option7<-c("Mastrevirus","aphids","vector-only","VMU2")
option8<-c("Mastrevirus","grasshopper","vector-only","VMU1")
option9<-c("Potyvirus","aphids","mechanical","VMU2")
option10<-c("Polerovirus","aphids","vector-only","VMU2")

#prob of true data is 15%
#prob of less likley true data is 10%
#prob of noise is 5%
for(j in 1:1000){
  i<-sample(1:10,1,replace=TRUE,prob=c(0.15,0.15,0.15,0.15,0.1,0.1,0.05,0.05,0.05,0.05))
  if(i==1){
    virome[j,]<-option1
  }
  if(i==2){
    virome[j,]<-option2
  }
  if(i==3){
    virome[j,]<-option3
  }
  if(i==4){
    virome[j,]<-option4
  }
  if(i==5){
    virome[j,]<-option5
  }
  if(i==6){
    virome[j,]<-option6
  }
  if(i==7){
    virome[j,]<-option7
  }
  if(i==8){
    virome[j,]<-option8
  }
  if(i==9){
    virome[j,]<-option9
  }
  if(i==10){
    virome[j,]<-option10
  }
}

virome<-as.data.frame(virome)

#making sure these are factors and not class char
virome$genus<-as.factor(virome$genus) 
virome$VMU<-as.factor(virome$VMU)
virome$vector<-as.factor(virome$vector)
virome$transmission<-as.factor(virome$transmission)
