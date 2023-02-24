
library(readr)
library(party)
library(partykit)
library(dplyr)
library(viridis)

#Load in data and clean it up:
management_data_generated <- read_csv("MLN_Wamaitha18_and-management_data_generated.csv")
set1<-as.matrix(management_data_generated[,c(2,5:12)])
colnames(set1)<-c("Species","Genus","Vector","Transmission","Seed Transmission","Resistance","Sanitizing","Antifeedant","Insecticide")

#lets create a sample dataset & clean it up
set<-sample(1:244,244,replace=FALSE) #randomly sampling from true data set
train<-set1[set[1:244],] 
train<-as.data.frame(train,stringsAsFactors=TRUE)
train$Antifeedant[which(train$Antifeedant=="innefective")]<-"ineffective"
train$Species[which(train$Species=="Maize yellow dwarf virus 2")]<-"Maize yellow dwarf virus"

#lets assign colors given the viral genus
col_pal<-viridis(length(unique(train$Genus)),option="D")
for(i in 1:dim(train)[1]){
  if(train$Genus[i]=="Polerovirus"){
    train$Color[i]<-col_pal[3]
  }
  if(train$Genus[i]=="Potyvirus"){
    train$Color[i]<-col_pal[4]
  }
  if(train$Genus[i]=="Machlomovirus"){
    train$Color[i]<-col_pal[2]
  }
  if(train$Genus[i]=="Mastrevirus"){
    train$Color[i]<-col_pal[1]
  }
}

#Lets plot the tree
tree<-ctree(Species~Resistance+Sanitizing+Antifeedant+Insecticide+`Seed Transmission`,data=train)
plot(tree,las=2,
     tp_args = list(id = TRUE, fill=train$Color))

