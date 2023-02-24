library(rgeoboundaries)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(igraph)
library(rworldmap)
library(sf)
library(tmap)
library(raster)

worldBoundaries <- map_data("world")
worldBoundaries <- worldBoundaries %>%
  filter(lat > -60)

centroids2<-read.csv("countriesNodes v2.csv")
centroids2[centroids2 == c("Bolivia (Plurinational State of)")] <- c("Bolivia")
centroids2[centroids2 == c("China, mainland")] <- c("China")
centroids2[centroids2 == c("China, Hong Kong SAR")] <- c("Hong Kong")
centroids2[centroids2 == c("Republic of Korea")] <- c("South Korea")
centroids2[centroids2 == c("Lao People's Democratic Republic")] <- c("Laos")
centroids2[centroids2 == c("China, Taiwan Province of")] <- c("Taiwan")
centroids2[centroids2 == c("United Republic of Tanzania")] <- c("Tanzania")
centroids2[centroids2 == c("United Kingdom of Great Britain and Northern Ireland")] <- c("United Kingdom")
centroids2[centroids2 == c("United States of America")] <- c("United States")
centroids2[centroids2 == c("Venezuela (Bolivarian Republic of)")] <- c("Venezuela")

centroids3<-read.csv("country_centroids_az8 modified v3.csv")

CIPgermplasm<-read.csv("CIP-Network.csv")
CIPgermplasm <- CIPgermplasm %>%
  filter(Germplasm_activity=="Outcoming")

CIPnode1<-as.matrix(unique(CIPgermplasm$Receiving_node))
CIPnode2<-as.matrix(unique(CIPgermplasm$Releasing_node))
CIPnodes <-as.data.frame(unique(rbind(CIPnode1, CIPnode2)))
colnames(CIPnodes)[1]<-"country"

CIPnode_attribute<-merge(CIPnodes, centroids2, by="country", all.x = TRUE)
CIPnode_attribute <- as_tibble(CIPnode_attribute)%>%
  dplyr::select(country, lon, lat)

CIPnodelist <- as.data.frame(CIPnode_attribute)

CIPedgelist <- as.data.frame(CIPgermplasm)

CIPgeneNet <- graph_from_data_frame(d = CIPedgelist,
                                    vertices = CIPnodelist,
                                    directed = TRUE)

map_background <- raster("map_grey_background.tif")

E(CIPgeneNet)$weight <-CIPedgelist$Link_weight

plot(map_background, col = "#FAC127FF",xaxt='n',  yaxt='n', axes=F, box=F,   legend = F, main=paste('CIP genebank network'), cex.main=1.9)
plot(countriesLow, add=TRUE, border = "white")
plot(CIPgeneNet, vertex.size = 200, 
     layout = cbind(CIPnode_attribute$lon, CIPnode_attribute$lat), 
     add = TRUE, rescale = FALSE,  
     edge.arrow.size = 0.5, edge.curved = TRUE, 
     edge.col ='black', edge.color =  '#800000',
     edge.width=(E(CIPgeneNet)$weight)^(1/3)/3.5,
     vertex.label = NA,
     vertex.color = NA, vertex.frame.color='#990000')