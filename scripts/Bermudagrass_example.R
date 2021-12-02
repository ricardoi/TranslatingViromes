#'@title: Translatting Viromes
#'@author: R Alcala
#'@description:
#'

#-- Setting working directory
setwd("git_db/TranslattingViromes/")

#-- loading libraries
library("tidyverse")

#-- loading data
bermudagrass_virome <- read.csv("data/Bermudagrass_virome_BlasXall_2016.csv",
                       header = T)
head(bermudagrass_virome)
dim(bermudagrass_virome)

boxplot(bermudagrass_virome$length)

cov=c(NULL)
for (i in 1:nrow(bermudagrass_virome)){
 cov[i] <- c(strsplit(bermudagrass_virome$Query, split = "_")[[i]][6])
}
bermudagrass_virome$Coverage <- as.numeric(cov)
#@ Collecting metadata
viromeb <- bermudagrass_virome %>%
  select("Exp", Species="Description", "Query", "Genus", "Family", "length", "Coverage", "E.Value",
         "Query.coverage", "X..Pairwise.Identity", "Bit.Score", "Sequence") %>%
  group_by(Species, Genus, Family, Exp) %>% 
  summarise(Cov = mean(Coverage), n=n()) # differences mean

viromeb

#---- Generating hist plots
#-- libraries
#library(ggplot2)
library(tidytext)
library(viridis)


ggplot(viromeb, aes(x=reorder(Exp, Species), y=(n), fill=Genus)) +
  geom_bar(position="fill", stat="identity", width = 1, alpha = 0.75)+
  ggtitle("Virome genus frequency of bermudagrass virome") +
  facet_grid(scales = "free", space = "free") +
  scale_fill_viridis(discrete = T, option = "C") +
  scale_x_reordered() +
  theme_bw()


# trata de crear un plot de presencia ausencia de virus por grupo experimental 
# y que se vea los virus abundantes vs los no abundantes. 


