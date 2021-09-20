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
