#'@title: Translatting Viromes
#'@author: R Alcala
#'@description: 
#'

#-- Setting working directory
setwd("git_db/TranslattingViromes/")

#-- loading data
mln_virome <- read.csv("data/Virome_MLN_Kenya_species_Wamaitha18.txt", sep = "\t"
                       , header = F)
mln_virome
dim(mln_virome)

meta <- read.csv("data/Virome_MLN_Kenya_hosts.txt")
meta

vsam = vls = 0
for (i in 1:nrow(mln_virome)){
  obj <- paste(mln_virome$V1[i], mln_virome[i,], sep="-")
  vls <- c(unlist(obj), vls)
  }
vls

vdf <- strsplit(vls, split = "-")

for(i in seq_along(vls)){
  vres  <- as.data.frame(t(as.data.frame(vdf[i])))
  if (ncol(vres)  == 2 ){
  vsam <-  rbind(vres, vsam)
  }
}

vsam$rmcol <- as.numeric(vsam$V1) * as.numeric(vsam$V2)
vsam <- vsam[is.na(vsam$rmcol),]
rownames(vsam) <- 1:nrow(vsam)
vsam
