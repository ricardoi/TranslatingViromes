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

# meta <- read.csv("data/Virome_MLN_Kenya_hosts.txt")
# meta

# Manipulating data to construct a data frame
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
colnames(vsam) <- c("IDs", "Species", "freq")
virome1=vsam
#- End manipulating data

#--- Algorithm to classify viruses using Species
x=list()
Species <- unique(virome1$Species)
for (i in seq_along(Species)){
x[[i]] <- ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Sugarcane mosaic virus",
                          "Maize dwarf mosic virus", "Iranian johnsongrass mosaic virus",
                          "Scallion mosaic virus", "Potato virus V", "Telosma mosaic virus",
                          "Lily mottle virus", "Sorghum mosaic virus" ), "Potyvirus",
                ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "Machlomovirus",
                       ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "Polerovirus",
                              ifelse(Species[i] %in% c("Maize streak virus"), "Mastrevirus",
                              "no-poty-ni-maclo-ni-polero-ni-mastre"))))
}
#- formatting data frame
taxa<- data.frame(Species, "Genus"=unlist(x))
virome1$freq <- rep(1, nrow(virome1))
virome1 = merge(virome1, taxa, by="Species")

#---- Generating hist plots
#-- libraries
library(ggplot2)
library(tidytext)
library(viridis)


ggplot(virome1, aes(x=reorder(IDs, Genus), y=(freq), fill=Genus)) +
  geom_bar(position="fill", stat="identity", width = 1, alpha = 0.75)+
  ggtitle("Virome genus frequency of MLN virome in Kenya") +
  facet_grid(scales = "free", space = "free") +
  scale_fill_viridis(discrete = T, option = "C") +
  scale_x_reordered() +
  theme_bw()

#---- Generating alluvial plots
#-- libraries
library(alluvial)
library(tidyverse)
# library(reshape2)

RbPal <- viridis(c(length(unique(virome1$Genus))))

virome1 <- virome1 %>%
  mutate( ss = paste(Genus), # pasting two groups for a special category coloring
          cols = RbPal[ match(ss, sort(unique(ss))) ]
  )

alluvial(virome1[,c(4,1)], freq=virome1$freq,
         #hide = datc$length == 0,
         col = virome1$cols,
         border = virome1$cols,
         alpha = 0.9,
         blocks = FALSE,
         ordering = list(NULL, order(as.factor(virome1$freq))),
         # change NULL to order them
         cex =0.8
)
