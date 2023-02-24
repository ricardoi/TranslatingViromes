#'@title: Translatting Viromes
#'@author: R Alcala
#'@description:
#'

#-- Setting working directory
setwd("git_local/TranslattingViromes/")

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
x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus", "Maize dwarf mosaic virus",
                                   "Iranian johnsongrass mosaic virus", "Scallion mosaic virus",
                                   "Potato virus V", "Telosma mosaic virus",
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

#----- Custom Algorithm to create vectors
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus", "Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Scallion mosaic virus",
                                     "Potato virus V", "Telosma mosaic virus",
                                     "Lily mottle virus", "Sorghum mosaic virus" ), "aphid",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "beetles",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "aphid",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "grasshoppers",
                                        "NULL"))))
}
vectors <- data.frame(Species, "vector"=unlist(x))
virome1 = merge(virome1, vectors, by="Species")

#----- Custom Algorithm to create vectors
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus", "Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Scallion mosaic virus",
                                     "Potato virus V", "Telosma mosaic virus",
                                     "Lily mottle virus", "Sorghum mosaic virus" ), "mechanical",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "mechanical",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "vector-only",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "vector-only",
                                        "NULL"))))
}
transmission <- data.frame(Species, "transmission"=unlist(x))
virome1 = merge(virome1, transmission, by="Species")


#----- Custom Algorithm to create seed transmitted
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "seed",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "seed",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "seed",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "not seed",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "seedtr"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

#----- Custom Algorithm to create resistance
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "resistance",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "tolerance",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "unknown",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "unknown",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "resistance"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

#----- Custom Algorithm to create resistance
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "effective",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "effective",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "ineffective",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "ineffective",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "sanitizing"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

#----- Custom Algorithm to create antifeedant
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "effective",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "ineffective",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "ineffective",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "ineffective",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "antifeedant"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

#----- Custom Algorithm to create insecticide
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "ineffective",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "ineffective",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "ineffective",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "ineffective",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "insecticide"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

#----- Custom Algorithm to create management
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus" ), "yes",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "yes",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "yes",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "no",
                                        "unknown"))))
}
VMUs <- data.frame(Species, "management"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")

virome1 <- read.csv("data/MLN_Wamaitha18_and-management_data_generated.csv")

#----- Custom Algorithm to create VMUs
for (i in seq_along(Species)){
  x[[i]] <- ifelse(Species[i] %in% c("Sugarcane mosaic virus","Maize dwarf mosaic virus",
                                     "Iranian johnsongrass mosaic virus", "Sorghum mosaic virus", 
                                     "Scalion mosaic virus", "Potato virus Y", "Lily mottle virus",
                                     "Telosma mosaic virus"), "VMU1",
                   ifelse(Species[i] %in% c( "Maize chlorotic mottle virus"), "VMU4",
                          ifelse(Species[i] %in% c("Maize yellow dwarf virus", "Maize yellow mosaic virus", "Barley virus G",
                                                   "Tobacco vein distorting virus", "Maize yellow dwarf virus 2"), "VMU3",
                                 ifelse(Species[i] %in% c("Maize streak virus"), "VMU2",
                                        "unassigned"))))
}
VMUs <- data.frame(Species, "VMU"=unlist(x))
virome1 = merge(virome1, VMUs, by="Species")


write.csv(virome1, "data/MLN_Wamaitha18_and-management_data_generated.csv")

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
  mutate( #ss = paste(Genus, Family), # pasting two groups for a special category coloring
          cols = RbPal[ match(Genus, sort(unique(Genus))) ]
  )

head(virome1)

# original alluvial - minus 8 and 13
alluvial(virome1[,c(1,5,6,7,8,14)], freq=virome1$freq,
         #hide = virome1$length == 0,
         col = virome1$cols,
         border = virome1$cols,
         alpha = 0.9,
         blocks = FALSE,
         ordering = list(sort(virome1$Species), sort(virome1$Species), NULL, NULL, NULL, sort(virome1$VMU)),
         # change NULL to order them
         cex =0.8
)

# alluvial with management strategies
alluvial(virome1[,c(1,4,5,13,6,8,9,10,11,12,7)], freq=virome1$freq,
         #hide = virome1$length == 0,
         col = virome1$cols,
         border = virome1$cols,
         alpha = 0.9,
         blocks = FALSE,
         ordering = list(NULL, sort(virome1$Species), NULL, NULL, NULL, NULL,
                         NULL, NULL,NULL, NULL, sort(virome1$VMU)),
         # change NULL to order them
         cex =0.8
)



