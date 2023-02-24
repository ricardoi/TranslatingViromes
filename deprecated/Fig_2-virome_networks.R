library(igraph)

# EXAMPLE TRADITIONAL DIAGNOSTICS
a<-matrix(ncol = 4,
  c(1,0,1,0,0,0,
  0,1,0,0,0,1,
  0,1,1,0,0,0,
  0,0,1,1,1,0)
)

rownames(a) <- c("s1", "s2", "s3", "s4", "s5", "s6")
colnames(a) <- c("SPFMV", "SPCVS", "SPPV", "SPMMV")
a

g <- graph.incidence(t(a), directed = F)
V(g)$type
shapes = c(rep("circle", length(V(g)$type[V(g)$type == "FALSE"])),
           rep("square", length(V(g)$type[V(g)$type == "TRUE"])))
plot(g, edge.width=1, vertex.size=20, vertex.shape=shapes, vertex.label.color='#5E8E3F',
     vertex.label.cex=1, layout = layout_as_bipartite)


g2 <- bipartite.projection(g2)

# Samples
adj1 <- get.adjacency(g2$proj1,sparse=FALSE,attr="weight")
plot(g2$proj1,edge.width=E(g2$proj1)$weight^2, vertex.label=V(g2$proj1)$name)

# Virus
adj2 <- get.adjacency(g2$proj2,sparse=FALSE,attr="weight")
plot(g2$proj2,edge.width=E(g2$proj2)$weight^2, vertex.label=V(g2$proj2)$name)


 adj1 <- get.adjacency(g2$proj1, sparse=FALSE, attr="weight")


 # EXAMPLE VIROME DIAGNOSTICS

 b<-matrix(ncol = 5,
           c(1,1,1,0,0,0,
             0,1,0,0,0,1,
             0,1,1,0,0,0,
             0,0,0,1,1,1,
             0,0,1,1,0,1)
 )

 rownames(b) <- c("s1", "s2", "s3", "s4", "s5", "s6")
 colnames(b) <- c("SPFMV", "SPCVS", "SPPV", "SPMMV", "NV")
 b

 g2 <- graph.incidence(t(b), directed = F)
 V(g2)$type
 shapes = c(rep("circle", length(V(g2)$type[V(g2)$type == "FALSE"])),
            rep("square", length(V(g2)$type[V(g2)$type == "TRUE"])))
 plot(g2, edge.width=1, vertex.size=20, vertex.shape=shapes, vertex.label.color='#5E8E3F',
      vertex.label.cex=1, layout = layout_as_bipartite)


 g4 <- bipartite.projection(g2)

 # Virus
 adj1 <- get.adjacency(g4$proj1,sparse=FALSE,attr="weight")
 plot(g4$proj1,edge.width=E(g4$proj1)$weight^2, vertex.label=V(g4$proj1)$name)

 # Samples
 adj2 <- get.adjacency(g4$proj2,sparse=FALSE,attr="weight")
 plot(g4$proj2,edge.width=E(g4$proj2)$weight^2, vertex.label=V(g4$proj2)$name)


 adj1 <- get.adjacency(g2$proj1, sparse=FALSE, attr="weight")
