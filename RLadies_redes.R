#######################
#####
#####           R-Ladies Santiago
#####           Taller Uso de R
#####           
#####           Carmen Le Foulon
#####           19/10/18
#####           
############################
getwd()

rm(list=ls())

library(igraph)
library(tidyverse)
#
#### GENERAR NETWORK
#### 


## Dos opciones
## 
## 

## 1) edge list

load("netsponsor_2006_1mes.Rda")
load("vertex_2006_1mes.Rda")

## verificar que la info de los nodos sea la misma que en net

vertex_2006_1mes %>% dplyr::filter(id_nuevo %in% unique(c(netsponsor_2006_1mes$sponsor01,netsponsor_2006_1mes$sponsor02))) -> vertex_2006_1mes

e_net <- graph_from_data_frame(netsponsor_2006_1mes, directed=FALSE, vertices=vertex_2006_1mes)
E(e_net)$weight <- 1
is.simple(e_net)
net<-simplify(e_net)
is.simple(net)

##  2) Adjacency matrix: simetrica y diagonal 0

base_moc<-readRDS("base_moc.rds")
adj<-t(base_moc)%*%base_moc
diag(adj) <- 0

net2<-graph_from_adjacency_matrix(adj, mode = "undirected", weighted = T, diag = TRUE,
                            add.colnames = T)

is.simple(net2)

#### 
#### 


############### GRAFICAR 
#####

plot(net)

#no muy lindo!
#
set.seed(1)
l <- layout_with_fr(net)
plot(net, vertex.label="", layout=l)
plot(net, vertex.label="", vertex.size=10, edge.lty=1, layout=l)
plot(net, vertex.label="", vertex.size=10, edge.lty=0, layout=l)


## lo fijamos para la sesión
igraph.options(vertex.label="", vertex.size=10, edge.lty=1)


## distintos tipos de gráficos

# Fruchterman, T. M. J., & Reingold, E. M. (1991). Graph Drawing by Force-Directed Placement. Software: Practice and Experience, 21(11).


lays<-c( "layout_randomly", "layout_with_kk" , 
         "layout_with_fr","layout_with_mds")

par(mfrow=c(2,2), mar=c(1,1,1,1))
for (layout in lays) {
  print(layout)
  l <- do.call(layout, list(net)) 
  set.seed(1)
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

dev.off()

## ojo: es algortimo aleatorio: 

seeds<-c(1, 25, 100, 1000)
noms<-paste("seed=", seeds, sep="")
par(mfrow=c(2,2), mar=c(1,1,1,1))
for (i in seeds) {
  print(i)
  set.seed(i)
  nom<-paste("seed=", i, sep="")
  plot(net, edge.arrow.mode=0, layout=layout_with_fr, main=nom) }

dev.off()
plot(net, edge.arrow.mode=0, layout=layout_with_fr) 
### Nos quedamos con Fruchterman-Reingold
### Un par de opciones adicionales

###  colores
set.seed(1)
l <- layout_with_fr(net)

plot(net, vertex.label="", layout=l)

plot(net, edge.color="orange",
     vertex.color="steelblue",
     main="Primer mes", layout=l)

## colores que digan algo!, por ejemplo: coalicion

pacto<-get.vertex.attribute(net, "pact")
net
table(pacto)

coal_colors <- as.character(length(pacto))
coal_colors [pacto == "Alianza"] <- "royalblue4"
coal_colors[pacto == "Concertacion/NM"] <- "darkred"
coal_colors [pacto == "Otro"] <- "yellow2"


plot(net, 
     vertex.color=coal_colors,
     edge.color="gray",
     main="Primer mes",layout=l)


###  DEFINIR SHAPE POR COALICION
###  

# En shapes no hay triangles: 

source("triangle_shape.R")
coal_shape  <-  as.character(length(pacto))
coal_shape [pacto == "Alianza"] <- "triangle"
coal_shape [pacto == "Concertacion/NM"] <- "circle"
coal_shape [pacto == "Otro"] <- "square"

plot( net, vertex.shape=coal_shape, 
      vertex.color=coal_colors,
      mark.border=c("black"),
      main="Primer mes", layout=l)



## DEFINIR TAMAÑOS
coal_size <-  as.integer(length(pacto))
coal_size [pacto == "Alianza"] <- 13
coal_size [pacto == "Concertacion/NM"] <- 11
coal_size [pacto == "Otro"] <- 8

plot( net, vertex.shape=coal_shape, 
      vertex.color=coal_colors, vertex.size=coal_size,
      mark.border=c("black"),
      main="Primer mes", layout=l)



### NETWORK STATISTICS
### 

# DENSITY
# 
graph.density(net)


# CLUSTERING
transitivity(net, type ="undirected") # o global

transitivity(net, type ="global")

transitivity(net, type ="average")

transitivity(net, type ="local")


mean(transitivity(net, type ="local"), na.rm = T)

hist(transitivity(net, type ="local"))


# K-CONNECTIVITY
# 

cohesion(net)

is.connected(net)


#  DISTANCE

diameter(net)

average.path.length(net)

# CLIQUES
# 

clique.number(net)




### DESCRIPTORES NODOS
### 

hist(degree(net), col="steelblue", xlim=c(0,50),
     xlab="Nodes degree", ylab="Frequency", main="Histograma degree")


### una medida es closeness: cl(v)=1/sum(dist)
### 
V(net)$close<-closeness(net)

## qué pasó?
##  grafico no conectado! 

## generar subgrafico: hasta 3 vecinos desde el primer nodo
net_1 <- induced.subgraph(net,
                             neighborhood(net, 2, V(net)[2:1])[[1]])


plot(net_1)
V(net_1)$close<-closeness(net_1)

###betweennes: vertice ubicado "entre" los otros vertices: shortest paths the pasan por v

###
V(net)$btw <- betweenness(net, directed = FALSE)

V(net_1)$btw<-betweenness(net_1, directed = FALSE)




### homofilia:

#Para modularidad por pacto
pacto_num <- as.numeric(factor(V(net)$pact)) 

#unweighted
#
modularity(net,pacto_num)

#weighted
#
modularity(net,pacto_num, weights=E(net)$weight)


### COMUNIDADES
### MULTIPLES ALGORITMOS!
### 

net
set.seed(19)


comm<- cluster_walktrap(net)
comm_2 <- cluster_leading_eigen(net)

data.frame(tipo=c("walktrap", "leading_eigen"), 
           mod=c(modularity(comm), modularity(comm_2)),
          n_com=c(length(comm), length(comm_2)))


### GRAFICAR COMUNIDADES
###  DEFINIR COLORES POR COMUNIDAD


## para graficos:
## comunidades: difieren por color
## coaliciones: difieron por color y forma
## 
dev.off()



plot( comm, net, vertex.shape=coal_shape, 
      col=coal_colors,vertex.size=coal_size ,
      mark.border=c("black"),
      main="Primer mes", layout=l)


comm
plot( comm, net, vertex.shape=coal_shape, 
      col=coal_colors , vertex.size=coal_size ,
      mark.border=c("blue"),edge.lty=0,
      main="Primer mes", layout=l)

deg <- degree(net)

### incorpora el tamaño como atributo, no hay que especificarlo
V(net)$size <- deg


plot( comm, net, vertex.shape=coal_shape, 
      col=coal_colors ,
      mark.border=c("blue"),edge.lty=0,
      main="Primer mes", layout=l)



legend("bottomright", 
       legend = c("Concertacion", "Alianza", "Indep"), 
       col = c("darkred", "royalblue4", "yellow2" ),
       pch = c(19, 17, 15), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(.8, 0))  



######