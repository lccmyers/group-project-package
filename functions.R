library(factoextra)
library(dplyr)
library(ggplot2)

d <- read_csv("data.csv", col_names = TRUE)

pca.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  return(summary(pcafam))
}

pca.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  return(summary(pcasup))
}

graph.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  fviz_pca_ind(pcafam, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dfam$Genus, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Genus") + labs(title = "PCA", x = "PC1", y = "PC2")
}

graph.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  fviz_pca_ind(pcasup, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dsup$Family, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Family") + labs(title = "PCA", x = "PC1", y = "PC2")
}

#testing functions
pca.family(d, "Cercopithecidae")

pca.superfamily(d, "Ceboidea")

graph.family(d, "Cercopithecidae")

graph.superfamily(d, "Ceboidea") + labs(title="PCA1")


#notes: ggplot functions can be added onto the graph functions using +
#i.e., if i wanted to change the title id do graph.family()+labs(title=)
#if there are too few individuals in a group, the function will not draw
#an ellipse around the points - cutoff is 2 (i think)
#functons require data to be named in specific way - needs columns with family,
#superfamily and genus, need to be labelled with the first letter capitalized
