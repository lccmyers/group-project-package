library(tidyverse)
library(factoextra)

#reading in data
d <- read_csv("data.csv", col_names = TRUE)
View(d)
d<-na.omit(d)

##PCAs

#for individual families - graph will separate by genuses
#function needs to specify which family, and the subset of data

dfam<-filter(d, Family=="Cercopithecidae")

pcafam<-prcomp(dfam[c(5:17)], center=T, scale=T)
summary(pcafam)
pcafam$rotation

#family function
pca.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  return(summary(pcafam))
}

#it works :)
pca.family(data=d, family.name="Cercopithecidae")



#for individual superfamilies - graph will separate by family
#function needs to specify which genus, and the subset of data
#note - this will not work if values within the sample are the same
dsup<-filter(d, Superfamily =="Ceboidea")

pcasup<-prcomp(dsup[c(5:17)], center=T, scale=T)
summary(pcasup)


#superfamily function
pca.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  return(summary(pcasup))
}


#it also works :)
pca.superfamily(data=d, superfamily.name="Ceboidea")


##Graphs

#for individual families, separated by genus
#if there are too few individuals it will not create an ellipse
fviz_pca_ind(pcafam, geom.ind = "point", pointshape = 21, 
      pointsize=2, fill.ind = dfam$Genus, invisible="quali", palette="pal8",
      addEllipses=TRUE)+
  labs(title ="PCA", x = "PC1", y = "PC2")



#function
graph.family<-function(){
  fviz_pca_ind(pcafam, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dfam$Genus, invisible = "quali", palette = "pal8",
  addEllipses = T) + labs(title = "PCA", x = "PC1", y = "PC2")
}

#testing
graph.family()


#for individual superfamilies, separated by family
#if there are too few individuals it will not create an ellipse
fviz_pca_ind(pcasup, geom.ind = "point", pointshape = 21, 
             pointsize=2, fill.ind = dsup$Family, invisible="quali", palette="pal8",
             addEllipses=TRUE)+
  labs(title ="PCA", x = "PC1", y = "PC2")

#function
graph.superfamily<-function(){
  fviz_pca_ind(pcasup, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dsup$Family, invisible = "quali", palette = "pal8",
  addEllipses = T) + labs(title = "PCA", x = "PC1", y = "PC2")
}

#testing
graph.superfamily()
