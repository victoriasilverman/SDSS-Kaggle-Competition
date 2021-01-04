## Victoria Silverman, 101039968
## STAT4601 Final Project

##set up directory 
dir='/Users/Victoria/Documents/Victoria/4th year/STAT4601'
data="Skyserver_SQL2_27_2018 6_51_39 PM.csv"
google=paste(dir,data,sep="/")

##import libraries needed
library(RColorBrewer)
library(mda)
library(factoextra)
library(nnet)
library(ggfortify)
library(FactoMineR)
library(corrplot)
library(ggplot2)
library(GGally)
library(tidyverse)
library(data.table)
library(mltools)
library(gridExtra)
library(caret)
library(ape)
library(colorRamps)
#read in data
playstore<-read_csv(google, col_names=TRUE)

summary(playstore)

#visualize data

playstore$class<-as.factor(playstore$class)
classes<-playstore$class
ggpairs(playstore, columns = 4:8,  ggplot2::aes(colour=as.factor(classes)), upper = list(continuous = wrap("cor", size = 2)))

ggpairs(playstore, columns = 2:3,  ggplot2::aes(colour=as.factor(classes)), upper = list(continuous = wrap("cor", size = 2)))

ggpairs(playstore[,c(11,12,13,15,16,17,18)],  ggplot2::aes(colour=as.factor(classes)), upper = list(continuous = wrap("cor", size = 2)))

##plot of location
ggplot() +
  geom_point(data=playstore, aes(x=ra, y=dec, colour=classes), size=1) +
  scale_colour_hue(l = 70, c = 150, h = c(180, 300)) +
  coord_map("mollweide",xlim=c(0,360),ylim=c(-90,90)) +
  scale_y_continuous(breaks = seq(-90, 90, 30)) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(panel.grid.major = element_line(colour="darkgrey")) + 
  ggtitle("location of Stars, Galaxies and Quasars from Sloan Digital Sky Server") + 
  theme(plot.title = element_text(size =10))

##correlation and correlation plot
playstore.cor<-cor(playstore[,-c(1,10,14)])
corrplot(playstore.cor,type="upper", is.corr=FALSE, tl.col="black", na.label=" ")

#set up set that works
playstore_reduc<-playstore[,-c(1,10,14)] 

#dimension reduction
#run PCA
playstore_prc<-prcomp(playstore_reduc,scale=TRUE)
#summary of PCA
summary(playstore_prc) #we notice there are 18 principal components 

#look at scree plot of pcs
variance <- (playstore_prc$sdev)^2
loadings <- playstore_prc$rotation
rownames(loadings) <- colnames(playstore_reduc)
colName<-colnames(playstore_reduc)
scores <- playstore_prc$x 
varPercent <- variance/sum(variance) * 100 # a vector with the percentage of variation captured in each of the 10
PC<-varPercent[1:15]

#scree plot of 18 pc's
barplot(PC, xlab='PC', ylab='Percent Variance',
        names.arg=1:15, las=1, col=brewer.pal(n=5, name="BuPu"), main = "Scree Plot for 15 PC's") 

colourCount = 10
getPalette = colorRampPalette(brewer.pal(5, "BuPu"))

p1<-fviz_contrib(playstore_prc, choice = "var", axes = 1, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

# Contributions of variables to PC2
p2<-fviz_contrib(playstore_prc, choice = "var", axes = 2, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

# Contributions of variables to PC3
p3<-fviz_contrib(playstore_prc, choice = "var", axes = 3, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

playstore_reduc<-playstore[,-c(1,10,13,14,16,17,18)] 

#run PCA
playstore_prc<-prcomp(playstore_reduc,scale=TRUE)
#summary of PCA
summary(playstore_prc) #we notice there are 18 principal components 

#look at scree plot of pcs
variance <- (playstore_prc$sdev)^2
loadings <- playstore_prc$rotation
rownames(loadings) <- colnames(playstore_reduc)
colName<-colnames(playstore_reduc)
scores <- playstore_prc$x 
varPercent <- variance/sum(variance) * 100 # a vector with the percentage of variation captured in each of the 10
PC<-varPercent[1:11]

#scree plot of 14 pc's
barplot(PC, xlab='PC', ylab='Percent Variance',
        names.arg=1:11, las=1, col=brewer.pal(n=9, name="BuPu"), main = "Scree Plot for 11 PC's") 

p4<-fviz_contrib(playstore_prc, choice = "var", axes = 1, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

# Contributions of variables to PC2
p5<-fviz_contrib(playstore_prc, choice = "var", axes = 2, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

# Contributions of variables to PC3
p6<-fviz_contrib(playstore_prc, choice = "var", axes = 3, top = 10, fill=getPalette(colourCount), sortcontrib = "desc")

#print the two sets of graphs 
print("including id variables")
grid.arrange(p1,p2,p3,nrow=2)

print("not including id variables")
grid.arrange(p4,p5,p6,nrow=2)

#vis - plot the first 3 principal components in 3d
source("//Users//Victoria//Documents/Victoria//4th year//STAT4601//MakeStereo.r")

make.Stereo(playstore_prc$x[,c(1,2,3)], classes, Main="SDSS", asp="F",
            Xlab="PC2", Ylab="PC3", Zlab="PC1")


set.seed(456) #for random parts


#kmeans

#for kmeans we are going to us the PCA components, we are going to 
#use the first 5 since they explain 79% of the variance 
playstore_km<-as.data.frame(playstore_prc$x[,c(1:3)])
playstore_km<-scale(playstore_km, center = TRUE, scale = TRUE)


##kmeans algorithm with various parameters
k2 <- kmeans(playstore_km, centers = 2, nstart = 25)
k3 <- kmeans(playstore_km, centers = 3, nstart = 25)
k4 <- kmeans(playstore_km, centers = 4, nstart = 25)
k5 <- kmeans(playstore_km, centers = 5, nstart = 25)

#plots for kmeans 
p1 <- fviz_cluster(k2, data = playstore_km)
p2 <- fviz_cluster(k3, data = playstore_km)
p3 <- fviz_cluster(k4, data = playstore_km)
p4 <- fviz_cluster(k5, data = playstore_km)

#arrange plots in a grid
grid.arrange(p1,p2,p3,p4, nrow=2)

##use the expansion code from the course docs 
source("//Users//Victoria//Documents/Victoria//4th year//STAT4601//confusion.expand.r")
#confusion matrices for each # of centers 
print("2 centers")
confusion.expand(k2$cluster, classes)
print("3 centers")
confusion.expand(k3$cluster, classes)
print("4 centers")
confusion.expand(k4$cluster, classes)
print("5 centers")
confusion.expand(k5$cluster, classes)

#heirarchial clustering
playstore_hc<-as.data.frame(playstore_prc$x[,1:3])
playstore_hc<-scale(playstore_hc)

c.dist <- dist(playstore_hc)
# Complete Linkage - heirarchial clustering
c.hclust <- hclust(c.dist, method = "complete" )
c.hclust$labels <- classes

#reg dendrogram
plot(c.hclust, main = "Complete Linkage")
rect.hclust(c.hclust, k = 3, border = "purple")
rect.hclust(c.hclust, k = 2, border = "blue")
rect.hclust(c.hclust, k = 4, border = "green")
rect.hclust(c.hclust, k = 5, border = "orange")

hc2 <- cutree(c.hclust, k=2)
hc3 <- cutree(c.hclust, k=3)
hc4 <- cutree(c.hclust, k=4)
hc5 <- cutree(c.hclust, k=5)


testing<-as.data.frame(playstore_km)
c.hclust$labels <- names(testing)

#this is very finiky and thats why I put it in as static images. 
colors = c("red", "blue", "yellow","purple", "green")
c.hclust<-as.phylo(c.hclust)
complete<-plot(as.phylo(c.hclust), type = "fan", tip.color = colors[hc3],
               label.offset = 1, cex = 0.7)

complete<-plot(as.phylo(c.hclust), type = "fan", tip.color = colors[hc4],
               label.offset = 1, cex = 0.7)

complete<-plot(as.phylo(c.hclust), type = "fan", tip.color = colors[hc5],
               label.offset = 1, cex = 0.7)



## !you need the hc objects from above for these confusion matrices!
print("2 clusters")
confusion.expand(hc2, classes)
print("3 clusters")
confusion.expand(hc3, classes)
print("4 clusters")
confusion.expand(hc4, classes)
print("5 clusters")
confusion.expand(hc5, classes)

#classification 
#neural net

source("//Users//Victoria//Documents/Victoria//4th year//STAT4601//L9//functions//get.train.R")

"%w/o%" <- function(x,y) x[!x %in% y]

star.species <- c(rep("Star",4152),rep("Galaxy",4998),rep("QSO",850))
t(class.ind(star.species))
apply(class.ind(star.species), 2, sum)
Train.sz <- 2000
# Get the indices for the training and test samples
set.seed(356)

playstore_reduc2<-playstore[,-c(1,10)]

#get testing training set
tt.ind <- get.train(dim(playstore_reduc2)[1], Train.sz)

#look at the proportions
apply(class.ind(star.species[tt.ind$train]), 2, sum)
apply(class.ind(star.species[tt.ind$test]), 2, sum)

#these are the testing and training sets
trainset <- playstore_reduc2[tt.ind$train, ]
testset <- playstore_reduc2[tt.ind$test,]

trainset$class <- ifelse(trainset$class %in% "GALAXY", 1, ifelse(
  trainset$class %in% "STAR", 2, 3
))

#no node in hidden layer
star.nn0 <- nnet(formula = trainset$class ~ ., data = trainset,
                 size = 0, skip = TRUE, rang = 0.1, decay = 5e-4, maxit = 500)


# Predict
t(star.nn0$fitted.values)
t(predict(star.nn0, playstore_reduc2[tt.ind$train, ]))
max.col(star.nn0$fitted.values)

#2 nodes in hidden layer
star.nn2 <- nnet(formula = trainset$class ~ ., data = trainset,
                 size = 2, skip = TRUE, rang = 0.1, decay = 5e-4, maxit = 500)

t(star.nn2$fitted.values)
t(predict(star.nn2, playstore_reduc2[tt.ind$train, ]))
max.col(star.nn2$fitted.values)

#3 nodes in hidden layer
star.nn3 <- nnet(formula = trainset$class ~ ., data = trainset,
                 size = 3, skip = TRUE, rang = 0.1, decay = 5e-4, maxit = 500)
t(star.nn3$fitted.values)

t(predict(star.nn3, playstore_reduc2[tt.ind$train, ]))
max.col(star.nn3$fitted.values)

#6 nodes in hidden layer
star.nn6 <- nnet(formula = trainset$class ~ ., data = trainset,
                 size = 6, skip = TRUE, rang = 0.1, decay = 5e-4, maxit = 500)

t(star.nn6$fitted.values)
t(predict(star.nn6, playstore_reduc2[tt.ind$train, ]))
max.col(star.nn6$fitted.values)

# confusion matrix
# Check accuracy
print("no nodes")
mda::confusion(max.col(star.nn0$fitted.values), star.species[tt.ind$train])
print("2 nodes")
mda::confusion(max.col(star.nn2$fitted.values), star.species[tt.ind$train])
print("3 nodes")
mda::confusion(max.col(star.nn3$fitted.values), star.species[tt.ind$train])
print("5 nodes")
mda::confusion(max.col(star.nn6$fitted.values), star.species[tt.ind$train])

#prep for knn
dummy <- playstore_reduc2[tt.ind$train,]
dummy2 <- playstore_reduc2[tt.ind$test,]


trainset$class <- ifelse(trainset$class %in% "GALAXY", 1, ifelse(
  trainset$class %in% "STAR", 2, 3
))

testset$class <- ifelse(testset$class %in% "GALAXY", 1, ifelse(
  testset$class %in% "STAR", 2, 3
))

train.class<-as.factor(dummy$class)
test.class<-as.factor(dummy2$class)


# 3 neighbors
knn.pred3<-knn(trainset,testset,train.class,k=3)

# 25 neighbors
knn.pred25<-knn(trainset,testset,train.class,k=25)

#100 neighbors
knn.pred100<-knn(trainset,testset,train.class,k=100)

print("3 neighbors")
caret::confusionMatrix(knn.pred3, test.class)

print("25 neighbors")
caret::confusionMatrix(knn.pred25, test.class)

print("100 neighbors")
caret::confusionMatrix(knn.pred100, test.class)
