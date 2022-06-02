library(ggplot2)
library(ggnewscale)
library(patchwork)
require(scales)
# set the working directory
setwd("")
deco <- read.csv("data_morphological description.txt",  sep = "")

# plots a-e
p1<-ggplot(deco, aes(x = "",y = leaves )) + 
  geom_violin(trim=FALSE, size=0.5)+geom_boxplot(width=0.1, outlier.shape=NA, size=0.5)+ylim(1,21)+ylab("n")+xlab("Leaves")+theme_bw()+
  theme(axis.title=element_text(size=8))+ggtitle("(a)")
p2 <- ggplot(deco, aes(x = direc.)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+ylab("%")+xlab("Directions")+theme_bw()+
  theme(axis.title=element_text(size=8))+ggtitle("(b)")
p3 <- ggplot(deco, aes(x = knots)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+ylab("%")+xlab("Knots")+theme_bw()+
  theme(axis.title=element_text(size=8))+ggtitle("(c)")
p4<-ggplot(deco, aes(x = "",y = length)) + 
  geom_violin(trim=FALSE, size=0.5)+geom_boxplot(width=0.1, outlier.shape=NA, size=0.5)+ylab("cm")+ylim(0,25)+xlab("Length")+theme_bw()+
  theme(axis.title=element_text(size=8))+ggtitle("(d)")
p5<-ggplot(deco, aes(x = "",y = width)) + 
  geom_violin(trim=FALSE, size=0.5)+geom_boxplot(width=0.1, outlier.shape=NA, size=0.5)+ylim(0,4)+ylab("cm")+xlab("Width")+theme_bw()+
  theme(axis.title=element_text(size=8))+ggtitle("(e)")
##PCA 
library(factoextra)
pca1 <- prcomp(deco, scale = TRUE)
fviz_eig(pca1)
#eigenvalue and explained variation 
eig.val <- get_eigenvalue(pca1)
eig.val

#plot PCA with the function fviz_pca_biplot and geom_density_2d_filled from ggplot2

pcaplot<-fviz_pca_biplot(pca1, geom="point", repel = TRUE)+ geom_density_2d_filled(alpha = 0.6) +
  geom_point(alpha =1) +theme_bw()+theme(legend.position = "none")+ylab("Dim2 (19%)")+xlab("Dim1 (61%)")+ggtitle("")+
  scale_y_reverse()+scale_x_reverse()+ggtitle("(f)")

#plot
(p1+p2+p3+p4+p5)|pcaplot

