###########################################
# Surface Plot Script - Alana Chin        #
#                                         #
# generates a range of VPD and gs values  #
# plots a surface exploring the range of  #
#  number of seconds transpiration is     #
#sustained by transfusion tissue collapse #
#   as a constant volume                  #
###########################################


##### Load libraries #####
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(investr)
library(lattice)
library(plot3D)
library(rgl)
library(viridis)
library(tidyverse)
##### Generate Full range of VPD data #####

VPD <-  seq(.5,1.5,length=45)#could use by= instead of length= to specify increment

##### Generate Full range of gs data #####

gs <-  seq(.03,.06,length=length(VPD))# the same length as the VPD vector
#mean treetop max gs:0.1, mid:0.08, lower:0.06


####### calculate seconds of transpiration sustained

V_released=0.19 #constant representing mean volume released during TT collapse (mol*m^-2):
#very highest 5 sampes: 0.06, all above 100:0.13, 70-82m: 0.27, lowest 6 samples (to catch bottom of crown for all trees):0.19.

grid <- function(gs, VPD){
  V_released/(gs*(VPD/99.6))#transpiration (mol*m^-2*second^-1)
}#the function "grid" is calculates the # of seconds of transpiration suatainable 
seconds <- outer(gs,VPD,grid)#outer() function applies the function "grid" at every combination of gs and VPD. Seconds is the z axis (a matrix)
rownames(seconds) = gs
colnames(seconds) = VPD

######
#the 3D plots

cols2 <- c(rgb(107/255,184/255,214/255,1),rgb(0/255,90/255,124/255,1))

persp(gs,VPD,seconds,theta = 50, phi = 15,col = "springgreen", shade = 0.5,ticktype = "detailed")#oldschool

persp3D(gs,VPD,seconds,theta = 40, phi = 30,colvar = seconds, shade = 0.5,ticktype = "detailed", xlab = "gs",ylab = "VPD increase", zlab = "Seconds",colkey = TRUE, facets = FALSE, clab = "Seconds")

persp3D(gs,VPD,seconds,theta = 140, phi = 25,colvar = seconds, shade = 0.5,ticktype = "detailed", xlab = "gs",ylab = "VPD increase", zlab = "Seconds",colkey = TRUE, facets = FALSE, clab = "Seconds",cex.lab=1,cex.axis=0.5)

persp3D(gs,VPD,seconds,theta = 140, phi = 30, shade = 0.5,ticktype = "detailed", xlab = "gs",ylab = "VPD", zlab = "Seconds",colkey = list(side = 1, length = 0.5), border = "white", clab = "Seconds",cex.lab=1,cex.axis=0.5, col = viridis(50))

persp3D(gs,VPD,seconds,theta=140, phi=25,xlab = "gs",ylab = "VPD increase", zlab = "Seconds",
        cex.lab=1,cex.axis=0.5,ticktype="detailed", clab = "Seconds", col = viridis(50))

#####
#2D plots

gg <- expand.grid(gs=gs,VPD=VPD)
gg$seconds <- with(gg,grid(gs,VPD))      # need long format for ggplot
library(ggplot2)
library(RColorBrewer)               #for brewer.pal()
brks <- cut(gg$seconds,breaks=seq(0,100,len=6))
brks <- gsub(","," - ",brks,fixed=TRUE)
gg$brks <- gsub("\\(|\\]","",brks)  # reformat guide labels
ggplot(gg,aes(gs,VPD)) + 
  geom_tile(aes(fill=brks))+
  scale_fill_manual("seconds",values=brewer.pal(6,"YlOrRd"))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_fixed()

filled.contour(gs,VPD,seconds,nlevels=60,col=viridis(54))#uses lattice package

contour(gs,VPD,seconds,nlevels=60,col=viridis(54))#uses lattice package
#####
#save as jpeg
jpeg(file="surfaceplot.jpg",width=4000,height=3500)

plot3D::persp3D(gs,VPD,seconds,theta=140, phi=25,xlab = "gs",ylab = "VPD increase", zlab = "Seconds",
                cex.lab=10,cex.axis=5,ticktype="detailed", clab = "Seconds", border = "white",colkey = list( length = 0.5,cex.clab=5,cex.axis=5, dist=-.03,side.clab =1),col = viridis(50))
dev.off()

 
