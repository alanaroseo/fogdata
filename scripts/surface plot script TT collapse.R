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

##### Generate Full range of VPD data #####

VPD <-  seq(.5,1.5,length=30)#could use by= instead of length= to specify increment

##### Generate Full range of gs data #####

gs <-  seq(.8,1.8,length=length(VPD))# the same length as the VPD vector


####### calculate seconds of transpiration sustained

V_released=15 #constant representing mean voulume released during TT collapse (mol*m^-2):
#very highest 5 sampes: 0.06, all above 100:0.13, 70-82m: 0.27, lowest 6 samples (to catch bottom of crown for all trees):0.19.

grid <- function(gs, VPD){
  V_released/(gs*(VPD/99.6))#transpiration (mol*m^-2*second^-1)
}#the function "grid" is calculates the # of seconds of transpiration suatainable 
seconds <- outer(gs,VPD,grid)#outer() function applies the function "grid" at every combination of gs and VPD. Seconds is the z axis (a matrix)

###### the plot

persp(gs,VPD,seconds,theta = 50, phi = 15,col = "springgreen", shade = 0.5,ticktype = "detailed")#the plot

persp3D(gs,VPD,seconds,theta = 50, phi = 30,colvar = seconds, shade = 0.5,ticktype = "detailed", xlab = "gs",ylab = "VPD", zlab = "seconds",colkey = TRUE, facets = FALSE)

persp3D(gs,VPD,seconds,theta = 50, phi = 30,colvar = seconds, shade = 0.5,ticktype = "detailed", xlab = "gs",ylab = "VPD", zlab = "seconds",colkey = TRUE,border = "white")

surf3D(seconds, gs, VPD, colvar = seconds, colkey = TRUE, box = FALSE)

wireframe(seconds)
require(rgl)
surface3d(gs, VPD, seconds)#require(rgl)
 
