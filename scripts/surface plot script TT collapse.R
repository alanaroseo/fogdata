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
library(RColorBrewer)
library(rgl)
library(viridis)
library(tidyverse)

##### Generate Full range of VPD data #####

#all
VPD <-  seq(.5,1.5,length=100)#could use by= instead of length= to specify increment

##### Generate Full range of gs data #####
#treetop
gs_t <-  seq(.01,.15,length=length(VPD))# the same length as the VPD vector
#midcrown
gs_m <-  seq(.01,.15,length=length(VPD))
#bottom
gs_b <-  seq(.01,.15,length=length(VPD))


#mean treetop max gs:0.1, mid:0.08, lower:0.06


####### calculate seconds of transpiration sustained

#treetop
V_released_t=0.06 #constant representing mean volume released during TT collapse (mol*m^-2):
#midcrown
V_released_m=0.27 
#bottom
V_released_b=0.19

#very highest 5 sampes: 0.06, all above 100:0.13, 70-82m: 0.27, lowest 6 samples (to catch bottom of crown for all trees):0.19.

#####
######Make a function and calculate a z matrix

#treetop
E_t <- function(gs_t, VPD){
  V_released_t/(gs_t*(VPD/99.6))#transpiration (mol*m^-2*second^-1)
}#the function "grid" is calculates the # of seconds of transpiration suatainable 
seconds_t <- outer(gs_t,VPD,E_t)#outer() function applies the function "grid" at every combination of gs and VPD. Seconds is the z axis (a matrix)
rownames(seconds_t) = gs_t
colnames(seconds_t) = VPD

#midcrown
E_m <- function(gs_m, VPD){
  V_released_m/(gs_m*(VPD/99.6))#transpiration (mol*m^-2*second^-1)
}#the function "grid" is calculates the # of seconds of transpiration suatainable 
seconds_m <- outer(gs_m,VPD,E_m)#outer() function applies the function "grid" at every combination of gs and VPD. Seconds is the z axis (a matrix)
rownames(seconds_m) = gs_m
colnames(seconds_m) = VPD

#treetop
E_b <- function(gs_b, VPD){
  V_released_b/(gs_b*(VPD/99.6))#transpiration (mol*m^-2*second^-1)
}#the function "grid" is calculates the # of seconds of transpiration suatainable 
seconds_b <- outer(gs_b,VPD,E_b)#outer() function applies the function "grid" at every combination of gs and VPD. Seconds is the z axis (a matrix)
rownames(seconds_b) = gs_b
colnames(seconds_b) = VPD

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

gg <- as.data.frame(c(gs,VPD))
gg$seconds <- with(gg,E_t(gs,VPD))      # need long format for ggplot
library(ggplot2)
library(RColorBrewer)               #for brewer.pal()
brks <- cut(gg$seconds,breaks=seq(0,100,len=6))
brks <- gsub(","," - ",brks,fixed=TRUE)
gg$brks <- gsub("\\(|\\]","",brks)  # reformat guide labels

ggplot(gg,aes(gs,VPD)) + 
  geom_tile(fill=seconds)+
  scale_fill_continuous("viridis")

filled.contour(gs,VPD,seconds,nlevels=60,col=plasma(59,direction = -1))#uses lattice package

contour(gs,VPD,seconds,nlevels=20,col=inferno(22,direction = -1), lwd =3, labcex=1, add=TRUE)#uses lattice package

#####
#filled contour 3
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }
#combo plots

#Source the following functions (change the paths as necessary)
source("http://http://wiki.cbr.washington.edu/qerm/sites/qerm/images/1/16/Filled.contour3.R")
source("http://wiki.cbr.washington.edu/qerm/images/2/25/Filled.legend.R")


MakeLetter <- function(a, where="topleft", cex=2)
  legend(where, pt.cex=0, bty="n", title=a, cex=cex, legend=NA)
        

#####
#filled legend
filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                         length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes, ...) 
  {
    # modification of filled.contour by Carey McGilliard and Bridget Ferris
    # designed to just plot the legend
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #  on.exit(par(par.orig))
    #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    #  par(las = las)
    #  mar <- mar.orig
    #  mar[4L] <- mar[2L]
    #  mar[2L] <- 1
    #  par(mar = mar)
    # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
  }
#
#    if (!missing(key.title)) 
#        key.title
#    mar <- mar.orig
#    mar[4L] <- 1
#    par(mar = mar)
#    plot.new()
#    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
#    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
#        stop("no proper 'z' matrix specified")
#    if (!is.double(z)) 
#        storage.mode(z) <- "double"
#    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
#        col = col))
#    if (missing(plot.axes)) {
#        if (axes) {
#            title(main = "", xlab = "", ylab = "")
#            Axis(x, side = 1)
#            Axis(y, side = 2)
#        }
#    }
#    else plot.axes
#    if (frame.plot) 
#        box()
#    if (missing(plot.title)) 
#        title(...)
#    else plot.title
#    invisible()
#}

#####
## An annotation inside plot:
#The xpd=NA allows for writing outside the plot limits, but still using the the x and y axes to place the text
#par()
#text(x=31,y=1.5,"x",cex = 1.5,font = 1)
#MakeLetter( "(a)")

#The plots

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.60,0.7,0.95),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 5,                 # size of axis annotation
    tck = -0.02,
    mgp = c(3, 4, 0))                 # major tick size and direction, < 0 means outside

#Top plot:
#
# the filled contour - colored areas
filled.contour3(gs_t,
                VPD,
                seconds_t,
                nlevels=40,
                col=viridis(25,direction = -1),
                xlab = "",        # suppress x-axis annotation
                ylab = "",        # suppress y-axis annotation
                xlim = c(min(gs_t),max(gs_t)),
                ylim = c(min(VPD),max(VPD)),
                zlim = c(min(seconds_t),max(2500))
)
# the contour part - draw iso-lines
contour(gs_t,
        VPD,
        seconds_t,
        nlevels=40,
        lwd =6, 
        labcex=5,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(seconds_m)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)
#

######################################################################
#
#

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.6,0.38,0.63),  # defining window for second plot
    las = 1,
    cex.axis = 5)
#
filled.contour3(
  gs_m,
  VPD,
  seconds_m,
  col=viridis(25,direction = -1),
  xlab = "",
  ylab = "",
  xlim = c(min(gs_m),max(gs_m)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(2500))
)
#
contour(
  gs_m,
  VPD,
  seconds_m,
  nlevels=10,
  lwd =6, 
  labcex=5,
  xlab = "",
  ylab = "",
  xlim = c(min(gs_m),max(gs_m)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(seconds_m)),
  add=TRUE
)



######################################################################
#
#Bottom plot:
par(new = "TRUE",
    plt = c(0.2,0.6,0.05,0.3),
    las = 1,
    cex.axis = 5)
#
filled.contour3(
  gs_b,
  VPD,
  seconds_b,
  col=viridis(25,direction = -1),
  xlab = "",
  ylab = "",
  xlim = c(min(gs_b),max(gs_b)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(2500))
)
#
contour(gs_b,
        VPD,
        seconds_b,
        nlevels=20,
        lwd =6, 
        labcex=5,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(seconds_m)),
        add = TRUE)
#

######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.70,0.75,0.15,0.85),   # define plot region for legend
    las = 1,
    cex.axis = 5)
#
filled.legend(
  gs_t,
  VPD,
  seconds_m,
  col=viridis(25,direction = -1),
  xlab = "",
  ylab = "",
  xlim = c(min(gs_b),max(gs_t)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(2500)))

#lable it
par(xpd = NA)
text(x=11,y=1.5,"x",cex = 1.5,font = .5)
MakeLetter2( "Seconds
             
             
             
             
             
             
             
             
             
             
             ")


#####save it
tiff(file="contour.tiff", units ="mm", width=82.3, height = 140, res = 600)
contour_plot
dev.off()

#Add some figure labels
par(xpd=NA,cex = 1.3)
text(x = -7,y = 0,"gs",cex = 1.3)
text(x = -10,y =2,srt = 90,expression(paste(italic(Delta),"VPD",sep = "")),cex = 1.3)



#####
MakeLetter <- function(a, where="left", cex=4)
  legend(where, pt.cex=0, bty="n", title=a, cex=cex, legend=NA)



MakeLetter2 <- function(a, where="top", cex=5)
  legend(where, pt.cex=0, bty="n", title=a, cex=cex, legend=NA)

MakeLetter3 <- function(a, where="topright", cex=1)
  legend(where, pt.cex=-20, bty="n", title=a, cex=cex, legend=NA)

#####
#save as jpeg
jpeg(file="surfaceplot.jpg",width=4000,height=3500)

plot3D::persp3D(gs,VPD,seconds,theta=140, phi=25,xlab = "gs",ylab = "VPD increase", zlab = "Seconds",
                cex.lab=10,cex.axis=5,ticktype="detailed", clab = "Seconds", border = "white",colkey = list( length = 0.5,cex.clab=5,cex.axis=5, dist=-.03,side.clab =1),col = viridis(50))
dev.off()

 
