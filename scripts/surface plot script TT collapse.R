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

gg <- as.data.frame(c(gs,VPD))
gg$seconds <- with(gg,grid(gs,VPD))      # need long format for ggplot
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
#The plots
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.1,0.4,0.60,0.95),   # using plt instead of mfcol (compare
    # coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 1,                 # size of axis annotation
    tck = -0.02 )                 # major tick size and direction, < 0 means outside

#Top left plot:
#
# the filled contour - coloured areas
filled.contour3(gs,
                VPD,
                seconds,
                color=terrain.colors,
                xlab = "",        # suppress x-axis annotation
                ylab = "",        # suppress y-axis annotation
                xlim = c(min(gs),max(gs)),
                ylim = c(min(VPD),max(VPD)),
                zlim = c(min(seconds),max(seconds))
)
# the contour part - draw iso-lines
contour(gs,
        VPD,
        seconds,
        color=terrain.colors,
        xlab = "",
        ylab = "",
        xlim = c(min(gs),max(gs)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds),max(seconds)),
        add=TRUE,                 # add the contour plot to filled-contour,
        #thus making an overlay
        col = grey(0.4)           # color of overlay-lines
)
#
# An annotation inside first plot
#The xpd=NA allows for writing outside the plot limits, but still using the the x and y axes to place the text
par(xpd = NA)
text(x=11,y=1.5,"x",cex = 1.5,font = 2)
MakeLetter( "(a)")

######################################################################
#
#
plot.new()
#Top right plot:
par(new = "TRUE",
    plt = c(0.5,0.8,0.60,0.95),  # defining window for second plot
    las = 1,
    cex.axis = 1)
#
filled.contour3(
  gs,
  VPD,
  seconds,
  color=heat.colors,
  xlab = "",
  ylab = "",
  xlim = c(min(gs),max(gs)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds),max(seconds))
)
#
contour(
  gs,
  VPD,
  seconds,
  xlab = "",
  ylab = "",
  xlim = c(min(gs),max(gs)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds),max(seconds)),
  add=TRUE
)
#
#Alternatively, you could set z axis limits to depend
#on the min and max values in seconds.
#filled.contour3(gs,VPD,seconds,color=heat.colors,xlab = "",ylab = "",xlim = c(min(gs),max(gs)),ylim = c(min(VPD),max(VPD)),zlim = c(min(seconds),max(seconds)))
#
# Add annotation
text(x=11,
     y=1.5,
     "x",
     cex = 1.5,
     font = 2)

print.letter(text = "(b)")

######################################################################
#
#Bottom left plot:
par(new = "TRUE",
    plt = c(0.1,0.4,0.15,0.5),
    las = 1,
    cex.axis = 1)
#
filled.contour3(gs,
                VPD,
                seconds,
                col=colorpanel(11, "white", "grey10"),
                nlevels=11,
                xlab = "",
                ylab = "",
                xlim = c(min(gs),max(gs)),
                ylim = c(min(VPD),max(VPD)),
                zlim = c(-1,1))
#
contour(gs,
        VPD,
        seconds,
        xlab = "",
        ylab = "",
        xlim = c(min(gs),max(gs)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(-1,1),
        add = TRUE)
#
text(x=11,
     y=1.5,
     "x",
     cex = 1.5,
     font = 2,
     col = "white")

print.letter(text = "(c)",printcolor = "blue")

######################################################################
#
#Bottom right plot:
par(new = "TRUE",
    plt = c(0.5,0.8,0.15,0.5),
    las = 1,
    cex.axis = 1)
#
filled.contour3(
  gs,
  VPD,
  seconds,
  color = terrain.colors,
  xlab = "",
  ylab = "",
  xlim = c(min(gs),max(gs)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(-1,1)
)
#
contour(
  gs,
  VPD,
  seconds,
  xlab = "",
  ylab = "",
  xlim = c(min(gs),max(gs)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(-1,1),
  add=TRUE
)

text(x=11,
     y=1.5,
     "hello",
     cex = 1.5,
     font = 2)
print.letter(text = "(d)")
#
######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.85,0.9,0.25,0.85),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  gs,
  VPD,
  seconds,
  color = terrain.colors,
  xlab = "",
  ylab = "",
  xlim = c(min(gs),max(gs)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds),max(seconds)))

#Add some figure labels
par(xpd=NA,cex = 1.3)
text(x = -16.7,y = 0,"gs",srt = 90,cex = 1.3)
text(x = -8,y = -1.62,expression(paste(italic(Delta),"VPD",sep = "")),cex = 1.3)



#####
MakeLetter <- function(a, where="topleft", cex=2)
  legend(where, pt.cex=0, bty="n", title=a, cex=cex, legend=NA)

#####
#save as jpeg
jpeg(file="surfaceplot.jpg",width=4000,height=3500)

plot3D::persp3D(gs,VPD,seconds,theta=140, phi=25,xlab = "gs",ylab = "VPD increase", zlab = "Seconds",
                cex.lab=10,cex.axis=5,ticktype="detailed", clab = "Seconds", border = "white",colkey = list( length = 0.5,cex.clab=5,cex.axis=5, dist=-.03,side.clab =1),col = viridis(50))
dev.off()

 
