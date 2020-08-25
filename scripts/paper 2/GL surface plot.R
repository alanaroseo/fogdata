
library(viridis)
##### Generate Full range of height #####

#20-110m
height <-  seq(20,110,length=100)#Height in meters. could use by= instead of length= to specify increment

#or use true heights
height <-  as.numeric(read.delim("clipboard"))
#T11 height bins for uptake:
Uheight <- c(23.0,
            28.0,
            33.0,
            38.0,
            43.0,
            48.0,
            53.0,
            58.0,
            63.0,
            68.0,
            73.0,
            78.0,
            83.0,
            88.0,
            93.0
)

#T11 height bins for photosynthesis :
Aheight <- c(28.0,
            33.0,
            38.0,
            43.0,
            48.0,
            53.0,
            58.0,
            63.0,
            68.0,
            73.0,
            78.0,
            83.0,
            88.0,
            93.0
)
class(height)
### Generate Full range of G fraction data #####
#fraction of total leaf area
G_frac <-  seq(.01,.5,length=15)
AG_frac <-  seq(.01,.5,length=14)
#or use this:
G_frac <-  as.data.frame(read.delim("clipboard"))
G_frac <-  as.list(read.delim("clipboard"))
class(G_frac)
print(AG_frac)

#read in the uptake z matrix:
uptake <- as.matrix(read.delim("clipboard"))

rownames(uptake) = G_frac
colnames(uptake) = Uheight

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

########
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.75,0.5,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 3,                 # size of axis annotation
    tck = -0.01,
    mgp = c(3, 1.5, 0))                 # major tick size and direction, < 0 means outside

#Top plot:
#col=viridis(25,direction = -1),
# the filled contour - colored areas
filled.contour3(G_frac,
                height,
                uptake,
                nlevels=30,
                col=plasma(33),
                xlab = "",        # suppress x-axis annotation
                ylab = "",        # suppress y-axis annotation
                xlim = c(min(G_frac),max(G_frac)),
                ylim = c(min(height),max(height)),
                zlim = c(min(uptake),max(uptake))
)
# the contour part - draw iso-lines
contour(G_frac,
        height,
        uptake,
        nlevels=5,
        lwd =1, 
        labcex=1,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(G_frac),max(G_frac)),
        ylim = c(min(height),max(height)),
        zlim = c(min(uptake),max(uptake)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)
#dots
######################
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 3,                 # size of axis annotation
    tck = -0.01,
    mgp = c(3, 1.5, 0))                 # major tick size and direction, < 0 means outside

#Top plot:
#col=viridis(25,direction = -1),
# the filled contour - colored areas
filled.contour3(G_frac,
                height,
                photosynth,
                col=viridis(25),
                nlevels=40,
                xlab = "",        # suppress x-axis annotation
                ylab = "",        # suppress y-axis annotation
                xlim = c(min(G_frac),max(G_frac)),
                ylim = c(min(height),max(height)),
                zlim = c(min(photosynth),max(photosynth))
)
# the contour part - draw iso-lines
contour(G_frac,
        height,
        photosynth,
        nlevels=5,
        lwd =1, 
        labcex=1,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(G_frac),max(G_frac)),
        ylim = c(min(height),max(height)),
        zlim = c(min(photosynth),max(photosynth)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)
#dots


######################################################################

###########################
#Add a legend:
plot.new()
par(new = "TRUE",
    plt = c(0.63,0.66,0.15,0.85),   # define plot region for legend
    las = 1,
    cex.axis = 3)
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
text(x=11,y=1.5,"",cex = 1.5,font = .5)
MakeLetter2( "        Seconds
             
             
             
             

             
             
             ")

#the legend #legend
