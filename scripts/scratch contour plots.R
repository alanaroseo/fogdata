#####
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.60,0.7,0.95),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 5,                 # size of axis annotation
    tck = -0.01,
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
        nlevels=20,
        lwd =6, 
        labcex=5,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)
#dots
points(dots$max.treetop.gs, dots$max.VPD.diff,
     xlab = "",        # suppress x-axis annotation
     ylab = "",        # suppress y-axis annotation
     xlim = c(min(gs_t),max(gs_t)),
     ylim = c(min(VPD),max(VPD)),
     pch=21,
     bg="midnightblue",
     col="white",
     cex=8
)

#treetop dots only
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
  nlevels=7,
  lwd =6, 
  labcex=5,
  xlab = "",
  ylab = "",
  xlim = c(min(gs_m),max(gs_m)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(2500)),
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
        nlevels=8,
        lwd =6, 
        labcex=5,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add = TRUE)
#

######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.65,0.68,0.15,0.85),   # define plot region for legend
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
text(x=11,y=1.5,"",cex = 1.5,font = .5)
MakeLetter2( "Seconds
             
             
             
             
             
             
             
             
             
             
             ")





#########
#plots with all dots

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.60,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 5,                 # size of axis annotation
    tck = -0.01,
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
        nlevels=12,
        lwd =6, 
        labcex=5,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)
#dots
points(dots$max.treetop.gs, dots$max.VPD.diff,
       xlab = "",        # suppress x-axis annotation
       ylab = "",        # suppress y-axis annotation
       xlim = c(min(gs_t),max(gs_t)),
       ylim = c(min(VPD),max(VPD)),
       pch=21,
       bg="midnightblue",
       col="white",
       cex=4
)


######################################################################
#
#

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.6,0.38,0.65),  # defining window for second plot
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
  nlevels=7,
  lwd =6, 
  labcex=5,
  xlab = "",
  ylab = "",
  xlim = c(min(gs_m),max(gs_m)),
  ylim = c(min(VPD),max(VPD)),
  zlim = c(min(seconds_t),max(2500)),
  add=TRUE
)



######################################################################
#
#Bottom plot:
par(new = "TRUE",
    plt = c(0.2,0.6,0.05,0.32),
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
        nlevels=8,
        lwd =6, 
        labcex=5,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add = TRUE)
#

######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.63,0.66,0.15,0.85),   # define plot region for legend
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
text(x=11,y=1.5,"",cex = 1.5,font = .5)
MakeLetter2( "          Seconds
             
             
            

             
             ")

