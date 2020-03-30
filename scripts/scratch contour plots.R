#original plot for height-based trend
########
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 3,                 # size of axis annotation
    tck = -0.01,
    mgp = c(3, 1.5, 0))                 # major tick size and direction, < 0 means outside

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
        nlevels=16,
        lwd =4, 
        labcex=3,
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



######################################################################

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 3)
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
  nlevels=8,
  lwd =4, 
  labcex=3,
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
    plt = c(0.2,0.45,0.05,0.32),
    las = 1,
    cex.axis = 3)
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
        lwd =4, 
        labcex=3,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add = TRUE)
#


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

#########
#plot minimum (23%) collapse:

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 3,                 # size of axis annotation
    tck = -0.01,
    mgp = c(3, 1.5, 0))                 # major tick size and direction, < 0 means outside

#Top plot:
#
# the filled contour - colored areas
filled.contour3(gs_t,
                VPD,
                seconds_t_min,
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
        seconds_t_min,
        nlevels=7,
        lwd =4, 
        labcex=3,
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


#mimimum
######################################################################
#
#

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 3)
#
filled.contour3(
  gs_m,
  VPD,
  seconds_m_min,
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
  seconds_m_min,
  nlevels=8,
  lwd =4, 
  labcex=3,
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
    plt = c(0.2,0.45,0.05,0.32),
    las = 1,
    cex.axis = 3)
#
filled.contour3(
  gs_b,
  VPD,
  seconds_b_min,
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
        seconds_b_min,
        nlevels=15,
        lwd =4, 
        labcex=3,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add = TRUE)

######################################################################

#########
#plot maximum (75%) collapse:

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 3,                 # size of axis annotation
    tck = -0.01,
    mgp = c(3, 1.5, 0))                 # major tick size and direction, < 0 means outside

#Top plot:
#
# the filled contour - colored areas
filled.contour3(gs_t,
                VPD,
                seconds_t_max,
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
        seconds_t_max,
        nlevels=7,
        lwd =4, 
        labcex=3,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines
)

#maximum
######################################################################
#
#

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 3)
#
filled.contour3(
  gs_m,
  VPD,
  seconds_m_max,
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
  seconds_m_max,
  nlevels=8,
  lwd =4, 
  labcex=3,
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
    plt = c(0.2,0.45,0.05,0.32),
    las = 1,
    cex.axis = 3)
#
filled.contour3(
  gs_b,
  VPD,
  seconds_b_max,
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
        seconds_b_max,
        nlevels=7,
        lwd =4, 
        labcex=3,
        xlab = "",
        ylab = "",
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add = TRUE)

######################################################################


