

# height-decreasing -------------------------------------------------------
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 2.2,                 # size of axis annotation
    tck = -0.01,
    mgp = c(1.5, 1.5, 0))                 # major tick size and direction, < 0 means outside

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
        levels= c(seconds_t=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        # suppress x-axis annotation
        ylab = "",        # suppress y-axis annotation
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 # add the contour plot to filled-contour,
        #thus making an overlay
        # color of overlay-lines, col
)


#height-based trend
######################################################################

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 2.2)
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
contour(gs_m,
        VPD,
        seconds_m,
        levels= c(seconds_m=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
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
    cex.axis = 2.2)
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
        levels= c(seconds_b=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        
        ylab = "",        
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 
)
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


#legend


# minimum -----------------------------------------------------------------
#plot minimum (23%) collapse:

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 2.2,                 # size of axis annotation
    tck = -0.01,
    mgp = c(1.5, 1.5, 0))                 # major tick size and direction, < 0 means outside

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
#
contour(gs_t,
        VPD,
        seconds_t_min,
        levels= c(seconds_t_min=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        
        ylab = "",        
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 
)


#mimimum#min
######################################################################
#
#

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 2.2)
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
contour(gs_m,
        VPD,
        seconds_m_min,
        levels= c(seconds_m_min=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
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
    cex.axis = 2.2)
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
        levels= c(seconds_b_min=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        
        ylab = "",        
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 
)

######################################################################


# maximum -----------------------------------------------------------------


#plot maximum (71%) collapse:

plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.2,0.45,0.71,0.98),   # using plt instead of mfcol (compare coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 2.2,                 # size of axis annotation
    tck = -0.01,
    mgp = c(1.5, 1.5, 0))                 # major tick size and direction, < 0 means outside

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
# 
contour(gs_t,
        VPD,
        seconds_t_max,
        levels= c(seconds_t_max=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        
        ylab = "",        
        xlim = c(min(gs_t),max(gs_t)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 
)

#maximum#max
######################################################################

#middle plot:
par(new = "TRUE",
    plt = c(0.2,0.45,0.38,0.65),  # defining window for second plot
    las = 1,
    cex.axis = 2.2)
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
contour(gs_m,
        VPD,
        seconds_m_max,
        levels= c(seconds_m_max=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
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
    cex.axis = 2.2)
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
        levels= c(seconds_b_max=400),
        lwd =9, 
        labcex = 15,
        drawlabels = TRUE,
        xlab = "",        
        ylab = "",        
        xlim = c(min(gs_b),max(gs_b)),
        ylim = c(min(VPD),max(VPD)),
        zlim = c(min(seconds_t),max(2500)),
        add=TRUE                 
)
######################################################################


