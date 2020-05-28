###########################################
# Mason's version of                 #
#"Foliar Rehydration Kinetics" script     #
#                                         #
# Fit three and four parameter logistic   #
#  models to foliar rehydration data      #
#  and calculate instantaneous resistance #
#                                         #
###########################################


##### Load libraries #####
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(investr)

##### Load data #####
d_Pr_surf = read.table('Prunus 9 June.txt', header = TRUE)
d_Qu_surf = read.table('Valley oak surface 9 June.txt', header = TRUE)

# Set up plot export parameters
pdf('SurfaceUptakePlots.pdf', width = 6.5, height = 8.5)

##########################################
# Analyze water potential (psi) vs. time #
##########################################

# Fit logistic model(s) to prunus surface data
m_fpl_Pr_surf_WP = nls(WP ~ SSfpl(Time, A, B, xmid, scal), data = d_Pr_surf, start = list(A = max(d_Pr_surf$WP), B = 0, xmid = 100, scal = 40), lower = c(0, 0, 0, 0), upper = c(1000, 1000, 1e5, 10000), algorithm = "port")

# Summarize model output
summary(m_fpl_Pr_surf_WP)

# Define plotting parameters
par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

# Plot output
plotFit(m_fpl_Pr_surf_WP, interval = 'confidence', data = d_Pr_surf, xlim = c(0,450), ylim = c(0,2.5), shade = TRUE, main = titles[i], cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90')
#abline(v = coef(summary(m_fpl_Pr_surf_WP))[3,1], lty = 2)
axis(1, at = c(0,150,300,450), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, 1.25, 2.5), cex.axis = 1.5, labels = TRUE)
mtext(expression(paste('-',Psi, ' [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression(italic('P. dulcis')), side = 3, cex = 1.1, line = 1)

# Fit logistic model(s) to Quercus surface data
m_fpl_Qu_surf_WP = nls(WP ~ SSfpl(Time, A, B, xmid, scal), data = d_Qu_surf, start = list(A = max(d_Qu_surf$WP), B = 0, xmid = 100, scal = 40), lower = c(0, 0, 0, 0), upper = c(1000, 1000, 1e5, 10000), algorithm = "port")

# Summarize model output
summary(m_fpl_Qu_surf_WP)

# Plot output
plotFit(m_fpl_Qu_surf_WP, interval = 'confidence', data = d_Qu_surf, xlim = c(0,450), ylim = c(0,2.5), shade = TRUE, main = titles[i], cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90')
# abline(v = coef(summary(m_fpl_Qu_surf_WP))[3,1], lty = 2)
axis(1, at = c(0,150,300,450), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, 1.25, 2.5), cex.axis = 1.5, labels = FALSE)
mtext(expression(italic('Q. lobata')), side = 3, cex = 1.1, line = 1)


#########################################
# Analyze change in g water vs. time #
#########################################

# Fit logistic model(s) to Prunus surface data
m_tpl_Pr_surf = nls(M ~ SSlogis(Time, A, xmid, scal), data = d_Pr_surf, start = list(A = max(d_Pr_surf$M), xmid = 100, scal = 40), algorithm = 'port')

# Summarize model output
summary(m_tpl_Pr_surf)

# Plot output
plotFit(m_tpl_Pr_surf, interval = 'confidence', data = d_Pr_surf, shade = TRUE, xlim = c(0,450), ylim = c(0,20), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90')
axis(1, at = c(0,150,300,450), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, 10, 20), cex.axis = 1.5, labels = TRUE)
mtext(expression(paste(Delta, 'M [g ', m^-2, ']')), side = 2, cex = 1.1, line = 3)

# Fit logistic model(s) to Quercus surface mass data
m_tpl_Qu_surf = nls(M ~ SSlogis(Time, A, xmid, scal), data = d_Qu_surf, start = list(A = max(d_Qu_surf$M), xmid = 100, scal = 40), algorithm = 'port')


# Summarize model output
summary(m_tpl_Qu_surf)

# Plot output
plotFit(m_tpl_Qu_surf, interval = 'confidence', data = d_Qu_surf, shade = TRUE, xlim = c(0,450), ylim = c(0,20), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90')
axis(1, at = c(0,150,300,450), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, 10, 20), cex.axis = 1.5, labels = FALSE)


######################################
# Calculate instantaneous resistance #
######################################

# Time interval for prediction
d_Time = seq(0,450,1)

# Prunus 
# Estimate water potential over time
WP = predict(m_fpl_Pr_surf_WP, newdata = list(Time = d_Time))

# Estimate change in mass over time
dM_dt = diff(predict(m_tpl_Pr_surf, newdata = list(Time = d_Time)))

# Calculate instantaneous conductance
K_surf_Pr = dM_dt/WP[-1]/60
R_surf_Pr = 1/K_surf_Pr

# Quercus
# Estimate water potential over time
WP = predict(m_fpl_Qu_surf_WP, newdata = list(Time = d_Time))

# Estimate change in mass over time
dM_dt = diff(predict(m_tpl_Qu_surf, newdata = list(Time = d_Time)))

# Calculate instantaneous conductance
K_surf_Qu = dM_dt/WP[-1]/60
R_surf_Qu = 1/K_surf_Qu

# Plot K_surf for Prunus
plot(K_surf_Pr, type = 'l', ylim = c(0,0.002), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90', xlab = '', ylab = '')
axis(1, at = c(0,150,300,450), cex.axis = 1.5)
axis(2, at = c(0, 0.001, 0.002), cex.axis = 1.5, labels = TRUE)
mtext(expression(paste(K[surf], ' [g ', m^-2, ' ', s^-1, ' ', MPa^-1, ']')), side = 2, cex = 1.1, line = 3)

# Plot K_surf for Quercus
plot(K_surf_Qu, type = 'l', ylim = c(0,0.002), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90', xlab = '', ylab = '')
axis(1, at = c(0,150,300,450), cex.axis = 1.5)
axis(2, at = c(0, 0.001, 0.002), cex.axis = 1.5, labels = FALSE)

# Add axis label for time
mtext('rehydration time [mins]', side = 1, cex = 1.2, line = 1, outer = TRUE)

# Close active plotting device
dev.off()

