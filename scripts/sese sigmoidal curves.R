###########################################
# Alana's modification of                 #
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
fog <- as.data.frame(read.delim("clipboard"))


##########################################
# Analyze water potential (MPa) vs. time #
##########################################
#fitting gompertz curves

#Gompertz function = Asym*exp(-b2*b3^x)
#Asym 	a numeric parameter representing the asymptote.
#b2 	a numeric parameter related to the value of the function at x = 0
#b3 	a numeric parameter related to the scale of the x axis, should be less than 1.

T11_L_30$neg_MPa_f <- -(T11_L_30$MPa_f)

gomp_T11_L_30 <- nls(MPa_f ~ SSgompertz(Minutes, Asym, b2, b3), data = T11_L_30, start = list( b2 = max(T11_L_30$MPa_f), b3=-.4),  algorithm = "port") 

gomp_T11_L_30 <- nls(neg_MPa_f ~ SSgompertz(Minutes, Asym, b2, b3), data = T11_L_30, start = list(Asym = 0.000001, b2 = min(T11_L_30$neg_MPa_f),b3=.9 ), lower = c(0, 0, 0, 0), upper = c(1000, 1000, 1000, 1000), algorithm = "port") 

summary(gomp_T11_L_30)

#logistic, 3 parameters
tpl_T11_L_30 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

tpl_T11_L_30 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, start = list(A = max(T11_L_30$MPa_f), xmid = 100, scal = 40), algorithm = 'port')
summary(tpl_T11_L_30)

#four parameter logistic

fpl_T11_L_30 = nls(MPa_f ~ SSfpl(Minutes, A, B, xmid, scal), data = T11_L_30,  algorithm = "port")

summary(fpl_T11_L_30)

###############
# Fit 4 parameter logistic model(s) to sese surface data


########
#T11
names(fog)
#T1130L
m_fpl_T11_L_30 = nls(MPa_f ~ SSfpl(Minutes, A, B, xmid, scal), data = T11_L_30,  algorithm = "port")

# Summarize model output
summary(m_fpl_T11_L_30)

# Fit 3 parameter logistic model(s) to SESE surface data
m_tpl_T11_L_30 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30,  algorithm = 'port')

# Summarize model output


ggplot(T11_L_30, aes(Minutes,MPa_f)) + 
  geom_point()+
  geom_abline (aes(coef(tpl_T11_L_30)))
        


plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

# Plot output
plotFit(tpl_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(0,.3), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'orchid')
axis(1, at = c(0,50,100,150), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, .1, .2), cex.axis = 1.5, labels = TRUE)
mtext(expression(paste('-',Psi, ' [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)

#T1150L
m_fpl_T11_L_50 = nls(MPa_f ~ SSfpl(Minutes, A, B, xmid, scal), data = T11_L_50, start = list(A = max(T11_L_50$MPa_f), B = 0, xmid = 100, scal = 40), lower = c(0, 0, 0, 0), upper = c(1000, 1000, 1e5, 10000), algorithm = "port")

# Summarize model output
summary(m_fpl_T11_L_50)



# Define plotting parameters
par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

# Plot output
plotFit(m_fpl_T11_L_50, interval = 'confidence', data = d_Pr_surf, xlim = c(0,450), ylim = c(0,2.5), shade = TRUE, main = titles[i], cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', col.conf = 'grey90')
#abline(v = coef(summary(m_fpl_T11_L_50))[3,1], lty = 2)
axis(1, at = c(0,150,300,450), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, 1.25, 2.5), cex.axis = 1.5, labels = TRUE)
mtext(expression(paste('-',Psi, ' [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 50 L'), side = 3, cex = 1.1, line = 1)


#########################################
# Analyze change in g water vs. time #
#########################################

# Fit 3 parameter logistic model(s) to SESE surface data
m2_tpl_T11_L_30 = nls(weight_perA ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, start = list(A = max(T11_L_30$weight_perA), xmid = 100, scal = 40), algorithm = 'port')

# Summarize model output
summary(m2_tpl_T11_L_30)


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

