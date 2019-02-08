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


summary(tpl_T11_L_30)

#four parameter logistic

fpl_T11_L_30 = nls(MPa_f ~ SSfpl(Minutes, A, B, xmid, scal), data = T11_L_30,  algorithm = "port")

summary(fpl_T11_L_30)

###############
# Fit 3 parameter logistic models to sese surface data
#logistic equation in the form of: A/(1+exp((xmid-x)/scal), where A=asymptote, xmid=inflection point

########
#T11

#T1130L
tpl_T11_L_30 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

# Summarize model output
summary(tpl_T11_L_30)


#confidence interval and se for given point in the model, y0 must be an observed value
invest(tpl_T11_L_30,  y0 = max(T11_L_30$MPa_f) ,interval = c("Wald"), level = 0.95, mean.response = TRUE)

# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(tpl_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(0,.3), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1.5, labels = FALSE)
axis(2, at = c(0, .1, .2), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-',Psi, ' [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)

#T1150L





#########################################
# Analyze change in g water vs. time #
#########################################
#T1130L
g_tpl_T11_L_30 = nls(weight_perA ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

# Summarize model output
summary(g_tpl_T11_L_30)


######################################
# Calculate instantaneous resistance #
######################################
#T11_L_30
# Time interval for prediction
d_Time = seq(0,150,1)

# Estimate water potential over time
WP = predict(tpl_T11_L_30, newdata = list(Minutes = d_Time))

# Estimate change in mass over time
dM_dt = diff(predict(g_tpl_T11_L_30, newdata = list(Minutes = d_Time)))

# Calculate instantaneous conductance
K_T11_30_L = dM_dt/WP[-1]/60
R_T11_30_L = 1/K_T11_30_L


par(mfrow=c(1,2))
#Plot K_surf for T11_30_L
plot(K_T11_30_L, type = 'l', ylim = c(0,0.03), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',col = "dark red")
axis(1, at = c(0,50,100,150), cex.axis = 1)
axis(2, at = c(0, .01,0.02, 0.03), cex.axis = 1, labels = TRUE)
mtext(expression(paste(' [g ', m^-2, ' ', s^-1, ' ', MPa^-1, ']')), side = 2, cex = 1.1, line = 3)
mtext(expression(paste('rehydration time [mins]')), side = 1, cex = 1.2, line = 1, outer = TRUE)
mtext(expression(paste('T11_30_L[surf] conductance')), side = 3, cex = 1.2, line = 1, outer = TRUE)

plot(R_T11_30_L, type = 'l', ylim = c(0,60000), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n',  xlab = '', ylab = '')
axis(1, at = c(0,50,100,150), cex.axis = 1)
axis(2, at = c(0, 20000,40000, 60000), cex.axis = 1, labels = TRUE)
mtext(expression(paste('rehydration time [mins]')), side = 1, cex = 1.2, line = 1, outer = TRUE)
mtext(expression(paste('T11_30_L  resistance')), side = 3, cex = 1.2, line = 1)

plot(R_T11_30_L)


# Close active plotting device
dev.off()

