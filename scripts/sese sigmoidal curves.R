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
######Trying different curves for MPa
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

######## Having settled on a tpl:
# Fit 3 parameter logistic models to sese surface data
#logistic equation in the form of: A/(1+exp((xmid-x)/scal), where A=asymptote, xmid=inflection point

########
#T11

#T1130L
tpl_T11_L_30 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

# Summarize model output
summary(tpl_T11_L_30)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T11_L_30)
n <- length(pred)
res <- resid(tpl_T11_L_30)
w <- weights(tpl_T11_L_30)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T11_L_30)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T11_L_30 <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
R2_tpl_T11_L_30


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(tpl_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(0,.3), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .1, .2), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)


#confidence interval and se for given point in the model, y0 must be an observed value
invest(tpl_T11_L_30,  y0 = max(T11_L_30$MPa_f) ,interval = c("Wald"), level = 0.95, mean.response = TRUE)


#T1150L
tpl_T11_L_50 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_50, algorithm = 'port')

# Summarize model output
summary(tpl_T11_L_50)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T11_L_50)
n <- length(pred)
res <- resid(tpl_T11_L_50)
w <- weights(tpl_T11_L_50)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T11_L_50)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T11_L_50 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T11_L_50


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T11_L_50, interval = 'confidence', data = T11_L_50, xlim = c(0,max(T11_L_50$Minutes)), ylim = c(0,.1+max(T11_L_50$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .3, .6), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 50 L'), side = 3, cex = 1.1, line = 1)

#T1190L
tpl_T11_L_90 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_90, algorithm = 'port')

# Summarize model output
summary(tpl_T11_L_90)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T11_L_90)
n <- length(pred)
res <- resid(tpl_T11_L_90)
w <- weights(tpl_T11_L_90)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T11_L_90)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T11_L_90 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T11_L_90


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T11_L_90, interval = 'confidence', data = T11_L_90, xlim = c(0,max(T11_L_90$Minutes)), ylim = c(0,.1+max(T11_L_90$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T11_L_90$Minutes),max(T11_L_90$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .3,.6, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 90 L'), side = 3, cex = 1.1, line = 1)

##### 
#T6

#T6_L_20
tpl_T6_L_20 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T6_L_20, algorithm = 'port')

# Summarize model output
summary(tpl_T6_L_20)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T6_L_20)
n <- length(pred)
res <- resid(tpl_T6_L_20)
w <- weights(tpl_T6_L_20)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T6_L_20)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T6_L_20 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T6_L_20


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T6_L_20, interval = 'confidence', data = T6_L_20, xlim = c(0,max(T6_L_20$Minutes)), ylim = c(0,.1+max(T6_L_20$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_20$Minutes),max(T6_L_20$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 20 L'), side = 3, cex = 1.1, line = 1)


#T6_L_45
tpl_T6_L_45 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T6_L_45, algorithm = 'port')

# Summarize model output
summary(tpl_T6_L_45)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T6_L_45)
n <- length(pred)
res <- resid(tpl_T6_L_45)
w <- weights(tpl_T6_L_45)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T6_L_45)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T6_L_45 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T6_L_45


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T6_L_45, interval = 'confidence', data = T6_L_45, xlim = c(0,max(T6_L_45$Minutes)), ylim = c(0,.1+max(T6_L_45$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_45$Minutes),max(T6_L_45$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 45 L'), side = 3, cex = 1.1, line = 1)

#T6_L_91
tpl_T6_L_91 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T6_L_91, algorithm = 'port')

# Summarize model output
summary(tpl_T6_L_91)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T6_L_91)
n <- length(pred)
res <- resid(tpl_T6_L_91)
w <- weights(tpl_T6_L_91)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T6_L_91)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T6_L_91 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T6_L_91


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T6_L_91, interval = 'confidence', data = T6_L_91, xlim = c(0,max(T6_L_91$Minutes)), ylim = c(0,.1+max(T6_L_91$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_91$Minutes),max(T6_L_91$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 91 L'), side = 3, cex = 1.1, line = 1)

#####
#T8

#T8_L_50
tpl_T8_L_50 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T8_L_50, algorithm = 'port')

# Summarize model output
summary(tpl_T8_L_50)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T8_L_50)
n <- length(pred)
res <- resid(tpl_T8_L_50)
w <- weights(tpl_T8_L_50)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T8_L_50)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T8_L_50 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T8_L_50


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T8_L_50, interval = 'confidence', data = T8_L_50, xlim = c(0,max(T8_L_50$Minutes)), ylim = c(0,.1+max(T8_L_50$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_50$Minutes),max(T8_L_50$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 50 L'), side = 3, cex = 1.1, line = 1)


#T8_L_66
tpl_T8_L_66 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T8_L_66, algorithm = 'port')

# Summarize model output
summary(tpl_T8_L_66)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T8_L_66)
n <- length(pred)
res <- resid(tpl_T8_L_66)
w <- weights(tpl_T8_L_66)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T8_L_66)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T8_L_66 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T8_L_66


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T8_L_66, interval = 'confidence', data = T8_L_66, xlim = c(0,max(T8_L_66$Minutes)), ylim = c(0,.1+max(T8_L_66$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_66$Minutes),max(T8_L_66$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 66 L'), side = 3, cex = 1.1, line = 1)


#T8_L_97
tpl_T8_L_97 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T8_L_97, algorithm = 'port')

# Summarize model output
summary(tpl_T8_L_97)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T8_L_97)
n <- length(pred)
res <- resid(tpl_T8_L_97)
w <- weights(tpl_T8_L_97)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T8_L_97)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T8_L_97 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T8_L_97


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T8_L_97, interval = 'confidence', data = T8_L_97, xlim = c(0,max(T8_L_97$Minutes)), ylim = c(0,.1+max(T8_L_97$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_97$Minutes),max(T8_L_97$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 97 L'), side = 3, cex = 1.1, line = 1)

#####
#T16


#T16_L_51
tpl_T16_L_51 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T16_L_51, algorithm = 'port')

# Summarize model output
summary(tpl_T16_L_51)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T16_L_51)
n <- length(pred)
res <- resid(tpl_T16_L_51)
w <- weights(tpl_T16_L_51)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T16_L_51)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T16_L_51 <- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)
R2_tpl_T16_L_51


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T16_L_51, interval = 'confidence', data = T16_L_51, xlim = c(0,max(T16_L_51$Minutes)), ylim = c(0,.1+max(T16_L_51$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T16_L_51$Minutes),max(T16_L_51$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T16 51 L'), side = 3, cex = 1.1, line = 1)


#T16_L_37
tpl_T16_L_37 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T16_L_37, algorithm = 'port')

# Summarize model output
summary(tpl_T16_L_37)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T16_L_37)
n <- length(pred)
res <- resid(tpl_T16_L_37)
w <- weights(tpl_T16_L_37)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T16_L_37)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T16_L_37 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T16_L_37


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T16_L_37, interval = 'confidence', data = T16_L_37, xlim = c(0,max(T16_L_37$Minutes)), ylim = c(0,.1+max(T16_L_37$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T16_L_37$Minutes),max(T16_L_37$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T16 37 L'), side = 3, cex = 1.1, line = 1)

#####
#T34

#T34_L_22
tpl_T34_L_22 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T34_L_22, algorithm = 'port')

# Summarize model output
summary(tpl_T34_L_22)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T34_L_22)
n <- length(pred)
res <- resid(tpl_T34_L_22)
w <- weights(tpl_T34_L_22)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T34_L_22)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T34_L_22 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T34_L_22


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T34_L_22, interval = 'confidence', data = T34_L_22, xlim = c(0,max(T34_L_22$Minutes)), ylim = c(0,.1+max(T34_L_22$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_22$Minutes),max(T34_L_22$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 22 L'), side = 3, cex = 1.1, line = 1)

#T34_L_56
tpl_T34_L_56 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T34_L_56, algorithm = 'port')

# Summarize model output
summary(tpl_T34_L_56)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T34_L_56)
n <- length(pred)
res <- resid(tpl_T34_L_56)
w <- weights(tpl_T34_L_56)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T34_L_56)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T34_L_56 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T34_L_56


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T34_L_56, interval = 'confidence', data = T34_L_56, xlim = c(0,max(T34_L_56$Minutes)), ylim = c(0,.1+max(T34_L_56$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_56$Minutes),max(T34_L_56$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 56 L'), side = 3, cex = 1.1, line = 1)


#T34_L_80
tpl_T34_L_80 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T34_L_80, algorithm = 'port')

# Summarize model output
summary(tpl_T34_L_80)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T34_L_80)
n <- length(pred)
res <- resid(tpl_T34_L_80)
w <- weights(tpl_T34_L_80)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T34_L_80)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T34_L_80 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T34_L_80


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T34_L_80, interval = 'confidence', data = T34_L_80, xlim = c(0,max(T34_L_80$Minutes)), ylim = c(0,.1+max(T34_L_80$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_80$Minutes),max(T34_L_80$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 80 L'), side = 3, cex = 1.1, line = 1)


#T34_L_102
tpl_T34_L_102 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T34_L_102, algorithm = 'port')

# Summarize model output
summary(tpl_T34_L_102)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T34_L_102)
n <- length(pred)
res <- resid(tpl_T34_L_102)
w <- weights(tpl_T34_L_102)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T34_L_102)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T34_L_102 <- list(pseudo.R.squared = r.sq,
                        adj.R.squared = adj.r.sq)
R2_tpl_T34_L_102


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T34_L_102, interval = 'confidence', data = T34_L_102, xlim = c(0,max(T34_L_102$Minutes)), ylim = c(0,.1+max(T34_L_102$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_102$Minutes),max(T34_L_102$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 102 L'), side = 3, cex = 1.1, line = 1)


#####
#T48

#T48_L_60
tpl_T48_L_60 = nls(MPa_f ~ SSlogis(Minutes, A, xmid, scal), data = T48_L_60, algorithm = 'port')

# Summarize model output
summary(tpl_T48_L_60)

#roughly check fit using Efron's pseudo R2
pred <- predict(tpl_T48_L_60)
n <- length(pred)
res <- resid(tpl_T48_L_60)
w <- weights(tpl_T48_L_60)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(tpl_T48_L_60)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_tpl_T48_L_60 <- list(pseudo.R.squared = r.sq,
                         adj.R.squared = adj.r.sq)
R2_tpl_T48_L_60


# Plot model output
plot.new()
par(mfrow=c(1,1))

plotFit(tpl_T48_L_60, interval = 'confidence', data = T48_L_60, xlim = c(0,max(T48_L_60$Minutes)), ylim = c(0,.1+max(T48_L_60$MPa_f)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T48_L_60$Minutes),max(T48_L_60$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, .2,.4,.6,.8, 1), cex.axis = 1, labels = TRUE)
mtext(expression(paste('-', Psi, '  [MPa]')), side = 2, cex = 1.1, line = 3)
mtext(expression('T48 60 L'), side = 3, cex = 1.1, line = 1)


#########################################
# Analyze change in g water vs. time #
#########################################
##Fit curves
#exponential 
exp_T11_L_30 = nls(weight_perA  ~ exp(Minutes), data = T11_L_30, start = c( weight_perA  = min(T11_L_30$weight_perA ), Minutes=0))

summary(exp_T11_L_30)

#T1130L
g_tpl_T11_L_30 = nls(weight_perA ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

# Summarize model output
summary(g_tpl_T11_L_30)

# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(g_tpl_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(-5,10), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1, labels = TRUE)
axis(2, at = c(-5, 4, 9), cex.axis = 1, labels = TRUE)
mtext(expression(paste( Delta, '  g', ' water' ,'/','m^2' )), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)




######################################
# Calculate instantaneous resistance #
######################################

#coef()[1]=A, coef()[2]=xmid, coef()[3]=scal
#form = coef()[1]/(1+exp((coef()[2]-x)/coef()[3])
#T11_L_30

#library(Deriv)
#fTll_30_L<- function(x){ coef(g_tpl_T11_L_30)[1]/(1+exp((coef(g_tpl_T11_L_30)[2]-x)/coef(g_tpl_T11_L_30)[3]))}
                  
#d_fTll_30_L <- Deriv(fTll_30_L, x=T11_L_30$weight_perA)
#plot(fTll_30_L(T11_L_30$weight_perA))

x <- T11_L_30$weight_perA
A <- coef(g_tpl_T11_L_30)[1]
xmid <- coef(g_tpl_T11_L_30)[2]
scal <- coef(g_tpl_T11_L_30)[3]
d_fT11_30_L <- Deriv(A/(1+exp((xmid-x)/scal)),x)

plot(d_fT11_30_L)

# Time interval for prediction
d_Time = seq(0,150,1)

# Estimate water potential over time
WP = predict(tpl_T11_L_30, newdata = list(Minutes = d_Time))


# Estimate change in mass over time
dM_dt = diff(predict(g_tpl_T11_L_30, newdata = list(Minutes = d_Time)))

#maximum uptake rate (flux):
max(dM_dt)



# Calculate instantaneous conductance
K_T11_30_L = dM_dt/WP[-1]/60
R_T11_30_L = 1/K_T11_30_L


par(mfrow=c(1,2))
#Plot K_surf for T11_30_L
plot(K_T11_30_L, type = 'l', ylim = c(0,0.03), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',col = "purple")
axis(1, at = c(0,50,100,150), cex.axis = 1)
axis(2, at = c(0, .01,0.02, 0.03), cex.axis = 1, labels = TRUE)
mtext(expression(paste(' [g ', m^-2, ' ', s^-1, ' ', MPa^-1, ']')), side = 2, cex = 1.1, line = 3)
mtext(expression(paste('rehydration time [mins]')), side = 1, cex = 1.2, line = 1, outer = TRUE)
mtext(expression(paste('T11_30_L[surf] conductance')), side = 3, cex = 1.2, line = 1, outer = TRUE)

plot(R_T11_30_L, type = 'l', ylim = c(0,60000), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, xaxt = 'n', yaxt = 'n',  xlab = '', ylab = '', col = "purple")
axis(1, at = c(0,50,100,150), cex.axis = 1)
axis(2, at = c(0, 20000,40000, 60000), cex.axis = 1, labels = TRUE)
mtext(expression(paste('rehydration time [mins]')), side = 1, cex = 1.2, line = 1, outer = TRUE)
mtext(expression(paste('T11_30_L  resistance')), side = 3, cex = 1.2, line = 1)

plot(R_T11_30_L)


# Close active plotting device
dev.off()

