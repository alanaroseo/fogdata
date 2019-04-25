
###########################################
# Alana's modification of                 #
#"Foliar Rehydration Kinetics" script     #
#                                         #
#  Fit nonlinear                          #
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
library(AICcmodavg)
library(manipulate)

############################################
# Analyze change in g water/area  vs. time #
############################################


#####
#T11

#T11 30

start_T11_L_30 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_30$Minutes, T11_L_30$weight_perA)#plot observed data
    k <- kk; b0 <- b00; b1 <- b10
    curve(k*exp(-b1*x) + b0, add=TRUE)
    start_T11_L_30 <<- list(k=k, b0=b0, b1=b1)
  },
  kk=slider(-1, 1, step = 0.001,  initial = 0.03),
  b10=slider(-1, 1, step = 0.001, initial = -.03),
  b00=slider(-1, 1, step=0.001,initial= 0))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_30

# Model fit using the start values:
expfit_T11_L_30 <- nls(weight_perA ~ I(k*exp(-b1*Minutes) + b0), data= T11_L_30, start = start_T11_L_30)

summary(expfit_T11_L_30)


#roughly check fit using Efron's pseudo R2
pred <- predict(expfit_T11_L_30)
n <- length(pred)
res <- resid(expfit_T11_L_30)
w <- weights(expfit_T11_L_30)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(expfit_T11_L_30)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_expfit_T11_L_30 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_expfit_T11_L_30


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(expfit_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(-10,10), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1, labels = TRUE)
axis(2, at = c(-10, 0, 10), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_30 <- lm(weight_perA ~ Minutes, data = T11_L_30)

summary(lm_T11_L_30)

#compare to tpl
g_tpl_T11_L_30 = nls(weight_perA ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

anova(expfit_T11_L_30,lm_T11_L_30)
AICc(expfit_T11_L_30)
AICc(lm_T11_L_30)
AICc(g_tpl_T11_L_30)


#T11 50

start_T11_L_50 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_50$Minutes, T11_L_50$weight_perA)#plot observed data
    k <- kk; b0 <- b00; b1 <- b10
    curve(k*exp(-b1*x) + b0, add=TRUE)
    start_T11_L_50 <<- list(k=k, b0=b0, b1=b1)
  },
  kk=slider(-1, 1, step = 0.001,  initial = 0.03),
  b10=slider(-1, 1, step = 0.001, initial = -.03),
  b00=slider(-1, 1, step=0.001,initial= 0))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_50

# Model fit using the start values:
expfit_T11_L_50 <- nls(weight_perA ~ I(k*exp(-b1*Minutes) + b0), data= T11_L_50, start = start_T11_L_50, control = list(maxiter = 500))

summary(expfit_T11_L_50)


#roughly check fit using Efron's pseudo R2
pred <- predict(expfit_T11_L_50)
n <- length(pred)
res <- resid(expfit_T11_L_50)
w <- weights(expfit_T11_L_50)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(expfit_T11_L_50)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_expfit_T11_L_50 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_expfit_T11_L_50


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plot(expfit_T11_L_50)

plotFit(expfit_T11_L_50, interval = 'confidence', data = T11_L_50, xlim = c(0,max(T11_L_50$weight_perA)), ylim = c(1-min(T11_L_50$weight_perA), 0,1+max(T11_L_50$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T11_L_50$Minutes),max(T11_L_50$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 50 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_50 <- lm(weight_perA ~ Minutes, data = T11_L_50)

summary(lm_T11_L_50)

anova(expfit_T11_L_50,lm_T11_L_50)
AICc(expfit_T11_L_50)
AICc(lm_T11_L_50)


#T11 90

start_T11_L_90 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_90$Minutes, T11_L_90$weight_perA)#plot observed data
    k <- kk; b0 <- b00; b1 <- b10
    curve(k*exp(-b1*x) + b0, add=TRUE)
    start_T11_L_90 <<- list(k=k, b0=b0, b1=b1)
  },
  kk=slider(-1, 1, step = 0.001,  initial = 0.03),
  b10=slider(-1, 1, step = 0.001, initial = -.03),
  b00=slider(-1, 1, step=0.001,initial= 0))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_90

# Model fit using the start values:
expfit_T11_L_90 <- nls(weight_perA ~ I(k*exp(-b1*Minutes) + b0), data= T11_L_90, start = start_T11_L_90, control = list(maxiter = 500))

summary(expfit_T11_L_90)



#roughly check fit using Efron's pseudo R2
pred <- predict(expfit_T11_L_90)
n <- length(pred)
res <- resid(expfit_T11_L_90)
w <- weights(expfit_T11_L_90)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(expfit_T11_L_90)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_expfit_T11_L_90 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_expfit_T11_L_90


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(expfit_T11_L_90, interval = 'confidence', data = T11_L_90, xlim = c(0,max(T11_L_90$weight_perA)), ylim = c(-10, 1+max(T11_L_90$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T11_L_90$Minutes),max(T11_L_90$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 90 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_90 <- lm(weight_perA ~ Minutes, data = T11_L_90)

summary(lm_T11_L_90)

anova(expfit_T11_L_90,lm_T11_L_90)
AICc(expfit_T11_L_90)
AICc(lm_T11_L_90)

#####
#T16

#T16 37.5


start_T16_L_37 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T16_L_37$Minutes, T16_L_37$weight_perA)#plot observed data
    k <- kk; b0 <- b00; b1 <- b10
    curve(k*exp(-b1*x) + b0, add=TRUE)
    start_T16_L_37 <<- list(k=k, b0=b0, b1=b1)
  },
  kk=slider(-1, 1, step = 0.001,  initial = 0.03),
  b10=slider(-1, 1, step = 0.001, initial = -.03),
  b00=slider(-1, 1, step=0.001,initial= 0))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T16_L_37

# Model fit using the start values:
expfit_T16_L_37 <- nls(weight_perA ~ I(k*exp(-b1*Minutes) + b0), data= T16_L_37, start = start_T16_L_37, control = list(maxiter = 1000, minFactor = .0000009))

summary(expfit_T16_L_37)



#roughly check fit using Efron's pseudo R2
pred <- predict(expfit_T16_L_37)
n <- length(pred)
res <- resid(expfit_T16_L_37)
w <- weights(expfit_T16_L_37)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(expfit_T16_L_37)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_expfit_T16_L_37 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_expfit_T16_L_37


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(expfit_T16_L_37, interval = 'confidence', data = T16_L_37, xlim = c(0,max(T16_L_37$weight_perA)), ylim = c(-10, 1+max(T16_L_37$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T16_L_37$Minutes),max(T16_L_37$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T16 37 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T16_L_37 <- lm(weight_perA ~ Minutes, data = T16_L_37)

summary(lm_T16_L_37)

anova(expfit_T16_L_37,lm_T16_L_37)
AICc(expfit_T16_L_37)
AICc(lm_T16_L_37)