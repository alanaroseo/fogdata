###########################################
# Alana's script for ln models            #
#                                         #
#  Fit nls                                #
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


# ln models: y = m*ln(x) - b ---------------------------------------------------------------

# T11 ---------------------------------------------------------------------


#T11 30

start_T11_L_30 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_30$Minutes, T11_L_30$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T11_L_30 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_30

# Model fit using the start values:
lnfit_T11_L_30 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T11_L_30, start = start_T11_L_30)

summary(lnfit_T11_L_30)

#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T11_L_30)
n <- length(pred)
res <- resid(lnfit_T11_L_30)
w <- weights(lnfit_T11_L_30)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T11_L_30)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T11_L_30 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T11_L_30


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T11_L_30, interval = 'confidence', data = T11_L_30, xlim = c(0,150), ylim = c(-10,10), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,50,100,150), cex.axis = 1, labels = TRUE)
axis(2, at = c(-10, 0, 10), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_30 <- lm(weight_perA ~ Minutes, data = T11_L_30)

summary(lm_T11_L_30)

#compare to tpl
g_tpl_T11_L_30 = nls(weight_perA ~ SSlogis(Minutes, A, xmid, scal), data = T11_L_30, algorithm = 'port')

anova(lnfit_T11_L_30,lm_T11_L_30)
AICc(lnfit_T11_L_30)
AICc(lm_T11_L_30)
AICc(g_tpl_T11_L_30)


#T11 50

start_T11_L_50 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_50$Minutes, T11_L_50$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T11_L_50 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))
# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_50

# Model fit using the start values:
lnfit_T11_L_50 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T11_L_50, start = start_T11_L_50, control = list(maxiter = 500))

summary(lnfit_T11_L_50)


#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T11_L_50)
n <- length(pred)
res <- resid(lnfit_T11_L_50)
w <- weights(lnfit_T11_L_50)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T11_L_50)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T11_L_50 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T11_L_50


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plot(lnfit_T11_L_50)

plotFit(lnfit_T11_L_50, interval = 'confidence', data = T11_L_50, xlim = c(0,max(T11_L_50$Minutes)), ylim = c(1-min(T11_L_50$weight_perA), 0,1+max(T11_L_50$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T11_L_50$Minutes),max(T11_L_50$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 50 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_50 <- lm(weight_perA ~ Minutes, data = T11_L_50)

summary(lm_T11_L_50)

anova(lnfit_T11_L_50,lm_T11_L_50)
AICc(lnfit_T11_L_50)
AICc(lm_T11_L_50)


#T11 90

start_T11_L_90 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_90$Minutes, T11_L_90$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T11_L_90 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T11_L_90

# Model fit using the start values:
lnfit_T11_L_90 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T11_L_90, start = start_T11_L_90, control = list(maxiter = 500))

summary(lnfit_T11_L_90)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T11_L_90)
n <- length(pred)
res <- resid(lnfit_T11_L_90)
w <- weights(lnfit_T11_L_90)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T11_L_90)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T11_L_90 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T11_L_90


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T11_L_90, interval = 'confidence', data = T11_L_90, xlim = c(0,max(T11_L_90$Minutes)), ylim = c(-10, 1+max(T11_L_90$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T11_L_90$Minutes),max(T11_L_90$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 90 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T11_L_90 <- lm(weight_perA ~ Minutes, data = T11_L_90)

summary(lm_T11_L_90)

anova(lnfit_T11_L_90,lm_T11_L_90)
AICc(lnfit_T11_L_90)
AICc(lm_T11_L_90)
#T11

#T16

# T16 ---------------------------------------------------------------------


#T16 37.5


start_T16_L_37 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T16_L_37$Minutes, T16_L_37$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T16_L_37 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T16_L_37

# Model fit using the start values:
lnfit_T16_L_37 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T16_L_37, start = start_T16_L_37, control = list(maxiter = 5000, minFactor = .000000009))

summary(lnfit_T16_L_37)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T16_L_37)
n <- length(pred)
res <- resid(lnfit_T16_L_37)
w <- weights(lnfit_T16_L_37)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T16_L_37)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T16_L_37 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T16_L_37


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T16_L_37, interval = 'confidence', data = T16_L_37, xlim = c(0,max(T16_L_37$Minutes)), ylim = c(-10, 1+max(T16_L_37$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T16_L_37$Minutes),max(T16_L_37$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T16 37 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T16_L_37 <- lm(weight_perA ~ Minutes, data = T16_L_37)

summary(lm_T16_L_37)

anova(lnfit_T16_L_37,lm_T16_L_37)
AICc(lnfit_T16_L_37)
AICc(lm_T16_L_37)


#T16 51


start_T16_L_51 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T16_L_51$Minutes, T16_L_51$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T16_L_51 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T16_L_51

# Model fit using the start values:
lnfit_T16_L_51 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T16_L_51, start = start_T16_L_51, control = list(maxiter = 1000, minFactor = .000000009))

summary(lnfit_T16_L_51)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T16_L_51)
n <- length(pred)
res <- resid(lnfit_T16_L_51)
w <- weights(lnfit_T16_L_51)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T16_L_51)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T16_L_51 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T16_L_51


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T16_L_51, interval = 'confidence', data = T16_L_51, xlim = c(0,max(T16_L_51$Minutes)), ylim = c(-10, 1+max(T16_L_51$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T16_L_51$Minutes),max(T16_L_51$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T16 51 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T16_L_51 <- lm(weight_perA ~ Minutes, data = T16_L_51)

summary(lm_T16_L_51)

anova(lnfit_T16_L_51,lm_T16_L_51)
AICc(lnfit_T16_L_51)
AICc(lm_T16_L_51)



# T6 ----------------------------------------------------------------------


#T6 20

start_T6_L_20 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T6_L_20$Minutes, T6_L_20$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T6_L_20 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T6_L_20

# Model fit using the start values:
lnfit_T6_L_20 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T6_L_20, start = start_T6_L_20, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T6_L_20)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T6_L_20)
n <- length(pred)
res <- resid(lnfit_T6_L_20)
w <- weights(lnfit_T6_L_20)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T6_L_20)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T6_L_20 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T6_L_20


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T6_L_20, interval = 'confidence', data = T6_L_20, xlim = c(0,max(T6_L_20$Minutes)), ylim = c(-10, 1+max(T6_L_20$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_20$Minutes),max(T6_L_20$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 20 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T6_L_20 <- lm(weight_perA ~ Minutes, data = T6_L_20)

summary(lm_T6_L_20)

anova(lnfit_T6_L_20,lm_T6_L_20)
AICc(lnfit_T6_L_20)
AICc(lm_T6_L_20)

#T6 45

start_T6_L_45 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T6_L_45$Minutes, T6_L_45$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T6_L_45 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T6_L_45

# Model fit using the start values:
lnfit_T6_L_45 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T6_L_45, start = start_T6_L_45, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T6_L_45)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T6_L_45)
n <- length(pred)
res <- resid(lnfit_T6_L_45)
w <- weights(lnfit_T6_L_45)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T6_L_45)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T6_L_45 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T6_L_45


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T6_L_45, interval = 'confidence', data = T6_L_45, xlim = c(0,max(T6_L_45$Minutes)), ylim = c(-10, 1+max(T6_L_45$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_45$Minutes),max(T6_L_45$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 45 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T6_L_45 <- lm(weight_perA ~ Minutes, data = T6_L_45)

summary(lm_T6_L_45)

anova(lnfit_T6_L_45,lm_T6_L_45)
AICc(lnfit_T6_L_45)
AICc(lm_T6_L_45)


#T6 91

start_T6_L_91 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T6_L_91$Minutes, T6_L_91$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T6_L_91 <<- list(m=m, b=b)   
    },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T6_L_91

# Model fit using the start values:
lnfit_T6_L_91 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T6_L_91, start = start_T6_L_91, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T6_L_91)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T6_L_91)
n <- length(pred)
res <- resid(lnfit_T6_L_91)
w <- weights(lnfit_T6_L_91)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T6_L_91)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T6_L_91 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T6_L_91


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T6_L_91, interval = 'confidence', data = T6_L_91, xlim = c(0,max(T6_L_91$Minutes)), ylim = c(-10, 1+max(T6_L_91$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T6_L_91$Minutes),max(T6_L_91$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T6 91 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T6_L_91 <- lm(weight_perA ~ Minutes, data = T6_L_91)

summary(lm_T6_L_91)

anova(lnfit_T6_L_91,lm_T6_L_91)
AICc(lnfit_T6_L_91)
AICc(lm_T6_L_91)



#T8

# T8 ----------------------------------------------------------------------

#T8 50


start_T8_L_50 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T8_L_50$Minutes, T8_L_50$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T8_L_50 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T8_L_50

# Model fit using the start values:
lnfit_T8_L_50 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T8_L_50, start = start_T8_L_50, control = list(maxiter = 10000, minFactor = .00000000009))

summary(lnfit_T8_L_50)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T8_L_50)
n <- length(pred)
res <- resid(lnfit_T8_L_50)
w <- weights(lnfit_T8_L_50)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T8_L_50)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T8_L_50 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T8_L_50


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T8_L_50, interval = 'confidence', data = T8_L_50, xlim = c(0,max(T8_L_50$Minutes)), ylim = c(-10, 1+max(T8_L_50$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_50$Minutes),max(T8_L_50$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 50 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T8_L_50 <- lm(weight_perA ~ Minutes, data = T8_L_50)

summary(lm_T8_L_50)

plot(lm_T8_L_50)

anova(lnfit_T8_L_50,lm_T8_L_50)
AICc(lnfit_T8_L_50)
AICc(lm_T8_L_50)



#T8 66


start_T8_L_66 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T8_L_66$Minutes, T8_L_66$weight_perA)#plot observed data
    m <- mm; b <- b0;      curve(m*log(x) + b, add=TRUE)
    start_T8_L_66 <<- list(m=m, b=b)   },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T8_L_66

# Model fit using the start values:
lnfit_T8_L_66 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T8_L_66, start = start_T8_L_66, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T8_L_66)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T8_L_66)
n <- length(pred)
res <- resid(lnfit_T8_L_66)
w <- weights(lnfit_T8_L_66)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T8_L_66)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T8_L_66 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T8_L_66


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T8_L_66, interval = 'confidence', data = T8_L_66, xlim = c(0,max(T8_L_66$Minutes)), ylim = c(-10, 1+max(T8_L_66$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_66$Minutes),max(T8_L_66$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 66 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T8_L_66 <- lm(weight_perA ~ Minutes, data = T8_L_66)

summary(lm_T8_L_66)

plot(lm_T8_L_66)

anova(lnfit_T8_L_66,lm_T8_L_66)
AICc(lnfit_T8_L_66)
AICc(lm_T8_L_66)


#T8 97


start_T8_L_97 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T8_L_97$Minutes, T8_L_97$weight_perA)#plot observed data
    m <- mm; b <- b0;      
    curve(m*log(x) + b, add=TRUE)
    start_T8_L_97<<- list(m=m, b=b)   
    },   mm=slider(-1, 10, step = 0.001,  initial = 0.03),   b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T8_L_97

# Model fit using the start values:
lnfit_T8_L_97 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T8_L_97, start = start_T8_L_97, control = list(maxiter = 10000, minFactor = .0000009))

summary(lnfit_T8_L_97)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T8_L_97)
n <- length(pred)
res <- resid(lnfit_T8_L_97)
w <- weights(lnfit_T8_L_97)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T8_L_97)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T8_L_97 <- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_lnfit_T8_L_97


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T8_L_97, interval = 'confidence', data = T8_L_97, xlim = c(0,max(T8_L_97$Minutes)), ylim = c(-10, 1+max(T8_L_97$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T8_L_97$Minutes),max(T8_L_97$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T8 97 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T8_L_97 <- lm(weight_perA ~ Minutes, data = T8_L_97)

summary(lm_T8_L_97)

plot(lm_T8_L_97)

anova(lnfit_T8_L_97,lm_T8_L_97)
AICc(lnfit_T8_L_97)
AICc(lm_T8_L_97)



#T34

# T34 ---------------------------------------------------------------------


#T34 56

start_T34_L_56 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T34_L_56$Minutes, T34_L_56$weight_perA)#plot observed data
    m <- mm; b <- b0; 
    curve(m*log(x) + b, add=TRUE)
    start_T34_L_56 <<- list(m=m, b=b)
  },
  mm=slider(-1, 10, step = 0.001,  initial = 0.03),
  b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T34_L_56

# Model fit using the start

lnfit_T34_L_56 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T34_L_56, start = start_T34_L_56, control = list(maxiter = 10000, minFactor = .000000009))

summary(lnfit_T34_L_56)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T34_L_56)
n <- length(pred)
res <- resid(lnfit_T34_L_56)
w <- weights(lnfit_T34_L_56)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T34_L_56)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T34_L_56 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T34_L_56


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T34_L_56, interval = 'confidence', data = T34_L_56, xlim = c(0,max(T34_L_56$Minutes)), ylim = c(-10, 1+max(T34_L_56$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_56$Minutes),max(T34_L_56$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 56 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T34_L_56 <- lm(weight_perA ~ Minutes, data = T34_L_56)

summary(lm_T34_L_56)

plot(lm_T34_L_56)

anova(lnfit_T34_L_56,lm_T34_L_56)
AICc(lnfit_T34_L_56)
AICc(lm_T34_L_56)



#T34 22

start_T34_L_22 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T34_L_22$Minutes, T34_L_22$weight_perA)#plot observed data
    m <- mm; b <- b0; 
    curve(m*log(x) + b, add=TRUE)
    start_T34_L_22 <<- list(m=m, b=b)   
    },   
  mm=slider(-1, 10, step = 0.001,  initial = 0.03),   
  b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T34_L_22

# Model fit using the start values:
lnfit_T34_L_22 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T34_L_22, start = start_T34_L_22, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T34_L_22)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T34_L_22)
n <- length(pred)
res <- resid(lnfit_T34_L_22)
w <- weights(lnfit_T34_L_22)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T34_L_22)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T34_L_22 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T34_L_22


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T34_L_22, interval = 'confidence', data = T34_L_22, xlim = c(0,max(T34_L_22$Minutes)), ylim = c(-10, 1+max(T34_L_22$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_22$Minutes),max(T34_L_22$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 22 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T34_L_22 <- lm(weight_perA ~ Minutes, data = T34_L_22)

summary(lm_T34_L_22)

plot(lm_T34_L_22)

anova(lnfit_T34_L_22,lm_T34_L_22)
AICc(lnfit_T34_L_22)
AICc(lm_T34_L_22)


#T34 80

start_T34_L_80 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T34_L_80$Minutes, T34_L_80$weight_perA)#plot observed data
    m <- mm; b <- b0;      
    curve(m*log(x) + b, add=TRUE)
    start_T34_L_80 <<- list(m=m, b=b)   
    },   
  mm=slider(-1, 10, step = 0.001,  initial = 0.03),   
  b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T34_L_80

# Model fit using the start values:
lnfit_T34_L_80 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T34_L_80, start = start_T34_L_80, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T34_L_80)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T34_L_80)
n <- length(pred)
res <- resid(lnfit_T34_L_80)
w <- weights(lnfit_T34_L_80)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T34_L_80)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T34_L_80 <- list(pseudo.R.squared = r.sq,
                           adj.R.squared = adj.r.sq)

R2_lnfit_T34_L_80


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T34_L_80, interval = 'confidence', data = T34_L_80, xlim = c(0,max(T34_L_80$Minutes)), ylim = c(-10, 1+max(T34_L_80$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_80$Minutes),max(T34_L_80$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 80 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T34_L_80 <- lm(weight_perA ~ Minutes, data = T34_L_80)

summary(lm_T34_L_80)

plot(lm_T34_L_80)

anova(lnfit_T34_L_80,lm_T34_L_80)
AICc(lnfit_T34_L_80)
AICc(lm_T34_L_80)


#T34 102

start_T34_L_102 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T34_L_102$Minutes, T34_L_102$weight_perA)#plot observed data
    m <- mm; b <- b0; 
    curve(m*log(x) + b, add=TRUE)
    start_T34_L_102 <<- list(m=m, b=b)   },   
  mm=slider(-1, 10, step = 0.001,  initial = 0.03),   
  b0=slider(-30, 1, step = 0.001, initial = -.03))

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T34_L_102

# Model fit using the start values:
lnfit_T34_L_102 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T34_L_102, start = start_T34_L_102, control = list(maxiter = 1000, minFactor = .0000009))

summary(lnfit_T34_L_102)



#roughly check fit using Efron's pseudo R2
pred <- predict(lnfit_T34_L_102)
n <- length(pred)
res <- resid(lnfit_T34_L_102)
w <- weights(lnfit_T34_L_102)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(lnfit_T34_L_102)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_lnfit_T34_L_102 <- list(pseudo.R.squared = r.sq,
                            adj.R.squared = adj.r.sq)

R2_lnfit_T34_L_102


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plotFit(lnfit_T34_L_102, interval = 'confidence', data = T34_L_102, xlim = c(0,max(T34_L_102$Minutes)), ylim = c(-10, 1+max(T34_L_102$weight_perA)), cex = 1.25, cex.axis = 1.5, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.5*max(T34_L_102$Minutes),max(T34_L_102$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T34 102 L'), side = 3, cex = 1.1, line = 1)

#compare to lm
lm_T34_L_102 <- lm(weight_perA ~ Minutes, data = T34_L_102)

summary(lm_T34_L_102)

plot(lm_T34_L_102)

anova(lnfit_T34_L_102,lm_T34_L_102)
AICc(lnfit_T34_L_102)
AICc(lm_T34_L_102)



#T48 

# T48 ---------------------------------------------------------------------



start_T48_L_60 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T48_L_60$Minutes, T48_L_60$weight_perA)#plot observed data
    m <- mm; b <- b0; 
    curve(m*log(x) + b, add=TRUE)
    start_T48_L_60 <<- list(m=m, b=b)
  },
  mm=slider(-1, 10, step = 0.001,  initial = 0.03),
  b0=slider(-30, 1, step = 0.001, initial = -.03))
  

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T48_L_60

# Model fit using the start values:
lnfit_T48_L_60 <- nls(weight_perA ~ I(m*log(Minutes) - b), data= T48_L_60, start = start_T48_L_60)

summary(lnfit_T48_L_60)

