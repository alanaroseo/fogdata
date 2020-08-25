#######################
#  Fit Gompertz curve #
#  to Minutes ~ g/m^2 # 
#  For water uptake   #
#######################
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(investr)
library(manipulate)

#Gompertz: Asym*exp(-b2*b3^X)
#x 	a numeric vector of values at which to evaluate the model: (g/m^2)
#Asym 	a numeric parameter representing the asymptote.
#b2 	a numeric parameter related to the value of the function at x = 0
#b3 	a numeric parameter related to the scale the x axis.

#y'= -(Asym * (exp(-b2 * b3^X) * (b2 * (b3^X * log(b3)))))
dXdy <- D(expression( Asym*exp(-b2*b3^X)), "X")
d

#models that work:


# T11 30 -------------------------------------------------------------------


T11_L_30_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
           data = T11_L_30, control = list(maxiter = 1000))

summary(T11_L_30_mod)
coef(T11_L_30_mod)

# T16 37 ------------------------------------------------------------------

T16_37_L <- as.data.frame(read.delim("clipboard"))

T16_37_L_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                   data = T16_37_L, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T16_37_L_mod)
n <- length(pred)
res <- resid(T16_37_L_mod)
w <- weights(T16_37_L_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T16_37_L_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T16_37_L_mod<- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)

R2_T16_37_L_mod
summary(T16_37_L_mod)
coef(T16_37_L_mod)

# T16 51 -------------------------------------------------------------------


T16_L_51_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                   data = T16_L_51, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T16_L_51_mod)
n <- length(pred)
res <- resid(T16_L_51_mod)
w <- weights(T16_L_51_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T16_L_51_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T16_L_51_mod<- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)

R2_T16_L_51_mod
summary(T16_L_51_mod)
coef(T16_L_51_mod)


# T6 20 --------------------------------------------------------------------



T6_L_20_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                  data = T6_L_20, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T6_L_20_mod)
n <- length(pred)
res <- resid(T6_L_20_mod)
w <- weights(T6_L_20_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T6_L_20_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T6_L_20_mod<- list(pseudo.R.squared = r.sq,
                      adj.R.squared = adj.r.sq)

R2_T6_L_20_mod
summary(T6_L_20_mod)
coef(T6_L_20_mod)


# T6 45 --------------------------------------------------------------------





T6_L_45_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                  data = T6_L_45, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T6_L_45_mod)
n <- length(pred)
res <- resid(T6_L_45_mod)
w <- weights(T6_L_45_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T6_L_45_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T6_L_45_mod<- list(pseudo.R.squared = r.sq,
                      adj.R.squared = adj.r.sq)

R2_T6_L_45_mod
summary(T6_L_45_mod)
coef(T6_L_45_mod)



# T6 91 --------------------------------------------------------------------



T6_L_91_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                  data = T6_L_91, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T6_L_91_mod)
n <- length(pred)
res <- resid(T6_L_91_mod)
w <- weights(T6_L_91_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T6_L_91_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T6_L_91_mod<- list(pseudo.R.squared = r.sq,
                      adj.R.squared = adj.r.sq)

R2_T6_L_91_mod
summary(T6_L_91_mod)
coef(T6_L_91_mod)



# T8 97 --------------------------------------------------------------------


T8_L_97_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                  data = T8_L_97, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T8_L_97_mod)
n <- length(pred)
res <- resid(T8_L_97_mod)
w <- weights(T8_L_97_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T8_L_97_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T8_L_97_mod<- list(pseudo.R.squared = r.sq,
                      adj.R.squared = adj.r.sq)

R2_T8_L_97_mod
summary(T8_L_97_mod)
coef(T8_L_97_mod)


# T34 80 -------------------------------------------------------------------



T34_L_80_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                   data = T34_L_80, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T34_L_80_mod)
n <- length(pred)
res <- resid(T34_L_80_mod)
w <- weights(T34_L_80_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T34_L_80_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T34_L_80_mod<- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)

R2_T34_L_80_mod
summary(T34_L_80_mod)
coef(T34_L_80_mod)

# T34 56 -------------------------------------------------------------------


T34_L_56_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                   data = T34_L_56, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T34_L_56_mod)
n <- length(pred)
res <- resid(T34_L_56_mod)
w <- weights(T34_L_56_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T34_L_56_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T34_L_56_mod<- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)

R2_T34_L_56_mod
summary(T34_L_56_mod)
coef(T34_L_56_mod)



# T48 60 ------------------------------------------------------------------



T48_L_60_mod<- nls(corrected.weight.per.A ~ SSgompertz(Minutes, Asym, b2, b3),
                   data = T48_L_60, control = list(maxiter = 20000, minFactor = .00000000005))

#Efron's pseudo R2
pred <- predict(T48_L_60_mod)
n <- length(pred)
res <- resid(T48_L_60_mod)
w <- weights(T48_L_60_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T48_L_60_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T48_L_60_mod<- list(pseudo.R.squared = r.sq,
                       adj.R.squared = adj.r.sq)

R2_T48_L_60_mod
summary(T48_L_60_mod)
coef(T48_L_60_mod)



#################################
# Calculating lag and uptake rate -----------------------------------------
#################################

# Time sequence for predicted values
TimeSeq = seq(0,600,1) # from 0-600, in 1 minute intervals
########################

# T11 30 ------------------------------------------------------------------



#lag
# Estimate g/m^2 over time
T11_L_30_correction = predict(T11_L_30_mod, newdata = list(Minutes= TimeSeq))
T11_L_30_flux <- correction-23
T11_L_30_flux_lag <- (T11_L_30_flux >0)
T11_L_30_total <- count(flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T11_L_30_lag <- T11_L_30_total[1,2]+1
T11_L_30_lag

# Estimate uptake rate
T11_L_30_dFlux_dt = diff(predict(T11_L_30_mod, newdata = list(Minutes = TimeSeq)))
max(T11_L_30_dFlux_dt)

# T16 37 ------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T16_37_L_correction = predict(T16_37_L_mod, newdata = list(Minutes= TimeSeq))
T16_37_L_flux <- T16_37_L_correction-23
T16_37_L_flux_lag <- (T16_37_L_flux >0)
T16_37_L_total <- count(T16_37_L_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T16_37_L_lag <- T16_37_L_total[1,2]+1
T16_37_L_lag

# Estimate uptake rate
T16_37_L_dFlux_dt = diff(predict(T16_37_L_mod, newdata = list(Minutes = TimeSeq)))
max(T16_37_L_dFlux_dt)


# T16 51 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T16_L_51_correction = predict(T16_L_51_mod, newdata = list(Minutes= TimeSeq))
T16_L_51_flux <- T16_L_51_correction-23
T16_L_51_flux_lag <- (T16_L_51_flux >0)
T16_L_51_total <- count(T16_L_51_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T16_L_51_lag <- T16_L_51_total[1,2]+1
T16_L_51_lag

# Estimate uptake rate
T16_L_51_dFlux_dt = diff(predict(T16_L_51_mod, newdata = list(Minutes = TimeSeq)))
max(T16_L_51_dFlux_dt)



# T6 20 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_L_20_correction = predict(T6_L_20_mod, newdata = list(Minutes= TimeSeq))
T6_L_20_flux <- T6_L_20_correction-23
T6_L_20_flux_lag <- (T6_L_20_flux >0)
T6_L_20_total <- count(T6_L_20_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T6_L_20_lag <- T6_L_20_total[1,2]+1
T6_L_20_lag

# Estimate uptake rate
T6_L_20_dFlux_dt = diff(predict(T6_L_20_mod, newdata = list(Minutes = TimeSeq)))
max(T6_L_20_dFlux_dt)



# T6 45 -------------------------------------------------------------------


#lag
# Estimate g/m^2 over time
T6_L_45_correction = predict(T6_L_45_mod, newdata = list(Minutes= TimeSeq))
T6_L_45_flux <- T6_L_45_correction-23
T6_L_45_flux_lag <- (T6_L_45_flux >0)
T6_L_45_total <- count(T6_L_45_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T6_L_45_lag <- T6_L_45_total[1,2]+1
T6_L_45_lag

# Estimate uptake rate
T6_L_45_dFlux_dt = diff(predict(T6_L_45_mod, newdata = list(Minutes = TimeSeq)))
max(T6_L_45_dFlux_dt)


# T6 91 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_L_91_correction = predict(T6_L_91_mod, newdata = list(Minutes= TimeSeq))
T6_L_91_flux <- T6_L_91_correction-23
T6_L_91_flux_lag <- (T6_L_91_flux >0)
T6_L_91_total <- count(T6_L_91_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T6_L_91_lag <- T6_L_91_total[1,2]+1
T6_L_91_lag

# Estimate uptake rate
T6_L_91_dFlux_dt = diff(predict(T6_L_91_mod, newdata = list(Minutes = TimeSeq)))
max(T6_L_91_dFlux_dt)


# T8 97 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T8_L_97_correction = predict(T8_L_97_mod, newdata = list(Minutes= TimeSeq))
T8_L_97_flux <- T8_L_97_correction-23
T8_L_97_flux_lag <- (T8_L_97_flux >0)
T8_L_97_total <- count(T8_L_97_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T8_L_97_lag <- T8_L_97_total[1,2]+1
T8_L_97_lag

# Estimate uptake rate
T8_L_97_dFlux_dt = diff(predict(T8_L_97_mod, newdata = list(Minutes = TimeSeq)))
max(T8_L_97_dFlux_dt)


# T34 80 ------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T34_L_80_correction = predict(T34_L_80_mod, newdata = list(Minutes= TimeSeq))
T34_L_80_flux <- T34_L_80_correction-23
T34_L_80_flux_lag <- (T34_L_80_flux >0)
T34_L_80_total <- count(T34_L_80_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T34_L_80_lag <- T34_L_80_total[1,2]+1
T34_L_80_lag

# Estimate uptake rate
T34_L_80_dFlux_dt = diff(predict(T34_L_80_mod, newdata = list(Minutes = TimeSeq)))
max(T34_L_80_dFlux_dt)


# T34 56 ------------------------------------------------------------------


#lag
# Estimate g/m^2 over time
T34_L_56_correction = predict(T34_L_56_mod, newdata = list(Minutes= TimeSeq))
T34_L_56_flux <- T34_L_56_correction-23
T34_L_56_flux_lag <- (T34_L_56_flux >0)
T34_L_56_total <- count(T34_L_56_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T34_L_56_lag <- T34_L_56_total[1,2]+1
T34_L_56_lag

# Estimate uptake rate
T34_L_56_dFlux_dt = diff(predict(T34_L_56_mod, newdata = list(Minutes = TimeSeq)))
max(T34_L_56_dFlux_dt)
# T48 60 ------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T48_L_60_correction = predict(T48_L_60_mod, newdata = list(Minutes= TimeSeq))
T48_L_60_flux <- T48_L_60_correction-23
T48_L_60_flux_lag <- (T48_L_60_flux >0)
T48_L_60_total <- count(T48_L_60_flux_lag)
#lag = one more than the false x freq from count(flux_lag)
T48_L_60_lag <- T48_L_60_total[1,2]+1
T48_L_60_lag

# Estimate uptake rate
T48_L_60_dFlux_dt = diff(predict(T48_L_60_mod, newdata = list(Minutes = TimeSeq)))
max(T48_L_60_dFlux_dt)
plot(T48_L_60_dFlux_dt)













###################
# code for plots and R2 etc -----------------------------------------------

------------------------------------------------------------------





#Efron's pseudo R2
pred <- predict(T11_L_30_mod)
n <- length(pred)
res <- resid(T11_L_30_mod)
w <- weights(T11_L_30_mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T11_L_30_mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T11_L_30_mod<- list(pseudo.R.squared = r.sq,
                          adj.R.squared = adj.r.sq)

R2_T11_L_30_mod


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plot(T11_L_30_mod)

plotFit(T11_L_30_mod, interval = 'confidence', data = T11_L_30, xlim = c(0,max(T11_L_30$Minutes)), ylim = c(1-min(T11_L_30$corrected.weight.per.A), 1+max(T11_L_30$corrected.weight.per.A)), cex = 1.29, cex.axis = 1.9, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.9*max(T11_L_30$Minutes),max(T11_L_30$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 30 L'), side = 3, cex = 1.1, line = 1)


# other crap --------------------------------------------------------------


# Gompertz manipulater -------------------------------------------------------------------

start_T1190 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_90$Minutes, T11_L_90$corrected.weight.per.A)#plot observed data
    Asym <- d0; b2 <- a0; b3 <- c0;      curve(Asym*exp(-b2*b3^x), add=TRUE)
    start_T1190 <<- list(Asym=Asym, b2=b2, b3=b3)   },
  d0=slider(1, 200, step = 0.001,  initial = max(T11_L_90$corrected.weight.per.A)),   
  a0=slider(0,40, step = 0.001, initial = min(T11_L_90$corrected.weight.per.A)),
  c0=slider(0, 300, step = 0.001, initial = .9)
)

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T1190

# Model fit using the start values:
T11_L_30_mod<- nls(corrected.weight.per.A ~ I(Asym*exp(-b2*b3^Minutes)), data= T11_L_90, start = start_T1190, control = list(maxiter = 20000, minFactor = .000000000009) )

# 4 parameter logistic manipulater ----------------------------------------------------------------



summary(T1190mod)


start_T1190 <- list()     # Make an empty list for the starting values

#exponential curve manipulator, use sliders to adjust parameters until the curve fits the data
manipulate(
  {
    plot(T11_L_90$Minutes, T11_L_90$corrected.weight.per.A)#plot observed data
    d <- d0; a <- a0; c <- c0; b <- b0;     curve(d+((a-d)/(1+((x/c)^b))), add=TRUE)
    start_T1190 <<- list(a=a, b=b, c=c, d=d)   },
  d0=slider(1, 200, step = 0.001,  initial = max(T11_L_90$corrected.weight.per.A)),   
  a0=slider(0,40, step = 0.001, initial = 30),
  c0=slider(0, 300, step = 0.001, initial = 179), 
  b0 =slider(0,40, step = 0.001, initial = 9)
  )

# When the slider box closes start_data() becomes a list of named parameters for use as start values

#print start values:
start_T1190

# Model fit using the start values:
  T11_L_30_mod<- nls(corrected.weight.per.A ~ I(d+((a-d)/(1+((Minutes/c)^b)))), data= T11_L_90, start = start_T1190, control = list(maxiter = 20000, minFactor = .000000000009) )

summary(T1190mod)


coef(Tll90mod)



#Efron's pseudo R2
pred <- predict(T1190mod)
n <- length(pred)
res <- resid(T1190mod)
w <- weights(T1190mod)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(T1190mod)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
R2_T11_L_30_mod<- list(pseudo.R.squared = r.sq,
                    adj.R.squared = adj.r.sq)

R2_T1190mod


# Plot model output
plot.new()#call a new plot
par(mfrow=c(1,1))# Define plotting parameters to add multiple plots try par(mfrow=c(3,2), oma = c(3,3,3,1), mar = c(2,3,1,2))

plot(T1190mod)

plotFit(T1190mod, interval = 'confidence', data = T11_L_90, xlim = c(0,max(T11_L_90$Minutes)), ylim = c(1-min(T11_L_90$corrected.weight.per.A), 1+max(T11_L_90$corrected.weight.per.A)), cex = 1.29, cex.axis = 1.9, cex.main = 1.4, ylab = '' ,xaxt = 'n', yaxt = 'n',col.fit ='salmon', col.conf = 'orchid', col='blue')
axis(1, at = c(0,.9*max(T11_L_90$Minutes),max(T11_L_90$Minutes)), cex.axis = 1, labels = TRUE)
axis(2, at = c(0, 10,20,40,60,80, 100), cex.axis = 1, labels = TRUE)
mtext(expression(paste('g  ', 'per', '  m^2')), side = 2, cex = 1.1, line = 3)
mtext(expression('T11 90 L'), side = 3, cex = 1.1, line = 1)

