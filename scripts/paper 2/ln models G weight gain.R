#######################
#  Fit ln curve to Gs #
#  to Minutes ~ g/m^2 # 
#  For water uptake   #
#######################

#packages below are for plots, models run in base R
#library(plyr)
#library(ggplot2)
#library(gridExtra)
#library(grid)
#library(investr)
#######################

#m*ln(x)+b
#note that log(x) in R returns the natural log of x

#read in data
G_all <- as.data.frame(read.delim("clipboard"))
names(G_all)

#models that work:


# T11 30 -------------------------------------------------------------------


T11_G_30_mod<- lm(weight_perA ~ log(Minutes),
                   data = T11_G_30)

summary(T11_G_30_mod)
coef(T11_G_30_mod)


# T11 50.5 ----------------------------------------------------------------

T11_G_50_mod<- lm(weight_perA ~ log(Minutes),
                  data = T11_G_50)

summary(T11_G_50_mod)
coef(T11_G_50_mod)

# T11 90 ------------------------------------------------------------------


T11_G_90_mod<- lm(weight_perA ~ log(Minutes),
                  data = T11_G_90)

summary(T11_G_90_mod)
coef(T11_G_90_mod)

# T16 51 -------------------------------------------------------------------


T16_G_51_mod<- lm(weight_perA ~ log(Minutes),
                   data = T16_G_51 )


summary(T16_G_51_mod)
coef(T16_G_51_mod)


# T6 20 --------------------------------------------------------------------



T6_G_20_mod<- lm(weight_perA ~ log(Minutes),
                  data = T6_G_20 )


summary(T6_G_20_mod)
coef(T6_G_20_mod)


# T6 91 --------------------------------------------------------------------



T6_G_91_mod<- lm(weight_perA ~ log(Minutes),
                  data = T6_G_91 )


summary(T6_G_91_mod)
coef(T6_G_91_mod)



# T8 97 --------------------------------------------------------------------


T8_G_97_mod<- lm(weight_perA ~ log(Minutes),
                  data = T8_G_97 )


summary(T8_G_97_mod)
coef(T8_G_97_mod)


# T8 80 -------------------------------------------------------------------

T8_G_80_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_G_80 )

summary(T8_G_80_mod)
coef(T8_G_80_mod)

# T8 66 -------------------------------------------------------------------


T8_G_66_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_G_66 )

summary(T8_G_66_mod)
coef(T8_G_66_mod)
# T8 50 -------------------------------------------------------------------

T8_G_50_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_G_50 )

summary(T8_G_50_mod)
coef(T8_G_50_mod)

# T34 80 -------------------------------------------------------------------



T34_G_102_mod<- lm(weight_perA ~ log(Minutes),
                   data = T34_G_102 )


summary(T34_G_102_mod)
coef(T34_G_102_mod)

# T34 56 -------------------------------------------------------------------


T34_G_56_mod<- lm(weight_perA ~ log(Minutes),
                   data = T34_G_56 )


summary(T34_G_56_mod)
coef(T34_G_56_mod)



# T34 22 ------------------------------------------------------------------



T34_G_22_mod<- lm(weight_perA ~ log(Minutes),
                   data = T34_G_22 )


summary(T34_G_22_mod)
coef(T34_G_22_mod)




#################################
# Calculating lag and uptake rate -----------------------------------------
#################################

# Time sequence for predicted values
TimeSeq = seq(0,600,1) # from 0-600, in 1 minute intervals
########################

# T11 30 ------------------------------------------------------------------



#lag
# Estimate g/m^2 over time

T11_G_30_flux <- predict(T11_G_30_mod, newdata = list(Minutes= TimeSeq))
T11_G_30_flux_Lag <- (T11_G_30_flux >0)
T11_G_30_total <- count(T11_G_30_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T11_G_30_Lag <- T11_G_30_total[1,2]+1
T11_G_30_Lag

# Estimate uptake rate
T11_G_30_dFlux_dt = diff(predict(T11_G_30_mod, newdata = list(Minutes = TimeSeq)))
max(T11_G_30_dFlux_dt)

# T11 50 ------------------------------------------------------------------

T11_G_50_flux <- predict(T11_G_50_mod, newdata = list(Minutes= TimeSeq))
T11_G_50_flux_Lag <- (T11_G_50_flux >0)
T11_G_50_total <- count(T11_G_50_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T11_G_50_Lag <- T11_G_50_total[1,2]+1
T11_G_50_Lag

# T16 51 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T16_G_51_correction = predict(T16_G_51_mod, newdata = list(Minutes= TimeSeq))
T16_G_51_flux <- T16_G_51_correction
T16_G_51_flux_Lag <- (T16_G_51_flux >0)
T16_G_51_total <- count(T16_G_51_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T16_G_51_Lag <- T16_G_51_total[1,2]+1
T16_G_51_Lag

# T11 90 ------------------------------------------------------------------

T11_G_90_flux <- predict(T11_G_90_mod, newdata = list(Minutes= TimeSeq))
T11_G_90_flux_Lag <- (T11_G_90_flux >0)
T11_G_90_total <- count(T11_G_90_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T11_G_90_Lag <- T11_G_90_total[1,2]+1
T11_G_90_Lag

# T6 20 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_G_20_correction = predict(T6_G_20_mod, newdata = list(Minutes= TimeSeq))
T6_G_20_flux <- T6_G_20_correction
T6_G_20_flux_Lag <- (T6_G_20_flux >0)
T6_G_20_total <- count(T6_G_20_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T6_G_20_Lag <- T6_G_20_total[1,2]+1
T6_G_20_Lag

# Estimate uptake rate
T6_G_20_dFlux_dt = diff(predict(T6_G_20_mod, newdata = list(Minutes = TimeSeq)))
max(T6_G_20_dFlux_dt)



# T6 91 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_G_91_correction = predict(T6_G_91_mod, newdata = list(Minutes= TimeSeq))
T6_G_91_flux <- T6_G_91_correction
T6_G_91_flux_Lag <- (T6_G_91_flux >0)
T6_G_91_total <- count(T6_G_91_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T6_G_91_Lag <- T6_G_91_total[1,2]+1
T6_G_91_Lag

# Estimate uptake rate
T6_G_91_dFlux_dt = diff(predict(T6_G_91_mod, newdata = list(Minutes = TimeSeq)))
max(T6_G_91_dFlux_dt)


# T8 97 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T8_G_97_correction = predict(T8_G_97_mod, newdata = list(Minutes= TimeSeq))
T8_G_97_flux <- T8_G_97_correction
T8_G_97_flux_Lag <- (T8_G_97_flux >0)
T8_G_97_total <- count(T8_G_97_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_G_97_Lag <- T8_G_97_total[1,2]+1
T8_G_97_Lag

# Estimate uptake rate
T8_G_97_dFlux_dt = diff(predict(T8_G_97_mod, newdata = list(Minutes = TimeSeq)))
max(T8_G_97_dFlux_dt)


# T8 80 -------------------------------------------------------------------
T8_G_80_correction = predict(T8_G_80_mod, newdata = list(Minutes= TimeSeq))
T8_G_80_flux <- T8_G_80_correction
T8_G_80_flux_Lag <- (T8_G_80_flux >0)
T8_G_80_total <- count(T8_G_80_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_G_80_Lag <- T8_G_80_total[1,2]+1
T8_G_80_Lag


# T8 66 -------------------------------------------------------------------

T8_G_66_correction = predict(T8_G_66_mod, newdata = list(Minutes= TimeSeq))
T8_G_66_flux <- T8_G_66_correction
T8_G_66_flux_Lag <- (T8_G_66_flux >0)
T8_G_66_total <- count(T8_G_66_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_G_66_Lag <- T8_G_66_total[1,2]+1
T8_G_66_Lag

# T8 50 -------------------------------------------------------------------

T8_G_50_correction = predict(T8_G_50_mod, newdata = list(Minutes= TimeSeq))
T8_G_50_flux <- T8_G_50_correction
T8_G_50_flux_Lag <- (T8_G_50_flux >0)
T8_G_50_total <- count(T8_G_50_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_G_50_Lag <- T8_G_50_total[1,2]+1
T8_G_50_Lag

# T34 102 ------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T34_G_102_correction = predict(T34_G_102_mod, newdata = list(Minutes= TimeSeq))
T34_G_102_flux <- T34_G_102_correction
T34_G_102_flux_Lag <- (T34_G_102_flux >0)
T34_G_102_total <- count(T34_G_102_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_G_102_Lag <- T34_G_102_total[1,2]+1
T34_G_102_Lag


# T34 56 ------------------------------------------------------------------


#lag
# Estimate g/m^2 over time
T34_G_56_correction = predict(T34_G_56_mod, newdata = list(Minutes= TimeSeq))
T34_G_56_flux <- T34_G_56_correction
T34_G_56_flux_Lag <- (T34_G_56_flux >0)
T34_G_56_total <- count(T34_G_56_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_G_56_Lag <- T34_G_56_total[1,2]+1
T34_G_56_Lag

# Estimate uptake rate
T34_G_56_dFlux_dt = diff(predict(T34_G_56_mod, newdata = list(Minutes = TimeSeq)))
max(T34_G_56_dFlux_dt)
# T34 22 ------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T34_G_22_correction = predict(T34_G_22_mod, newdata = list(Minutes= TimeSeq))
T34_G_22_flux <- T34_G_22_correction
T34_G_22_flux_Lag <- (T34_G_22_flux >0)
T34_G_22_total <- count(T34_G_22_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_G_22_Lag <- T34_G_22_total[1,2]+1
T34_G_22_Lag

# Estimate uptake rate
T34_G_22_dFlux_dt = diff(predict(T34_G_22_mod, newdata = list(Minutes = TimeSeq)))
max(T34_G_22_dFlux_dt)











###################
# 