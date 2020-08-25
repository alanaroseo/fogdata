#######################
#  Fit ln curve to Ls #
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
L_all <- as.data.frame(read.delim("clipboard"))
names(L_all)

#models that work:


# T11 30 -------------------------------------------------------------------


T11_L_30_mod<- lm(weight_perA ~ log(Minutes),
                  data = T11_L_30)

summary(T11_L_30_mod)
coef(T11_L_30_mod)


# T11 50.5 ----------------------------------------------------------------

T11_L_50_mod<- lm(weight_perA ~ log(Minutes),
                  data = T11_L_50)

summary(T11_L_50_mod)
coef(T11_L_50_mod)

# T11 90 ------------------------------------------------------------------


T11_L_90_mod<- lm(weight_perA ~ log(Minutes),
                  data = T11_L_90)

summary(T11_L_90_mod)
coef(T11_L_90_mod)

# T16 37 ------------------------------------------------------------------

T16_L_37_mod<- lm(weight_perA ~ log(Minutes),
                  data = T16_L_37 )


summary(T16_L_37_mod)
coef(T16_L_37_mod)

# T16 51 -------------------------------------------------------------------


T16_L_51_mod<- lm(weight_perA ~ log(Minutes),
                  data = T16_L_51 )


summary(T16_L_51_mod)
coef(T16_L_51_mod)


# T6 20 --------------------------------------------------------------------



T6_L_20_mod<- lm(weight_perA ~ log(Minutes),
                 data = T6_L_20 )


summary(T6_L_20_mod)
coef(T6_L_20_mod)


# T6 45 -------------------------------------------------------------------


T6_L_45_mod<- lm(weight_perA ~ log(Minutes),
                 data = T6_L_45 )


summary(T6_L_45_mod)
coef(T6_L_45_mod)

# T6 91 --------------------------------------------------------------------



T6_L_91_mod<- lm(weight_perA ~ log(Minutes),
                 data = T6_L_91 )


summary(T6_L_91_mod)
coef(T6_L_91_mod)



# T8 97 --------------------------------------------------------------------


T8_L_97_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_L_97 )


summary(T8_L_97_mod)
coef(T8_L_97_mod)


# T8 80 -------------------------------------------------------------------

T8_L_80_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_L_80 )

summary(T8_L_80_mod)
coef(T8_L_80_mod)

# T8 66 -------------------------------------------------------------------


T8_L_66_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_L_66 )

summary(T8_L_66_mod)
coef(T8_L_66_mod)
# T8 50 -------------------------------------------------------------------

T8_L_50_mod<- lm(weight_perA ~ log(Minutes),
                 data = T8_L_50 )

summary(T8_L_50_mod)
coef(T8_L_50_mod)

# T34 102 -----------------------------------------------------------------
T34_L_102_mod<- lm(weight_perA ~ log(Minutes),
                   data = T34_L_102 )


summary(T34_L_102_mod)
coef(T34_L_102_mod)

# T34 80 -------------------------------------------------------------------



T34_L_80_mod<- lm(weight_perA ~ log(Minutes),
                   data = T34_L_80 )


summary(T34_L_80_mod)
coef(T34_L_80_mod)

# T34 56 -------------------------------------------------------------------


T34_L_56_mod<- lm(weight_perA ~ log(Minutes),
                  data = T34_L_56 )


summary(T34_L_56_mod)
coef(T34_L_56_mod)



# T34 22 ------------------------------------------------------------------



T34_L_22_mod<- lm(weight_perA ~ log(Minutes),
                  data = T34_L_22 )


summary(T34_L_22_mod)
coef(T34_L_22_mod)

# T48 60 ------------------------------------------------------------------



T48_L_60_mod<- lm(weight_perA ~ log(Minutes),
                  data = T48_L_60 )


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
T11_L_30_flux <- T11_L_30_correction
T11_L_30_flux_Lag <- (T11_L_30_flux >0)
T11_L_30_total <- count(T11_L_30_flux_Lag==FALSE)
#lag = one more than the false x freq from count(flux_Lag)
T11_L_30_Lag <- T11_L_30_total[1,2]+1
T11_L_30_Lag

# Estimate uptake rate
T11_L_30_dFlux_dt = diff(predict(T11_L_30_mod, newdata = list(Minutes = TimeSeq)))
max(T11_L_30_dFlux_dt)

# T11 50 ------------------------------------------------------------------

T11_L_50_correction = predict(T11_L_50_mod, newdata = list(Minutes= TimeSeq))
T11_L_50_flux <- T11_L_50_correction
T11_L_50_flux_Lag <- (T11_L_50_flux >0)
T11_L_50_total <- count(T11_L_50_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T11_L_50_Lag <- T11_L_50_total[1,2]+1
T11_L_50_Lag



# T11 90 ------------------------------------------------------------------

T11_L_90_correction = predict(T11_L_90_mod, newdata = list(Minutes= TimeSeq))
T11_L_90_flux <- T11_L_90_correction
T11_L_90_flux_Lag <- (T11_L_90_flux >0)
# T16 51 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T16_L_51_correction = predict(T16_L_51_mod, newdata = list(Minutes= TimeSeq))
T16_L_51_flux <- T16_L_51_correction
T16_L_51_flux_Lag <- (T16_L_51_flux >0)
T16_L_51_total <- count(T16_L_51_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T16_L_51_Lag <- T16_L_51_total[1,2]+1
T16_L_51_Lag

# T16 37 ------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T16_L_37_correction = predict(T16_L_37_mod, newdata = list(Minutes= TimeSeq))
T16_L_37_flux <- T16_L_37_correction
T16_L_37_flux_Lag <- (T16_L_37_flux >0)





# T6 20 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_L_20_correction = predict(T6_L_20_mod, newdata = list(Minutes= TimeSeq))
T6_L_20_flux <- T6_L_20_correction
T6_L_20_flux_Lag <- (T6_L_20_flux >0)
T6_L_20_total <- count(T6_L_20_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T6_L_20_Lag <- T6_L_20_total[1,2]+1
T6_L_20_Lag

# Estimate uptake rate
T6_L_20_dFlux_dt = diff(predict(T6_L_20_mod, newdata = list(Minutes = TimeSeq)))
max(T6_L_20_dFlux_dt)




# T6 45 -------------------------------------------------------------------
T6_L_45_correction = predict(T6_L_45_mod, newdata = list(Minutes= TimeSeq))
T6_L_45_flux <- T6_L_45_correction
T6_L_45_flux_Lag <- (T6_L_45_flux >0)

# T6 91 -------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T6_L_91_correction = predict(T6_L_91_mod, newdata = list(Minutes= TimeSeq))
T6_L_91_flux <- T6_L_91_correction
T6_L_91_flux_Lag <- (T6_L_91_flux >0)
T6_L_91_total <- count(T6_L_91_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T6_L_91_Lag <- T6_L_91_total[1,2]+1
T6_L_91_Lag

# Estimate uptake rate
T6_L_91_dFlux_dt = diff(predict(T6_L_91_mod, newdata = list(Minutes = TimeSeq)))
max(T6_L_91_dFlux_dt)


# T8 97 -------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T8_L_97_correction = predict(T8_L_97_mod, newdata = list(Minutes= TimeSeq))
T8_L_97_flux <- T8_L_97_correction
T8_L_97_flux_Lag <- (T8_L_97_flux >0)
T8_L_97_total <- count(T8_L_97_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_L_97_Lag <- T8_L_97_total[1,2]+1
T8_L_97_Lag

# Estimate uptake rate
T8_L_97_dFlux_dt = diff(predict(T8_L_97_mod, newdata = list(Minutes = TimeSeq)))
max(T8_L_97_dFlux_dt)


# T8 80 -------------------------------------------------------------------
T8_L_80_correction = predict(T8_L_80_mod, newdata = list(Minutes= TimeSeq))
T8_L_80_flux <- T8_L_80_correction
T8_L_80_flux_Lag <- (T8_L_80_flux >0)
T8_L_80_total <- count(T8_L_80_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_L_80_Lag <- T8_L_80_total[1,2]+1
T8_L_80_Lag


# T8 66 -------------------------------------------------------------------

T8_L_66_correction = predict(T8_L_66_mod, newdata = list(Minutes= TimeSeq))
T8_L_66_flux <- T8_L_66_correction
T8_L_66_flux_Lag <- (T8_L_66_flux >0)
T8_L_66_total <- count(T8_L_66_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_L_66_Lag <- T8_L_66_total[1,2]+1
T8_L_66_Lag

# T8 50 -------------------------------------------------------------------

T8_L_50_correction = predict(T8_L_50_mod, newdata = list(Minutes= TimeSeq))
T8_L_50_flux <- T8_L_50_correction
T8_L_50_flux_Lag <- (T8_L_50_flux >0)
T8_L_50_total <- count(T8_L_50_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T8_L_50_Lag <- T8_L_50_total[1,2]+1
T8_L_50_Lag

# T34 102 ------------------------------------------------------------------
#lag
# Estimate g/m^2 over time
T34_L_102_correction = predict(T34_L_102_mod, newdata = list(Minutes= TimeSeq))
T34_L_102_flux <- T34_L_102_correction
T34_L_102_flux_Lag <- (T34_L_102_flux >0)
T34_L_102_total <- count(T34_L_102_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_L_102_Lag <- T34_L_102_total[1,2]+1
T34_L_102_Lag



# T34 80 ------------------------------------------------------------------
T34_L_80_correction = predict(T34_L_80_mod, newdata = list(Minutes= TimeSeq))
T34_L_80_flux <- T34_L_80_correction
T34_L_80_flux_Lag <- (T34_L_80_flux >0)

# T34 56 ------------------------------------------------------------------


#lag
# Estimate g/m^2 over time
T34_L_56_correction = predict(T34_L_56_mod, newdata = list(Minutes= TimeSeq))
T34_L_56_flux <- T34_L_56_correction
T34_L_56_flux_Lag <- (T34_L_56_flux >0)
T34_L_56_total <- count(T34_L_56_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_L_56_Lag <- T34_L_56_total[1,2]+1
T34_L_56_Lag

# Estimate uptake rate
T34_L_56_dFlux_dt = diff(predict(T34_L_56_mod, newdata = list(Minutes = TimeSeq)))
max(T34_L_56_dFlux_dt)
# T34 22 ------------------------------------------------------------------

#lag
# Estimate g/m^2 over time
T34_L_22_correction = predict(T34_L_22_mod, newdata = list(Minutes= TimeSeq))
T34_L_22_flux <- T34_L_22_correction
T34_L_22_flux_Lag <- (T34_L_22_flux >0)
T34_L_22_total <- count(T34_L_22_flux_Lag)
#lag = one more than the false x freq from count(flux_Lag)
T34_L_22_Lag <- T34_L_22_total[1,2]+1
T34_L_22_Lag

# Estimate uptake rate
T34_L_22_dFlux_dt = diff(predict(T34_L_22_mod, newdata = list(Minutes = TimeSeq)))
max(T34_L_22_dFlux_dt)











# T48 60 ------------------------------------------------------------------
T48_L_60_correction = predict(T48_L_60_mod, newdata = list(Minutes= TimeSeq))
T48_L_60_flux <- T48_L_60_correction
T48_L_60_flux_Lag <- (T48_L_60_flux >0)



###################
# 