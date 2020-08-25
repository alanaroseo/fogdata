# Set directory
setwd("C:/Users/dunaz/Documents/R analyses/FC ABA/Emilio/playing with emilio results")

# Load libraries
library(knitr)
library(magrittr)
library(car)
library(tidyverse)  # to e.g. mutate
library(ggplot2)
library(effects)
library(mgcv)
library(emmeans)
library(boot)
library(propagate)
library(ggpubr)
library(investr)

# BRACKETS: To extract a value or set of values from a data frame, write the data frame's name followed by a pair of hard brackets: deck [ , ]. Between the brackets will go two indexes separated by a comma. The indexes tell R which values to return. R will use the first index to subset the rows of the data frame and the second index to subset the columns.
#Indexes can be of various types, e.g. Positive integers, blank spaces, etc. 
#integers: deck[i,j] will return the value of deck that is in the ith row and the jth column.        To extract more than one value, use a vector of positive integers. For example, you can return the first row of deck with deck[1, c(1, 2, 3)] or deck[1, 1:3].
#You can use a blank space to tell R to extract every value in a dimension.
#very useful basics:https://www.oreilly.com/library/view/hands-on-programming-with/9781449359089/ch04.html


options(contrasts = c("contr.sum", "contr.poly")) ### I will figure it out


# Load data (pear data)
peard.rw <- read.table("C:/Users/dunaz/Documents/R analyses/FC ABA/Emilio/playing with emilio results/allPear15Mar19.txt", header = TRUE, sep= ',')

# We organize the data and create new columns in the dataframe:
peard.rw <- peard.rw %>%
  mutate(mass0 = mass0/1000) %>% #in g
  mutate(massT = massT/1000) %>% #in g
  mutate(area = area/10000) %>% #in m2
  mutate(dry.mass = dry.mass/1000) %>% #in g
  mutate(H2O.0 = (mass0 - dry.mass) / dry.mass) %>% 
  mutate(H2O.T = (massT - dry.mass) / dry.mass) %>%
  mutate(dH2O = H2O.T - H2O.0) %>% #in g g-1
  mutate(dM = massT - mass0) %>% #in g
  mutate(dM.A = dM / area) %>% #in g m-2
  filter(Time>0)



#We avoid any kind of filter

# Pear exponential models -------------------------------------------------

###### Pear mass vs time exponential models ######
### Pear dm.A (mass per surface) vs time exponential model ###### 
dM.A.nls1.p <- nlsList(dM.A ~ exp(A) * (1 - exp(-B*Time^2)) | treat,
                       data = peard.rw,
                       start = c(A = 2.393914, 
                                 B = 7.505735e-05)) 
#values changed for control values of not filtered

plot(dM.A.nls1.p) #plot standardized vs fitted values
summary(dM.A.nls1.p) 
cor(fitted(dM.A.nls1.p), peard.rw$dM.A)^2  #cor computes the correlation of vectors x and y. It seems it is a good correlation (if 1 is the best)

### Pear dH2O (mass increment per dw) vs time exponential model ###### 
dH2O.nls1.p <- nlsList(dH2O ~ exp(A) * (1 - exp(-B*Time^2)) | treat,
                       data = peard.rw,
                       start = c(A = -2.404087, 
                                 B = 6.623393e-05))

dH2O.nls1.p
plot(dH2O.nls1.p)
summary(dH2O.nls1.p)
cor(fitted(dH2O.nls1.p), peard.rw$dH2O)^2


####### Pear WP exponential Time model ###### 

WP.nls1.p <- nlsList(
  WP ~ A + exp(B0 + B2 * Time^2) | treat,
  data = peard.rw,
  start = c(A = 0.6026779,
            B0 = 0.3745309,
            B2 = -6.377952e-05),
  control = nls.control(maxiter = 1000, minFactor = 1e-5, tol = 1e-5))

#initial values changed for control values of not filtered data
WP.nls1.p
plot(WP.nls1.p)
summary(WP.nls1.p)
cor(fitted(WP.nls1.p), peard.rw$WP)^2


### Plot pear exponential models --------------------------------------------

# First, we create a list with all the predicted values.
newd <- expand.grid(treat = unique(peard.rw$treat), Time = 0:520) %>%
  split(., .$treat) %>%
  Map(cbind, ., dM.A = Map(predict, dM.A.nls1.p, .)) %>%
  Map(cbind, ., dH2O = Map(predict, dH2O.nls1.p, .)) %>%
  Map(cbind, ., WP = Map(predict, WP.nls1.p, .))

#Expand.grid creates a data frame from all combinations of the supplied vectors or factors.
#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
#The map function transform their input by applying a function to each element and returning a vector the same length as the input. It always returns a list. It applies a function to each element of a vector.


#so, I get a list composed of different lists/data frames that are the treatments.
# expand.grid(treat = unique(peard.rw$treat), Time = 0:520) creates the dataframe or list only with treatment and time
#split(., .$treat): split divides the data in the vector x into the groups defined by f.
# Meaning of . : often . in formulas means "all other variables", for example lm(y~., data=dd) will regress y on all the other variables in the data.frame dd. Also . is used to refer to the data in the current group.
#In this case, it refers to the data created previously, i.e. the list created for time 0 to 520.
#cbind combines by columns.


scatterplot(dM.A ~ Time | treat, peard.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$dM.A ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$dM.A ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$dM.A ~ newd$fc$Time, lwd = 2, col = "lightblue")

scatterplot(dH2O ~ Time | treat, peard.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$dH2O ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$dH2O ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$dH2O ~ newd$fc$Time, lwd = 2, col = "lightblue")

scatterplot(WP ~ Time | treat, peard.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$WP ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$WP ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$WP ~ newd$fc$Time, lwd = 2, col = "lightblue")


####### Resistance function ###### 
###THIS IS NOT USED!!!
curve(Rst.exp(Time = x), from = 0, to = 300, ylim = c(0, 200))
min(Rst.exp(Time = 0:650))

#### Create flux function: derivative of dM.A ####

# This is flux function (dM.A.dt):
dM.A.dt <- function(A, B, Time) 2 * B * exp(A - B * Time^2) * Time

#It can be calculated as:
D(expression(exp(A) * (1 - exp(-B*Time^2))), "Time") #D is deriv function
#it gives the same written in another way 

## Calculate Resistance from parameters of dM.A and WP ####
#THIS IS NOT USED!!!!
#Resistance is dM.A.dt / WP; 60 to transform into seconds
#The function of the resitance would be: Awp + exp(B0wp + B2wp * Time^2) / 2 * Bma * exp(Ama - Bma * Time^2) * Time
# Rst.exp <- function(Ama = 2.393914, Bma = 7.505738e-05, 
#                     Awp = 0.6026779, B0wp = 0.3745309, 
#                     B2wp = -6.377952e-05,Time = 100) {
#   60 * (exp(-Ama + Bma * Time^2) * 
#           (Awp + exp(B0wp + B2wp * Time^2)))/
#     (2 * Bma * Time)
# }

### Function to bootstrap conductance Kap ####
#(check other bootstrapping function for more details)
#We need to bootstrap resistance/conductance since it combines two function
#with experimental data, and we want confidence intervals in all the points
#We need to define data and i for bootstrapping
#Data is a generic name and will be substituted later by pear.dw
#data[i, ] will subset 'data' and return the i rows as a data frame.
#Since i is a parameter of the function, we specify that i is any row.
#original.data[, c("treat", "Time", "pdM.A", "pWP")] will 
#subset original data and return 4 columns named as treat, time etc.
#i.e. WE JUST WANT THESE COLUMNS
#data[, c("resdM.A", "resWP")]) will subset data and return the two columns
#named "resdM.A", "resWP"
##### I DON'T UNDERSTAND THE DIFFERENCE BETWEEN ORIGINAL.DATA AND DATA
#The with( ) function applys an expression to a dataset.
#In this case, it applys pdM.A + resdM.A to data (creating a new column)
#p possibly stands for predicted, res for residuals

#is.null returns TRUE if its argument's value is NULL and FALSE otherwise.
Kap.fun <- function(data, i, dma.mod, wp.mod){
  # build residual resampled boot data #### 
  original.data <- data #define original.data, I think
  data <- data[i, ] #specify we want i as a value in rows, I think. and define data
  data <- cbind(original.data[, c("treat", 
                                  "Time", 
                                  "pdM.A", 
                                  "pWP")],
                data[, c("resdM.A",
                         "resWP")])
  data$dM.A <- with(data, pdM.A + resdM.A) 
  data$WP <- with(data, pWP + resWP)
  
  # Update models with boot data ####
  dM.Anls <- update(dma.mod, data = data)
  WPnls <- update(wp.mod, data = data)
  
  # Trap nonfits ####
  null.fits <- any(unlist(
    list(lapply(lapply(dM.Anls, coef), is.null),
         lapply(lapply(WPnls, coef), is.null))
  )) 
  
  if(null.fits) {return(rep(NA, 450))} else{
    #450 was selected as an approximation, cause there was no need to add more
    
    # Add values for dM.A.dt, WP and Kap (conductance) ####
    #(1:150) * 3 because we choose to bootstrap every 3 min (not necessary more)
    newd <-  expand.grid(treat = unique(data$treat), Time = (1:150) * 3) %>%
      split(.$treat) %>%
      Map(cbind, ., 
          dM.A.dt = Map(function(x, y){
            dM.A.dt(A = unname(coef(x)[1]), 
                    B = coef(x)[2], 
                    Time = y$Time)}, 
            dM.Anls, .)) %>%
      Map(cbind, ., WP = Map(predict, WPnls, .)) %>%
      Map(cbind, ., Kap = lapply(., function(x) x$dM.A.dt / (x$WP * 60)))
    
    # Return results ####
    #Return all results of conductance (450 points)
    return(bind_rows(newd)$Kap)
  }
}

#####OTHER FUNCTIONS TO BE USED#####

#Minimum and maximum functions consulted in another source.
#They are used to estimate maximum wp and minimum r. Other max and min from parameteres in models
##### Find local minimum ####
lcl.min <- function(v1) {
  index <- (which(diff(sign(diff(v1))) == 2) + 1)
  if(length(index) == 0) index <- which.min(v1) else index <- index[1]
  return(c(index = index, value = v1[index]))
}

##### Find local maximum ####
lcl.max <- function(v1) {
  index <- (which(diff(sign(diff(v1))) == -2) + 1)
  if(length(index) == 0) index <- which.max(v1) else index <- index[1]
  return(c(index = index, value = v1[index]))
}

####### Function to bootstrap R, and all parameters for tables ####
parm.fun3 <- function(data, i, dma.mod, h20.mod, wp.mod){
  # build residual resampled boot data
  #(we need to define data and i for bootstraping)
  
  original.data <- data
  data <- data[i, ]
  data <- cbind(original.data[, c("treat", 
                                  "Time", 
                                  "pdM.A", 
                                  "pdH2O", 
                                  "pWP")],
                data[, c("resdM.A", #residuals extracted from models
                         "resdH2O",
                         "resWP")])
  
  data$dM.A <- with(data, pdM.A + resdM.A) #I think it replaces dM.A original data with that of the model
  data$dH2O <- with(data, pdH2O + resdH2O)
  data$WP <- with(data, pWP + resWP)
  
  # Update models with boot data ####
  dM.Anls <- update(dma.mod, data = data) #dM.Anls is a new name
  dH2Onls <- update(h20.mod, data = data)
  WPnls <- update(wp.mod, data = data)
  
  # Trap nonfits #### 
  #in case some data do not fit the model. We say to return NA values
  
  null.fits <- any(unlist(
    list(lapply(lapply(dM.Anls, coef), is.null),
         lapply(lapply(dH2Onls, coef), is.null),
         lapply(lapply(WPnls, coef), is.null))
  ))
  
  if(null.fits) {return(rep(NA, 18))} else{   #17 is the number of statistics calculated for each sample of bootstrap (see at the end)... or at least what we want to see?
    
    # Add predicted values, derivative for dM.A and R ####
    newd <-  expand.grid(treat = unique(data$treat), Time = c(1/60, 1:520)) %>%
      split(.$treat) %>%
      Map(cbind, 
          ., 
          dM.A.dt = Map(function(x, y){
            dM.A.dt(A = unname(coef(x)[1]), B = coef(x)[2], Time = y$Time)}, 
            dM.Anls, .)) %>%
      Map(cbind, ., dH2O = Map(predict, dH2Onls, .)) %>%
      Map(cbind, ., WP = Map(predict, WPnls, .)) %>%
      Map(cbind, ., dM.A = Map(predict, dM.Anls, .)) %>%
      Map(cbind, ., R = lapply(., function(x) 60 * x$WP / x$dM.A.dt))
    #Previous function calculates flux from the coeficients of exponential mass model:
    #coef 1 is A, and it is the asymptote (maximum mass)
    #(If we have problems with names we could use unname as (A = unname(coef(x)[1]), B = coef(x)[2], Time = y$Time)
    #Here is where we estimate resistance from WP and flux
    #(I can follow changes in str(newd) by running portions of the code after the } simbol)
    
    # Add  maxima, minima and corresponding times ####
    MaxdH2O.g <- lapply(dH2Onls, function(x) exp(unname(coef(x)[1])))
    MaxdM.A <- lapply(dM.Anls, function(x) exp(unname(coef(x)[1])))
    MaxWP <- lapply(newd, function(x) lcl.max(x$WP))
    MinWP <- lapply(WPnls, function(x) unname(coef(x)[1]))
    MaxdM.A.dt <- lapply(newd, function(x) c(index = which.max(x$dM.A.dt),
                                             value = max(x$dM.A.dt)))
    #MaxdH2O.g: applies a function to 'extract' the coef A of the model, which is the maximum (asymptote)
    #MaxdM.A: same 
    #MaxWP: to estimate maximum, it applies previously created function
    #MinWP: to estimate minimum, it extracts the coef A (1) from the model, that's the asympote on the bottom
    
    #adds estimated values to the dataframe
    #MAYBE C IS FOR COLUMN?
    newd <- Map(c, newd,
                maxdh20 = MaxdH2O.g,
                maxdma = MaxdM.A,
                maxwp = MaxWP,
                minwp = MinWP,
                maxdmadt = MaxdM.A.dt)
    
    MinR <- lapply(newd, function(x) lcl.min(x$R))
    t.MaxdM.A.dt <- lapply(newd, function(x) x$Time[x$maxdmadt.index])
    WP.MaxdM.A.dt <- lapply(newd, function(x) x$WP[x$maxdmadt.index])
    
    newd <- Map(c, newd,
                minr = MinR,
                wpmaxdmadt = WP.MaxdM.A.dt,
                tmaxdmadt = t.MaxdM.A.dt)
    
    t.MinR <- lapply(newd, function(x) x$Time[x$minr.index])
    WP.MinR <- lapply(newd, function(x) x$WP[x$minr.index])
    dH2O.g.MinR <- lapply(newd, function(x) x$dH2O[x$minr.index])
    dM.A.MinR <- lapply(newd, function(x) x$dM.A[x$minr.index])
    t.half.H2O.g <- lapply(newd, function(x) {
      d = abs(x$dH2O - x$maxdh20 / 2)
      return(x$Time[which.min(d)])
    })
    t.half.WP <- lapply(newd, function(x) {
      d = abs(x$WP - x$maxwp.value / 2)
      return(x$Time[which.min(d)])
    })
    t.half.dM.A <- lapply(newd, function(x) {
      d = abs(x$dM.A - x$maxdma / 2) 
      return(x$Time[which.min(d)])
    })
    
    newd <- Map(c, newd,
                tminr = t.MinR,
                wpminr = WP.MinR,
                dh20gminr = dH2O.g.MinR,
                dMAminr = dM.A.MinR,
                thalfh20g = t.half.H2O.g,
                thalfWP = t.half.WP,
                thalfdma = t.half.dM.A)
    # Return results ####
    #This is a function to return results of interest, and we unlist since we use lapply and we want to get an horizontal 'vector'
    return(
      unlist(
        lapply(
          newd, 
          function(x) x[c("maxdh20", 
                          "maxdma", 
                          "maxwp.index", 
                          "maxwp.value", 
                          "minwp", 
                          "maxdmadt.index", 
                          "maxdmadt.value", 
                          "minr.index", 
                          "minr.value", 
                          "wpmaxdmadt", 
                          "tmaxdmadt", 
                          "tminr", 
                          "wpminr", 
                          "dh20gminr", 
                          "dMAminr",
                          "thalfh20g", 
                          "thalfWP",
                          "thalfdma")])))
    }
  } ## End parm.fun3 ===


#maxdma does not appear in the table!!!! Trying with unname!


#I guess .index and .value are needed cause of the used functions.


#Now, we apply the functions

# Prepare the data set for the boot function ####
peard.rw$pdM.A <- fitted(dM.A.nls1.p)
peard.rw$pdH2O <- fitted(dH2O.nls1.p)
peard.rw$pWP <- fitted(WP.nls1.p)
peard.rw$resdM.A <- residuals(dM.A.nls1.p)
peard.rw$resdH2O <- residuals(dH2O.nls1.p)
peard.rw$resWP <- residuals(WP.nls1.p)

# Run on original data ####
pear.rslts.o <- parm.fun3(data = peard.rw, 
                          i = 1:nrow(peard.rw),
                          dma.mod = dM.A.nls1.p,
                          h20.mod = dH2O.nls1.p,
                          wp.mod = WP.nls1.p)


#ESTA PUTA FUNCION NO RECONOCE LA NUEVA ADICION DE THALFDMA!!!!!

#And this is what I get when I run the simulation:
# Error in t.star[r, ] <- res[[r]] : 
#   number of items to replace is not a multiple of replacement length
#sOLVED: WHERE NA THERE WAS A WRONG NUMBER PLUS
##IT WAS NOT RECOGNIZING DMA UNTIL I USED UNNAME!!!
#SO, FOR NEXT TIME, DO NOT USE THIS TYPE OF NOTATION WHEN IT CAN BE CONFUSED WITH A PARAMETER

# Run simulations ####
system.time(
  pear.rslts <- boot(data = peard.rw, 
                     statistic = parm.fun3, 
                     R = 5000, 
                     parallel = "multicore",
                     ncpus = 8,
                     dma.mod = dM.A.nls1.p,
                     h20.mod = dH2O.nls1.p,
                     wp.mod = WP.nls1.p)
)



#### Prep results 

#Emilio did this at the beginning. I added more things (see later)
#colnames(pear.rslts$t) <- names(pear.rslts.o)

#This is for comparing data based on 95% bootstrap confidence itnervals 
#(in this case, we are comparing R among treatments)
#I expand this later
# pear.rslts.df <- as.data.frame(pear.rslts$t) %>%
#   mutate(Rac = aba.minr.value - ctl.minr.value,
#          Raf = aba.minr.value - fc.minr.value,
#          Rcf = ctl.minr.value - fc.minr.value
#   )
# View(pear.rslts.df)
# colnames(pear.rslts.df)

#####Table of results for Pear

# pear.table <-
#   t(rbind(c(pear.rslts.o, 
#             Rac = mean(pear.rslts.df$Rac, na.rm = TRUE), 
#             Raf = mean(pear.rslts.df$Raf, na.rm = TRUE),
#             Rcf = mean(pear.rslts.df$Rcf, na.rm = TRUE)),
#           apply(pear.rslts.df, 2, 
#                 function(x) {
#                   quantile(x, c(0.025, 0.975), na.rm = TRUE)}))) %>%
#   as.data.frame() %>%
#   cbind(lbl = row.names(.)) %>%
#   mutate(Treatment = word(lbl, sep = fixed(".")),
#          Variable = word(lbl,start = 2, end = 2, sep = fixed("."))) %>%
#    dplyr::select(Treatment, Variable, Value = V1, CIlo = '2.5%', CIup = '97.5%') #%>%
#    #kable(digits = 3, caption = "Results for Pear")
# #kable is for this internet thing


str(pear.rslts)

##This table below is mine:
colnames(pear.rslts$t) <- names(pear.rslts.o)
#What is t? 
##CHECK IT BEFORE RUNNING THE COLNAMES COMMAND!
str(pear.rslts) #it is a list
names(pear.rslts.o)
pear.rslts.df <- as.data.frame(pear.rslts$t) %>%
  mutate(Rac = aba.minr.value - ctl.minr.value,
         Raf = aba.minr.value - fc.minr.value,
         Rcf = ctl.minr.value - fc.minr.value,
         Qmaxac = aba.maxdmadt.value - ctl.maxdmadt.value,
         Qmaxaf = aba.maxdmadt.value - fc.maxdmadt.value,
         Qmaxcf = ctl.maxdmadt.value - fc.maxdmadt.value, 
         Qmaxac = aba.maxdmadt.value - ctl.maxdmadt.value,
         tQmaxac = aba.tmaxdmadt - ctl.tmaxdmadt,
         tQmaxaf = aba.tmaxdmadt - fc.tmaxdmadt,
         tQmaxcf = ctl.tmaxdmadt - fc.tmaxdmadt,
         tRac = aba.tminr - ctl.tminr,
         tRaf = aba.tminr - fc.tminr,
         tRcf = ctl.tminr - fc.tminr,
         Mdma_ac = aba.maxdma - ctl.maxdma,
         Mdma_af = aba.maxdma - fc.maxdma,
         Mdma_cf = ctl.maxdma - fc.maxdma,
         wpQmax_ac =aba.wpmaxdmadt - ctl.wpmaxdmadt,
         wpQmax_af =aba.wpmaxdmadt - fc.wpmaxdmadt,
         wpQmax_cf =ctl.wpmaxdmadt - fc.wpmaxdmadt,
         wpminr_ac =aba.wpminr - ctl.wpminr,
         wpminr_af = aba.wpminr - fc.wpminr,
         wpminr_cf = ctl.wpminr - fc.wpminr, 
         thalfWP_ac =aba.thalfWP - ctl.thalfWP,
         thalfWP_af = aba.thalfWP - fc.thalfWP,
         thalfWP_cf = ctl.thalfWP - fc.thalfWP,
         thalfh20g_ac = aba.thalfh20g - ctl.thalfh20g,
         thalfh20g_af = aba.thalfh20g - fc.thalfh20g,
         thalfh20g_cf = ctl.thalfh20g - fc.thalfh20g,
         thalfdma_ac = aba.thalfdma - ctl.thalfdma,
         thalfdma_af = aba.thalfdma - fc.thalfdma,
         thalfdma_cf = ctl.thalfdma - fc.thalfdma,
         minwp_ac = aba.minwp - ctl.minwp,
         minwp_af = aba.minwp - fc.minwp,
         minwp_cf = ctl.minwp - fc.minwp,
         maxwp_ac = aba.maxwp.value - ctl.maxwp.value,
         maxwp_af = aba.maxwp.value - fc.maxwp.value,
         maxwp_cf = ctl.maxwp.value - fc.maxwp.value,
         dh20gminr_ac = aba.dh20gminr - ctl.dh20gminr,
         dh20gminr_af = aba.dh20gminr - fc.dh20gminr,
         dh20gminr_cf = ctl.dh20gminr - fc.dh20gminr,
         dMAminr_ac = aba.dMAminr - ctl.dMAminr,
         dMAminr_af = aba.dMAminr - fc.dMAminr,
         dMAminr_cf = ctl.dMAminr - fc.dMAminr,
         Mdh20_ac = aba.maxdh20 - ctl.maxdh20,
         Mdh20_af = aba.maxdh20 - fc.maxdh20,
         Mdh20_cf = ctl.maxdh20 - fc.maxdh20,
         thalf_dmawp_a = aba.thalfdma - aba.thalfWP,
         thalf_dmawp_c = ctl.thalfdma - ctl.thalfWP,
         thalf_dmawp_f = fc.thalfdma - fc.thalfWP)
  
View(pear.rslts.df)
##It works! 
## Fluxes are different since 0 is not in the confidence interval


pear.table2 <-
  t(rbind(c(pear.rslts.o, ##why pear.rslts.o and not df????
            Rac = mean(pear.rslts.df$Rac, na.rm = TRUE), 
            Raf = mean(pear.rslts.df$Raf, na.rm = TRUE),
            Rcf = mean(pear.rslts.df$Rcf, na.rm = TRUE),
            Qmaxac = mean(pear.rslts.df$Qmaxac, na.rm = TRUE),
            Qmaxaf = mean(pear.rslts.df$Qmaxaf, na.rm = TRUE),
            Qmaxcf = mean(pear.rslts.df$Qmaxcf, na.rm = TRUE),
            tQmaxac = mean(pear.rslts.df$tQmaxac, na.rm = TRUE),
            tQmaxaf = mean(pear.rslts.df$tQmaxaf, na.rm = TRUE),
            tQmaxcf = mean(pear.rslts.df$tQmaxcf, na.rm = TRUE),
            tRac = mean(pear.rslts.df$tRac, na.rm = TRUE), 
            tRaf = mean(pear.rslts.df$tRaf, na.rm = TRUE),
            tRcf = mean(pear.rslts.df$tRcf, na.rm = TRUE),
            Mdma_ac = mean(pear.rslts.df$Mdma_ac, na.rm = TRUE),
            Mdma_af = mean(pear.rslts.df$Mdma_af, na.rm = TRUE),
            Mdma_cf = mean(pear.rslts.df$Mdma_af, na.rm = TRUE),
            wpQmax_ac = mean(pear.rslts.df$wpQmax_ac, na.rm = TRUE),
            wpQmax_af = mean(pear.rslts.df$wpQmax_af, na.rm = TRUE),
            wpQmax_cf = mean(pear.rslts.df$wpQmax_cf, na.rm = TRUE),
            wpminr_ac = mean(pear.rslts.df$wpminr_ac, na.rm = TRUE),
            wpminr_af = mean(pear.rslts.df$wpminr_af, na.rm = TRUE),
            wpminr_cf = mean(pear.rslts.df$wpminr_cf, na.rm = TRUE),
            thalfWP_ac =mean(pear.rslts.df$thalfWP_ac, na.rm = TRUE),
            thalfWP_af =mean(pear.rslts.df$thalfWP_af, na.rm = TRUE),
            thalfWP_cf =mean(pear.rslts.df$thalfWP_cf, na.rm = TRUE),
            thalfh20g_ac =mean(pear.rslts.df$thalfh20g_ac, na.rm = TRUE),
            thalfh20g_af =mean(pear.rslts.df$thalfh20g_af, na.rm = TRUE),
            thalfh20g_cf =mean(pear.rslts.df$thalfh20g_cf, na.rm = TRUE),
            thalfdma_ac =mean(pear.rslts.df$thalfdma_ac, na.rm = TRUE),
            thalfdma_af =mean(pear.rslts.df$thalfdma_af, na.rm = TRUE),
            thalfdma_cf =mean(pear.rslts.df$thalfdma_cf, na.rm = TRUE),
            minwp_ac =mean(pear.rslts.df$minwp_ac, na.rm = TRUE),
            minwp_af =mean(pear.rslts.df$minwp_af, na.rm = TRUE),
            minwp_cf =mean(pear.rslts.df$minwp_cf, na.rm = TRUE),
            maxwp_ac =mean(pear.rslts.df$maxwp_ac, na.rm = TRUE),
            maxwp_af =mean(pear.rslts.df$maxwp_af, na.rm = TRUE),
            maxwp_cf =mean(pear.rslts.df$maxwp_cf, na.rm = TRUE),
            dh20gminr_ac =mean(pear.rslts.df$dh20gminr_ac, na.rm = TRUE),
            dh20gminr_af =mean(pear.rslts.df$dh20gminr_af, na.rm = TRUE),
            dh20gminr_cf =mean(pear.rslts.df$dh20gminr_cf, na.rm = TRUE),
            dMAminr_ac =mean(pear.rslts.df$dMAminr_ac, na.rm = TRUE),
            dMAminr_af =mean(pear.rslts.df$dMAminr_af, na.rm = TRUE),
            dMAminr_cf =mean(pear.rslts.df$dMAminr_cf, na.rm = TRUE),
            Mdh20_ac =mean(pear.rslts.df$Mdh20_ac, na.rm = TRUE),
            Mdh20_af =mean(pear.rslts.df$Mdh20_af, na.rm = TRUE),
            Mdh20_cf =mean(pear.rslts.df$Mdh20_cf, na.rm = TRUE),
            thalf_dmawp_a=mean(pear.rslts.df$thalf_dmawp_a, na.rm = TRUE),
            thalf_dmawp_c=mean(pear.rslts.df$thalf_dmawp_c, na.rm = TRUE),
            thalf_dmawp_f=mean(pear.rslts.df$thalf_dmawp_f, na.rm = TRUE)),
          apply(pear.rslts.df, 2, 
                function(x) {
                  quantile(x, c(0.025, 0.975), na.rm = TRUE)}),
          apply(pear.rslts.df, 2, 
                function(x) {
                  sd((x)/sqrt(length(x)), na.rm = TRUE)}),
          apply(pear.rslts.df, 2, 
                function(x) {
                  sd(x, na.rm = TRUE)}))) %>%
  as.data.frame() %>%
  cbind(lbl = row.names(.)) %>%
  mutate(Treatment = word(lbl, sep = fixed(".")),
          Variable = word(lbl,start = 2, end = 2, sep = fixed("."))) %>%
   dplyr::select(Treatment, Variable, Value = V1, CIlo = '2.5%', CIup = '97.5%', se = V4, sd = V5)
View(pear.table2) 

write.csv(pear.table2,'C:/Users/dunaz/Documents/R analyses/FC ABA/Emilio/playing with emilio results/peartablerevNPNONFILTEREDfinal_longernot filtered with thalf.csv')

####THIS IS THE GOOD TABLE!!
#In this table, I have extracted also se and sd



# Figures for both species ------------------------------------------------
####Figures for both species

#mycols <- c(aba = "dodgerblue4", ctl = "darkgreen", fc = "red4")
mycols <- c(aba = "#0072B2", ctl = "#009E73", fc = "#D55E00")
mypch <- c(aba = 22, ctl = 21, fc = 24)
#mypch <- c(aba = 0, ctl = 1, fc = 5)

# Prepare dH2O, WP and dM.A data for Pear figures ####
#Plus, I add flux.......................
pFig <- expand.grid(treat = unique(peard.rw$treat), Time = 0:520) %>%
  split(.$treat) %>%
  Map(cbind, ., dH2O = Map(predict, dH2O.nls1.p, .)) %>%
  Map(cbind, ., WP = Map(predict, WP.nls1.p, .)) %>%
  Map(cbind, ., dM.A = Map(predict, dM.A.nls1.p, .)) %>%
  Map(cbind, ., se.dH2O = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    dH2O.nls1.p, .)) %>%
  Map(cbind, ., se.WP = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    WP.nls1.p, .)) %>%
  Map(cbind, ., se.dM.A = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    dM.A.nls1.p, .)) %>%
  bind_rows()


dH2OT.pear.fig <- ggplot(data = pFig, 
                         aes(y = dH2O, 
                             x = Time, 
                             group = treat, 
                             colour = treat,
                             fill = treat)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = dH2O - 2 * se.dH2O, ymax = dH2O + 2 * se.dH2O), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = peard.rw, 
             aes(y = dH2O, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0, 0.13) +
  xlab("Time (min)") +
  ylab("Water absorbed per unit leaf dry mass") +
  theme_bw() + 
  theme(legend.position = "none")

dH2OT.pear.fig

# Prepare data for Conductance Kappa for figures ####

# Run simulations ####
system.time(
  Kap.p <- boot(data = peard.rw, 
                statistic = Kap.fun, 
                R = 2000, 
                parallel = "multicore",
                ncpus = 8,
                dma.mod = dM.A.nls1.p,
                wp.mod = WP.nls1.p)
)

# Organize results from bootstrap simulations ####
Kap.p <- apply(Kap.p$t, 2, function(x) {
  quantile(x, c(0.025, 0.975), na.rm = TRUE)}) %>%
  rbind(Kap = Kap.p$t0, .) %>%
  t() %>%
  as.data.frame() %>%
  cbind(treat = rep(c("aba", "ctl", "fc"), each = 150),
        Time = rep((1:150) * 3, 3), .)

names(Kap.p)[4:5] <- c("CIlo", "CIup")
str(Kap.p)

# Plot Water gain per unit dry mass ~ Time  ####
dH2OT.pear.fig <- ggplot(data = pFig, 
                         aes(y = dH2O, 
                             x = Time, 
                             group = treat, 
                             colour = treat,
                             fill = treat)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = dH2O - 2 * se.dH2O, ymax = dH2O + 2 * se.dH2O), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = peard.rw, 
             aes(y = dH2O, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0, 0.13) +
  xlab("Time (min)") +
  ylab("Water absorbed per unit leaf dry mass") +
  theme_bw() + 
  theme(legend.position = "none")

# Plot Water gain per unit leaf area ~ Time ####
dM.AT.pear.fig <- ggplot(data = pFig,
                         aes(y = dM.A, 
                             x = Time, 
                             group = treat, 
                             colour = treat,
                             fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = dM.A - 2 * se.dM.A, ymax = dM.A + 2 * se.dM.A), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = peard.rw, 
             aes(y = dM.A, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0, 15) +
  xlab("") +
  ylab(expression(paste(""))) +
  ggtitle("P. communis") +
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face="italic"))+
  theme(axis.text.y =element_blank(),
        axis.title.y =element_blank(),
        axis.text.x =element_blank(),
        axis.title.x =element_blank()) +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines")) 

dM.AT.pear.fig
# Plot Conductance ~ time ####
dKapT.pear.fig <- ggplot(data = Kap.p, 
                         aes(y = Kap, 
                             x = Time, 
                             group = treat, 
                             colour = treat,
                             fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = CIlo, ymax = CIup), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  xlim(0, 450) +
  ylim(0, 0.0045) +
  xlab("Time (min)") +
  ylab(expression(paste(""))) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=16), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("") +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines")) 
dKapT.pear.fig
##ylab(expression(paste(K[surf], " (g ", m^-2 , s^-1 , MPa^-1,")")))
# Plot Water potential ~ Time ####
WP.pear.fig <- ggplot(data = pFig,
                      aes(y = WP, 
                          x = Time, 
                          group = treat, 
                          colour = treat,
                          fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = WP - 2 * se.WP, ymax = WP + 2 * se.WP), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = peard.rw, 
             aes(y = WP, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0.20, 2.3) +
  xlab("") +
  ylab(expression(paste(""))) + 
  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.y =element_blank(),
        axis.title.y =element_blank(),
        axis.text.x =element_blank(),
        axis.title.x =element_blank()) +
  ggtitle("") +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines")) 
WP.pear.fig

ggarrange(dM.AT.pear.fig,
          WP.pear.fig, 
          dKapT.pear.fig,
          label.x = c(0.88, 0.88),
          label.y = 0.9,
          font.label = list(size = 12, face = "bold"),
          nrow = 3)



##### SAME FOR ALMOND
# Load data (almond data)
almond.rw <- read.table("C:/Users/dunaz/Documents/R analyses/FC ABA/Emilio/playing with emilio results/allAlmond15Mar19.txt", header = TRUE, sep= ',')
#This is a new data table (fc and control wp corrected)
View(almond.rw)
names(almond.rw)
almond.rw <- almond.rw %>%
  mutate(mass0 = mass0/1000) %>% #in g
  mutate(massT = massT/1000) %>% #in g
  mutate(area = area/10000) %>% #in m2
  mutate(dry.mass = dry.mass/1000) %>% #in g
  mutate(H2O.0 = (mass0 - dry.mass) / dry.mass) %>% 
  mutate(H2O.T = (massT - dry.mass) / dry.mass) %>%
  mutate(dH2O = H2O.T - H2O.0) %>% #in g g-1
  mutate(dM = massT - mass0) %>% #in g
  mutate(dM.A = dM / area) %>% #in g m-2
  filter(Time>0) 
#%>%
  #filter(WP > 0.36) #to exclude two wierd values of fc




# Almond dM.A exponential model ######
dM.A.nls1.a <- nlsList(dM.A ~ exp(A) * (1 - exp(-B*Time^2)) | treat,
                       data = almond.rw,
                       start = c(A = 2.366752, 
                                 B = 6.121328e-05))
#values still changed to control filtered
plot(dM.A.nls1.a)
summary(dM.A.nls1.a)
cor(fitted(dM.A.nls1.a), almond.rw$dM.A)^2

# Almond dH2O exponential model ######

dH2O.nls1.a <- nlsList(dH2O ~ exp(A) * (1 - exp(-B*Time^2)) | treat,
                       data = almond.rw,
                       start = c(A = -2.018421, 
                                 B = 6.587850e-05))

plot(dH2O.nls1.a)
summary(dH2O.nls1.a)
cor(fitted(dH2O.nls1.a), almond.rw$dH2O)^2

# Almond WP exponential model ######

WP.nls1.a <- nlsList(
  WP ~ A + exp(B0 + B2 * Time^2) | treat,
  data = almond.rw,
  start = c(A = 0.5795063,
            B0 = 0.03032546,
            B2 = -5.009529e-05),
  control = nls.control(maxiter = 1000, minFactor = 1e-5, tol = 1e-5))

#starging values changed to control not filtered and with changes in wp
plot(WP.nls1.a)
summary(WP.nls1.a)
cor(fitted(WP.nls1.a), almond.rw$WP)^2

newd <- expand.grid(treat = unique(almond.rw$treat), Time = 0:520) %>%
  split(., .$treat) %>%
  Map(cbind, ., dM.A = Map(predict, dM.A.nls1.a, .)) %>%
  Map(cbind, ., dH2O = Map(predict, dH2O.nls1.a, .)) %>%
  Map(cbind, ., WP = Map(predict, WP.nls1.a, .))

scatterplot(dM.A ~ Time | treat, almond.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$dM.A ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$dM.A ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$dM.A ~ newd$fc$Time, lwd = 2, col = "lightblue")

scatterplot(dH2O ~ Time | treat, almond.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$dH2O ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$dH2O ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$dH2O ~ newd$fc$Time, lwd = 2, col = "lightblue")

scatterplot(WP ~ Time | treat, almond.rw, regLine = FALSE, smooth = FALSE)
lines(newd$aba$WP ~ newd$aba$Time, lwd = 2, col = "blue")
lines(newd$ctl$WP ~ newd$ctl$Time, lwd = 2, col = "pink")
lines(newd$fc$WP ~ newd$fc$Time, lwd = 2, col = "lightblue")

# Prepare the data set for the boot function ####

almond.rw$pdM.A <- fitted(dM.A.nls1.a)
almond.rw$pdH2O <- fitted(dH2O.nls1.a)
almond.rw$pWP <- fitted(WP.nls1.a)
almond.rw$resdM.A <- residuals(dM.A.nls1.a)
almond.rw$resdH2O <- residuals(dH2O.nls1.a)
almond.rw$resWP <- residuals(WP.nls1.a)

# Run on original data ####

almond.rslts.o <- parm.fun3(data = almond.rw, 
                            i = 1:nrow(almond.rw),
                            dma.mod = dM.A.nls1.a,
                            h20.mod = dH2O.nls1.a,
                            wp.mod = WP.nls1.a)
# Run simulations ####
system.time(
  almond.rslts <- boot(data = almond.rw, 
                       statistic = parm.fun3, 
                       R = 5000, 
                       parallel = "multicore",
                       ncpus = 8,
                       dma.mod = dM.A.nls1.a,
                       h20.mod = dH2O.nls1.a,
                       wp.mod = WP.nls1.a)
)

# Prep results 
colnames(almond.rslts$t) <- names(almond.rslts.o)

almond.rslts.df <- as.data.frame(almond.rslts$t) %>%
  mutate(Rac = aba.minr.value - ctl.minr.value,
         Raf = aba.minr.value - fc.minr.value,
         Rcf = ctl.minr.value - fc.minr.value
  )

#Almond table
almond.table <-
  t(rbind(c(almond.rslts.o, 
            Rac = mean(almond.rslts.df$Rac, na.rm = TRUE), 
            Raf = mean(almond.rslts.df$Raf, na.rm = TRUE),
            Rcf = mean(almond.rslts.df$Rcf, na.rm = TRUE)),
          apply(almond.rslts.df, 2, 
                function(x) {
                  quantile(x, c(0.025, 0.975), na.rm = TRUE)}))) %>%
  as.data.frame() %>%
  cbind(lbl = row.names(.)) %>%
  mutate(Treatment = word(lbl, sep = fixed(".")),
         Variable = word(lbl,start = 2, end = 2, sep = fixed("."))) %>%
  dplyr::select(Treatment, Variable, Value = V1, CIlo = '2.5%', CIup = '97.5%') %>%
  kable(digits = 3, caption = "Results for almond")

# Prepare dH2O, WP and dM.A data for almond figures ####
aFig <- expand.grid(treat = unique(almond.rw$treat), Time = 0:520) %>%
  split(.$treat) %>%
  Map(cbind, ., dH2O = Map(predict, dH2O.nls1.a, .)) %>%
  Map(cbind, ., WP = Map(predict, WP.nls1.a, .)) %>%
  Map(cbind, ., dM.A = Map(predict, dM.A.nls1.a, .)) %>%
  Map(cbind, ., se.dH2O = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    dH2O.nls1.a, .)) %>%
  Map(cbind, ., se.WP = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    WP.nls1.a, .)) %>%
  Map(cbind, ., se.dM.A = Map(function(x, y) {
    predFit(object = x, 
            newdata = y, 
            se.fit = TRUE)$se.fit}, 
    dM.A.nls1.a, .)) %>%
  bind_rows()

# Prepare data for Conductance Kappa for figures ####

# Run simulations ####
system.time(
  Kap.a <- boot(data = almond.rw, 
                statistic = Kap.fun, 
                R = 2000, 
                parallel = "multicore",
                ncpus = 8,
                dma.mod = dM.A.nls1.a,
                wp.mod = WP.nls1.a)
)


###trying for almond

#####trying for almond table with all values
colnames(almond.rslts$t) <- names(almond.rslts.o)
names(almond.rslts.o)
View(almond.rslts.o)
almond.rslts.df <- as.data.frame(almond.rslts$t) %>%
  mutate(Rac = aba.minr.value - ctl.minr.value,
         Raf = aba.minr.value - fc.minr.value,
         Rcf = ctl.minr.value - fc.minr.value,
         Qmaxac = aba.maxdmadt.value - ctl.maxdmadt.value,
         Qmaxaf = aba.maxdmadt.value - fc.maxdmadt.value,
         Qmaxcf = ctl.maxdmadt.value - fc.maxdmadt.value, 
         Qmaxac = aba.maxdmadt.value - ctl.maxdmadt.value,
         tQmaxac = aba.tmaxdmadt - ctl.tmaxdmadt,
         tQmaxaf = aba.tmaxdmadt - fc.tmaxdmadt,
         tQmaxcf = ctl.tmaxdmadt - fc.tmaxdmadt,
         tRac = aba.tminr - ctl.tminr,
         tRaf = aba.tminr - fc.tminr,
         tRcf = ctl.tminr - fc.tminr,
         Mdma_ac = aba.maxdma - ctl.maxdma,
         Mdma_af = aba.maxdma - fc.maxdma,
         Mdma_cf = ctl.maxdma - fc.maxdma,
         wpQmax_ac =aba.wpmaxdmadt - ctl.wpmaxdmadt,
         wpQmax_af =aba.wpmaxdmadt - fc.wpmaxdmadt,
         wpQmax_cf =ctl.wpmaxdmadt - fc.wpmaxdmadt,
         wpminr_ac =aba.wpminr - ctl.wpminr,
         wpminr_af = aba.wpminr - fc.wpminr,
         wpminr_cf = ctl.wpminr - fc.wpminr, 
         thalfWP_ac =aba.thalfWP - ctl.thalfWP,
         thalfWP_af = aba.thalfWP - fc.thalfWP,
         thalfWP_cf = ctl.thalfWP - fc.thalfWP,
         thalfh20g_ac = aba.thalfh20g - ctl.thalfh20g,
         thalfh20g_af = aba.thalfh20g - fc.thalfh20g,
         thalfh20g_cf = ctl.thalfh20g - fc.thalfh20g,
         thalfdma_ac = aba.thalfdma - ctl.thalfdma,
         thalfdma_af = aba.thalfdma - fc.thalfdma,
         thalfdma_cf = ctl.thalfdma - fc.thalfdma,
         minwp_ac = aba.minwp - ctl.minwp,
         minwp_af = aba.minwp - fc.minwp,
         minwp_cf = ctl.minwp - fc.minwp,
         maxwp_ac = aba.maxwp.value - ctl.maxwp.value,
         maxwp_af = aba.maxwp.value - fc.maxwp.value,
         maxwp_cf = ctl.maxwp.value - fc.maxwp.value,
         dh20gminr_ac = aba.dh20gminr - ctl.dh20gminr,
         dh20gminr_af = aba.dh20gminr - fc.dh20gminr,
         dh20gminr_cf = ctl.dh20gminr - fc.dh20gminr,
         dMAminr_ac = aba.dMAminr - ctl.dMAminr,
         dMAminr_af = aba.dMAminr - fc.dMAminr,
         dMAminr_cf = ctl.dMAminr - fc.dMAminr,
         Mdh20_ac = aba.maxdh20 - ctl.maxdh20,
         Mdh20_af = aba.maxdh20 - fc.maxdh20,
         Mdh20_cf = ctl.maxdh20 - fc.maxdh20,
         thalf_dmawp_a = aba.thalfdma - aba.thalfWP,
         thalf_dmawp_c = ctl.thalfdma - ctl.thalfWP,
         thalf_dmawp_f = fc.thalfdma - fc.thalfWP)
View(almond.rslts.df)

almond.table2 <-
  t(rbind(c(almond.rslts.o, 
            Rac = mean(almond.rslts.df$Rac, na.rm = TRUE), 
            Raf = mean(almond.rslts.df$Raf, na.rm = TRUE),
            Rcf = mean(almond.rslts.df$Rcf, na.rm = TRUE),
            Qmaxac = mean(almond.rslts.df$Qmaxac, na.rm = TRUE),
            Qmaxaf = mean(almond.rslts.df$Qmaxaf, na.rm = TRUE),
            Qmaxcf = mean(almond.rslts.df$Qmaxcf, na.rm = TRUE),
            tQmaxac = mean(almond.rslts.df$tQmaxac, na.rm = TRUE),
            tQmaxaf = mean(almond.rslts.df$tQmaxaf, na.rm = TRUE),
            tQmaxcf = mean(almond.rslts.df$tQmaxcf, na.rm = TRUE),
            tRac = mean(almond.rslts.df$tRac, na.rm = TRUE), 
            tRaf = mean(almond.rslts.df$tRaf, na.rm = TRUE),
            tRcf = mean(almond.rslts.df$tRcf, na.rm = TRUE),
            Mdma_ac = mean(almond.rslts.df$Mdma_ac, na.rm = TRUE),
            Mdma_af = mean(almond.rslts.df$Mdma_af, na.rm = TRUE),
            Mdma_cf = mean(almond.rslts.df$Mdma_af, na.rm = TRUE),
            wpQmax_ac = mean(almond.rslts.df$wpQmax_ac, na.rm = TRUE),
            wpQmax_af = mean(almond.rslts.df$wpQmax_af, na.rm = TRUE),
            wpQmax_cf = mean(almond.rslts.df$wpQmax_cf, na.rm = TRUE),
            wpminr_ac = mean(almond.rslts.df$wpminr_ac, na.rm = TRUE),
            wpminr_af = mean(almond.rslts.df$wpminr_af, na.rm = TRUE),
            wpminr_cf = mean(almond.rslts.df$wpminr_cf, na.rm = TRUE),
            thalfWP_ac =mean(almond.rslts.df$thalfWP_ac, na.rm = TRUE),
            thalfWP_af =mean(almond.rslts.df$thalfWP_af, na.rm = TRUE),
            thalfWP_cf =mean(almond.rslts.df$thalfWP_cf, na.rm = TRUE),
            thalfh20g_ac =mean(almond.rslts.df$thalfh20g_ac, na.rm = TRUE),
            thalfh20g_af =mean(almond.rslts.df$thalfh20g_af, na.rm = TRUE),
            thalfh20g_cf =mean(almond.rslts.df$thalfh20g_cf, na.rm = TRUE),
            thalfdma_ac =mean(almond.rslts.df$thalfdma_ac, na.rm = TRUE),
            thalfdma_af =mean(almond.rslts.df$thalfdma_af, na.rm = TRUE),
            thalfdma_cf =mean(almond.rslts.df$thalfdma_cf, na.rm = TRUE),
            minwp_ac =mean(almond.rslts.df$minwp_ac, na.rm = TRUE),
            minwp_af =mean(almond.rslts.df$minwp_af, na.rm = TRUE),
            minwp_cf =mean(almond.rslts.df$minwp_cf, na.rm = TRUE),
            maxwp_ac =mean(almond.rslts.df$maxwp_ac, na.rm = TRUE),
            maxwp_af =mean(almond.rslts.df$maxwp_af, na.rm = TRUE),
            maxwp_cf =mean(almond.rslts.df$maxwp_cf, na.rm = TRUE),
            dh20gminr_ac =mean(almond.rslts.df$dh20gminr_ac, na.rm = TRUE),
            dh20gminr_af =mean(almond.rslts.df$dh20gminr_af, na.rm = TRUE),
            dh20gminr_cf =mean(almond.rslts.df$dh20gminr_cf, na.rm = TRUE),
            dMAminr_ac =mean(almond.rslts.df$dMAminr_ac, na.rm = TRUE),
            dMAminr_af =mean(almond.rslts.df$dMAminr_af, na.rm = TRUE),
            dMAminr_cf =mean(almond.rslts.df$dMAminr_cf, na.rm = TRUE),
            Mdh20_ac =mean(almond.rslts.df$Mdh20_ac, na.rm = TRUE),
            Mdh20_af =mean(almond.rslts.df$Mdh20_af, na.rm = TRUE),
            Mdh20_cf =mean(almond.rslts.df$Mdh20_cf, na.rm = TRUE),
            thalf_dmawp_a=mean(pear.rslts.df$thalf_dmawp_a, na.rm = TRUE),
            thalf_dmawp_c=mean(pear.rslts.df$thalf_dmawp_c, na.rm = TRUE),
            thalf_dmawp_f=mean(pear.rslts.df$thalf_dmawp_f, na.rm = TRUE)),
          apply(almond.rslts.df, 2, 
                function(x) {
                  quantile(x, c(0.025, 0.975), na.rm = TRUE)}),
          apply(almond.rslts.df, 2, 
                function(x) {
                  sd((x)/sqrt(length(x)), na.rm = TRUE)}),
          apply(almond.rslts.df, 2, 
                function(x) {
                  sd(x, na.rm = TRUE)}))) %>%
  as.data.frame() %>%
  cbind(lbl = row.names(.)) %>%
  mutate(Treatment = word(lbl, sep = fixed(".")),
         Variable = word(lbl,start = 2, end = 2, sep = fixed("."))) %>%
  dplyr::select(Treatment, Variable, Value = V1, CIlo = '2.5%', CIup = '97.5%', se = V4, sd = V5)
View(almond.table2)

write.csv(almond.table2,'C:/Users/dunaz/Documents/R analyses/FC ABA/Emilio/playing with emilio results/almondtablerevNP_NOTFILTERED_longerwith thalf.csv')


# Organize results from bootstrap simulations ####
Kap.a <- apply(Kap.a$t, 2, function(x) {
  quantile(x, c(0.025, 0.975), na.rm = TRUE)}) %>%
  rbind(Kap = Kap.a$t0, .) %>%
  t() %>%
  as.data.frame() %>%
  cbind(treat = rep(c("aba", "ctl", "fc"), each = 150),
        Time = rep((1:150) * 3, 3), .)

names(Kap.a)[4:5] <- c("CIlo", "CIup")
str(Kap.a)

# Plot Water gain per unit dry mass ~ Time  ####
dH2OT.almond.fig <- ggplot(data = aFig, 
                           aes(y = dH2O, 
                               x = Time, 
                               group = treat, 
                               colour = treat,
                               fill = treat)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = dH2O - 2 * se.dH2O, ymax = dH2O + 2 * se.dH2O), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = almond.rw, 
             aes(y = dH2O, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0, 0.13) +
  xlab("Time (min)") +
  ylab("Water absorbed per unit leaf dry mass") +
  theme_bw() + 
  theme(legend.position = "none")

# Plot Water gain per unit leaf area ~ Time ####
dM.AT.almond.fig <- ggplot(data = aFig,
                           aes(y = dM.A, 
                               x = Time, 
                               group = treat, 
                               colour = treat,
                               fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = dM.A - 2 * se.dM.A, ymax = dM.A + 2 * se.dM.A), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = almond.rw, 
             aes(y = dM.A, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0, 15) +
  xlab("") +
  ylab(expression(paste(Delta, "M (g ", m^-2,")"))) +
  ggtitle("P. dulcis") +
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5, size=16, face="italic")) +
  theme(axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines"))  
dM.AT.almond.fig

# Plot Conductance ~ time ####
dKapT.almond.fig <- ggplot(data = Kap.a, 
                           aes(y = Kap, 
                               x = Time, 
                               group = treat, 
                               colour = treat,
                               fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = CIlo, ymax = CIup), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  xlim(0, 450) +
  ylim(0, 0.0045) +
  xlab("Time (min)") +
  ylab(expression(paste("K"["surf"] , " (g ", m^-2 , s^-1 , MPa^-1,")"))) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16)) +
  ggtitle("") +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines")) 

dKapT.almond.fig
#ylab(expression(paste(K[surf], " (g ", m^-2 , s^-1 , MPa^-1,")")))
# Plot Water potential ~ Time ####
WP.almond.fig <- ggplot(data = aFig,
                        aes(y = WP, 
                            x = Time, 
                            group = treat, 
                            colour = treat,
                            fill = treat)) + 
  geom_line(size = 1) +
  scale_color_manual(values = mycols) + 
  geom_ribbon(aes(ymin = WP - 2 * se.WP, ymax = WP + 2 * se.WP), 
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = mycols) + 
  geom_point(data = almond.rw, 
             aes(y = WP, x = Time, 
                 shape = treat), size = 2) + 
  scale_shape_manual(values = mypch) + 
  xlim(0, 450) +
  ylim(0.20, 2.3) +
  xlab("") +
  ylab(expression(paste("-", Psi, " (MPa)"))) +
   theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  ggtitle("") +
  theme(plot.margin = unit(c(0,0.001,0,0), "lines")) 


#+
 # theme_minimal() + # theme(panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_rect(colour = "black", size=1, fill=NA))
WP.almond.fig

ggarrange(dM.AT.almond.fig, 
           
          WP.almond.fig, 
          
          dKapT.almond.fig, 
          
          label.x = c(0.88, 0.88),
          label.y = 0.9,
          
          font.label = list(size = 12, face = "bold"),
          nrow = 3)
# ggarrange(dM.AT.pear.fig + rremove("x.text") + rremove("xlab"), 
#           dM.AT.almond.fig + rremove("xy.text") + rremove("xylab"), 
#           WP.pear.fig + rremove("x.text") + rremove("xlab"), 
#           WP.almond.fig + rremove("xy.text") + rremove("xylab"), 
#           dKapT.pear.fig, 
#           dKapT.almond.fig + rremove("y.text") + rremove("ylab"), 
#           labels = c("P. communis", "P. dulcis"),
#           label.x = c(0.07, 0.04),
#           label.y = 0.93,
#           ncol = 2, nrow = 3)

# ggarrange(dM.AT.almond.fig + rremove("x.text") + rremove("xlab"), 
#           dM.AT.pear.fig + rremove("xy.text") + rremove("xylab"), 
#           WP.almond.fig + rremove("x.text") + rremove("xlab"), 
#           WP.pear.fig + rremove("xy.text") + rremove("xylab"), 
#           dKapT.almond.fig, 
#           dKapT.pear.fig + rremove("y.text") + rremove("ylab"),
#           label.x = c(0.07, 0.04),
#           label.y = 0.93,
#           ncol = 2, nrow = 3,
#           align = "hv")
ggarrange(dM.AT.almond.fig, 
          dM.AT.pear.fig, 
          WP.almond.fig, 
          WP.pear.fig, 
          dKapT.almond.fig, 
          dKapT.pear.fig,
          label.x = c(0.88, 0.88),
          label.y = 0.9,
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
          font.label = list(size = 14, face = "bold"),
          ncol = 2, nrow = 3,
          align = "hv")
#annotate_figure(final, top.left = text_grob("P. dulcis"), top.left = text_grob("P. communis"))
# ggarrange(dM.AT.almond.fig , 
#           dM.AT.pear.fig, 
#           WP.almond.fig , 
#           WP.pear.fig , 
#           dKapT.almond.fig, 
#           dKapT.pear.fig ,
#           label.x = c(0.07, 0.04),
#           label.y = 2.93,
#           ncol = 2, nrow = 3,
#           align = "v")

#I print 700 900
#When I try to modify margins with negative and I maybe I have align, it doesn't work.
### CHECK LINE SIZE AND THE BORDER OF THE PANEL, MAYBE I HAVE TO REMOVE ALL 
#BORDERS AND THEN ADD BACKGROUND. IT DOESN'T WORK EITHER. LINES AT THE 
#BOTTOM AND RIGHT ARE THICKER. IT SEEMS A COMMON PROBLEM. MAYBE IF I SAVE
#IT IN PDF IT IS NOT AS NOTICEABLE.
#pdf(xxxx)
View(peard.rw)

