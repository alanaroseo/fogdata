library(lavaan)
library(AICcmodavg)

L <- as.data.frame(read.delim("clipboard"))
names(L)

LG <- as.data.frame(read.delim("clipboard"))
names(LG)

G <- as.data.frame(read.delim("clipboard"))
names(G)
#fix scales
LG$ab_blue <- LG$ab_blue/10000
LG$ab_blue <- LG$ab_blue/10000

# modeling each composite variable ----------------------------------------

#GL surface resistance
re_mod <- 'uptake.rate~ ab.W.per.A +ab_blue+ab_wax '

re_mod_fitted <- sem(re_mod, data=LG)


summary(re_mod_fitted)

mod2 <- 'resist<~ -39.357*ab.W.per.A +ab_blue
uptake.rate~ resist
uptake.rate~MPa_i
ab.W.per.A ~~ab_blue
'
#no sig covariance between resist and MPa

mod_fitted <- sem(mod2, data=LG,check.gradient = FALSE)

summary(mod_fitted, rsq = T, standardized=T)
varTable(mod_fitted)

names(G)
cor(G$uptake.rate,G$MPa_i)


# Lsurface resistance

res_mod <- 'uptake.rate~ ab.W.per.A +ab_stomatal.density '

rs_mod_fitted <- sem(res_mod, data=L)


summary(rs_mod_fitted)
#L model non transformed

modL <- 'resist<~ -9.591*ab.W.per.A +ab_stomatal.density
uptake.rate~ resist
uptake.rate~MPa_i
ab.W.per.A ~~ab_stomatal.density
'
#no sig covariance between resist and MPa

modL_fitted <- sem(modL, data=L,check.gradient = FALSE)

summary(modL_fitted, rsq = T, standardized=T)
varTable(mod1_fitted)


#G suface resistance
reG_mod <- 'uptake.rate~ ab.W.per.A +ad_cute+ad_sur_lum+ab_sur_lum '

rG_mod_fitted <- sem(reG_mod, data=G)


summary(rG_mod_fitted, rsq = T, standardized=T)


modG <- 'resist<~ -5.181* ab.W.per.A+ad_cute
uptake.rate~ resist
uptake.rate~ MPa_i
'


modG_fitted <- sem(modG, data=G,check.gradient = FALSE)

summary(modG_fitted, rsq = T, standardized=T)
varTable(modG_fitted)


#ln transformed MPa, lower R2 better p values 
L$MPa_i_ln <- log(L$MPa_i)

mod1 <- 'resist<~ -9.591*ab.W.per.A +ab_stomatal.density
uptake.rate~ resist
uptake.rate~MPa_i
ab.W.per.A ~~ab_stomatal.density
'
#no sig covariance between resist and MPa

mod1_fitted <- sem(mod1, data=L,check.gradient = FALSE)

summary(mod1_fitted, rsq = T, standardized=T)
varTable(mod1_fitted)

mod2 <- 'resist<~ -3.366*ab.W.per.A +ab_cute
uptake.rate~ resist
ab.W.per.A ~~ab_cute
'

mod_fitted2 <- sem(mod2, data=L,check.gradient = FALSE)

summary(mod_fitted2, rsq = T)
varTable(mod_fitted2)

both <- lm(uptake.rate ~ ab.W.per.A +ab_cute,  data=L)
summary(both)


# instructions for lavaan -------------------------------------------------


#Syntax
#establish model: mod <- 'declare all variables, regressions, covariance, errors,and intercepts' bounded by ''
# fit model: sem_mod <- sem(mod, data=mydata)
#latent variable definition =~ is measured by
#composite variable <~ is caused by
#regression ~ is regressed on 
#(co)variance ~~ is correlated with
#intercept~ 1 intercept

#for example:
##mod <- ’ # regressions
y1 + y2 ~ f1 + f2 + x1 + x2
f1 ~ f2 + f3
f2 ~ f3 + x1 + x2
# latent variable definitions
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f3 =~ y7 + y8 + y9 + y10
# variances and covariances
y1 ~~ y1#variance
y2~~ .1*y2# fix variance of y2 to .1
y1 ~~ y2#covariance
'

##with composite variables##
# mod <- 'comp <~ 1*x1+x2           #1*x1 sets the loading on x1 to 1, better to use the parameter estimate from multiple linear regression instead of 1
response~comp
'



#Missing values
#If the data contain missing values, the default behavior is listwise deletion.  If the missingmechanism  is  MCAR  (missing  completely  at  random)  or  MAR  (missing  at  random),  the  lavaan  packageprovides case-wise (or ‘full information’) maximum likelihood estimation. You can turn this feature on, by usingthe argument missing = "FIML" for full information maximum likelihood, or missing ="ML". Use following the data in the sem() function

#Bootstrapping
#There are two ways for using the bootstrap in lavaan. Either you can set se = "bootstrap" ortest = "bootstrap" when fitting the model (and you will get bootstrap standard errors, and/or a bootstrapbased p-value respectively), or you can you the bootstrapLavaan()function, which needs an already fittedlavaan object.  The latter function can be used to ‘bootstrap’ any statistic (or vector of statistics) that youcan extract from a fitted lavaan object.

#extracting model results
#The summary()function gives a nice overview of a fitted model, but is for display only.  
#coef()function which extracts the estimated parameters of a fitted model.  #parameterEstimates() function extracts not only the values of the estimated parameters, but also the standard errors, the z-values, the standardized parameter values, and returns everythingconveniently as a data frame. 
#fitted.valuesThe fitted()and fitted.values()functions return the model-implied (fitted) covariancematrix (and mean vector) of a fitted model
#The function vcov()returns the estimated covariance matrix of the parameter estimates.
#AIC()and BIC()functions return the AIC and BIC values of a fitted model
#The fitMeasures()function  returns  all  the  fit  measures  computed  by  lavaan  as  a  namednumeric vector










####### modeling each composite -------------------------------------------------


#Surface resistance
