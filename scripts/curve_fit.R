model <- lm(y_var ~ poly(x_var,3)) #poly = polynominal 3=number of parameters (order)

summary(model)#

model <- lm( data= fog, rel_w_dif ~ poly(Minutes,3))

model_lin <- lm( data= fog, rel_w_dif ~ Minutes)
summary(model_lin)

plot(model)

stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 3, raw=TRUE),colour="red")#add 3rd order polynominal trendline to ggplot

#display polynominal equation :
library(polynom)

  
  my.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)#name a form for equation
  
  m <- lm(my.formula, fog)
  my.eq <- as.character(signif(as.polynomial(coef(m)), 3))



my.eq#display equation in console


#now to add to plot:
+annotate(geom = "text", x = 100, y = 10, label = my.eq, #example version giving x and y coordinates for lable placement
          family = "serif", hjust = 0, parse = TRUE, size = 4)

################################################################
#polynominal curve fitting for all L and G samples using relative weight gain

# all L samples

model_L <- lm( data= L_all, rel_w_dif ~ poly(Minutes,3))
summary(model_L)

L.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
a <- lm(L.formula,  L_all)
L.eq <- as.character(signif(as.polynomial(coef(a)), 3))
L.eq

# all G samples

model_G <- lm( data= G_all, rel_w_dif ~ poly(Minutes,3))
summary(model_L)

G.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
b <- lm(G.formula,  G_all)
G.eq <- as.character(signif(as.polynomial(coef(b)), 3))
G.eq

# all T6L samples

model_T6L <- lm( data= T6_L, rel_w_dif ~ poly(Minutes,3))
summary(model_T6L)

T6L.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
d <- lm(T6L.formula,  T6_L)
T6L.eq <- as.character(signif(as.polynomial(coef(d)), 3))
T6L.eq

# all T8L samples

model_T8L <- lm( data= T8_L, rel_w_dif ~ poly(Minutes,3))
summary(model_T8L)

T8L.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
e <- lm(T8L.formula,  T8_L)
T8L.eq <- as.character(signif(as.polynomial(coef(e)), 3))
T8L.eq

# all T11L samples

model_T11L <- lm( data= T11_L, rel_w_dif ~ poly(Minutes,3))
summary(model_T11L)

T11L.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
f <- lm(T11L.formula,  T11_L)
T11L.eq <- as.character(signif(as.polynomial(coef(f)), 3))
T11L.eq

# all T16L samples

model_T16L <- lm( data= T16_L, rel_w_dif ~ poly(Minutes,3))
summary(model_T16L)

T16L.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
g <- lm(T16L.formula,  T16_L)
T16L.eq <- as.character(signif(as.polynomial(coef(g)), 3))
T16L.eq

# all T6G samples

model_T6G <- lm( data= T6_G, rel_w_dif ~ poly(Minutes,3))
summary(model_T6G)

T6G.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
h <- lm(T6G.formula,  T6_G)
T6G.eq <- as.character(signif(as.polynomial(coef(h)), 3))
T6G.eq

# all T8G samples

model_T8G <- lm( data= T8_G, rel_w_dif ~ poly(Minutes,3))
summary(model_T8G)

T8G.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
i <- lm(T8G.formula,  T8_G)
T8G.eq <- as.character(signif(as.polynomial(coef(i)), 3))
T8G.eq

# all T11G samples

model_T11G <- lm( data= T11_G, rel_w_dif ~ poly(Minutes,3))
summary(model_T11G)

T11G.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
J <- lm(T11G.formula,  T11_G)
T11G.eq <- as.character(signif(as.polynomial(coef(j)), 3))
T11G.eq

# all T16G samples

model_T16G <- lm( data= T16_G, rel_w_dif ~ poly(Minutes,3))
summary(model_T16G)

T16G.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)
k <- lm(T16G.formula,  T16_G)
T16G.eq <- as.character(signif(as.polynomial(coef(k)), 3))
T16G.eq