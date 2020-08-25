
#predicting photosythesis
#A = mmol CO2 m-2 s-1
#A=7.309*log(height) - 23.612 #note that log in R is ln

#equation for total leaf area:van pelt et al 2016?
#LA = x*height....blah
#y = -0.0819x2 + 10.867x - 176.95#stand in



####### calculate estimated productivity given different potential G fractions across all heights  based on total LA and photosythesis (TSF-respiration penalty)

######Make a function and calculate a z matrix

productivity <- function(height, G_frac){
  ((-0.0819*height^2) + (10.867*height) - 176.95)*((7.309*log(height) - 23.612)*(1/G_frac))
}    #the function "productivity" calculates the estimated range of photosynthetic output across the leaf investment space (all combos of height and G fraction)
photosynth <- outer(G_frac,height,productivity)#outer() function applies the function "productivity" at every combination of gs and VPD. Seconds is the z axis (a matrix)
rownames(photosynth) = height
colnames(photosynth) = G_frac
class(photosynth)
####### calculate uptake (hydraulic flux)given different potential G fractions across all heights  

######Make a function and calculate a z matrix
#uptake for L leaves = 0.0017*height^2 - 0.1135*height + 5.8981
#uptake for G leaves = 22.85 g/m^2/min

up <- function(height, G_frac){
  ((0.0017*height^2 - 0.1135*height + 5.8981)*((-0.0819*height^2 + 10.867*height - 176.95)*(1/G_frac)))+(22.85*((-0.0819*height^2 + 10.867*height - 176.95)*G_frac))
}    #the function "up" calculates the estimated range of foliar water uptake across the leaf investment space (all combos of height and G fraction)
uptake <- outer(G_frac,height,productivity)#outer() function applies the function "up" at every combination of gs and VPD. Seconds is the z axis (a matrix)
