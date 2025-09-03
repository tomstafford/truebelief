'''
Code to run IV analysis, following https://tomstafford.github.io/truebelief/
'''

#install.packages("ivmodel")
library(ivmodel) # For running IV analysis (v. 1.9.1)

library(tidyverse)
dataloc <- "obsdat.csv"
df <- read_csv(dataloc)

# # standard OLS

m0 <- lm(data = df, C ~ A + V) # standard OLS

summary(m0)

'''
            Estimate Std. Error t value Pr(>|t|)  
A           -0.001267   0.001254   -1.01    0.313    
V            0.389842   0.012814   30.42   <2e-16 ***

V predicts C, A does not add to that prediction, but we do not distinguish causal direction
V may cause C or C may cause V

'''


# Instrumental Variable analysis 2SLS

m1 <- lm(data = df, V ~ A) # 2SLS, 1st Stage
summary(m1)

'''
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  2.827480   0.190916  14.810   <2e-16 ***
A           -0.009139   0.004411  -2.072   0.0388 *   

Showing A is a weak instrument
(but still an instrument, because it affects V and cannot a prior be affected by V)

'''

#create predicted value of V from this

intercept = m1$coefficients[1]
slope = m1$coefficients[2]

df$Vp = df$A*slope + intercept
  
# plot for interest/sanity check
p<-ggplot(data =df,aes(x = V,y = Vp))
p+geom_point() #unimpressive frankly


m2 = lm(data=df, C ~ Vp)# 2LSL, 2nd Stage

summary(m2)

'''
            Estimate Std. Error t value Pr(>|t|)  
Vp            0.5285     0.2325   2.273   0.0234 *

This provides the estimate of the (local) causal effect

'''

slope = m2$coefficients[2] #IV estimate 0.5284526

'''
you can shortcut this by using the ivmodel package
'''


# IV Analysis

iv_analysis <- ivmodel(Y = df$C, 
                       D = df$V, 
                       Z = df$A,
                       heteroSE = TRUE) 

# Print the full summary to see everything
summary(iv_analysis)

effect = iv_analysis$kClass$point.est[2] #0.5284526