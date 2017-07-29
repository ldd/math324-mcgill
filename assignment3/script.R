#read the contents of the file
ct <- read.csv("contents.csv", header=TRUE, row.names=1)
attach(ct) 
data <- Contents 

#import library for fitdistr
library(MASS)

#part a: fit various distributions
expFit <- fitdistr(data, "exponential") 
gammaFit <- fitdistr(data, "gamma")
logNormFit <- fitdistr(data, "log-normal")#note: some NaNs found from not setting lower=someValue
normalFit <- fitdistr(data, "normal")

#part b: display the Q-Q plot

 #first get the data
expData <- qexp(ppoints(length(data)), rate = expFit$estimate)
gammaData <- qgamma(ppoints(length(data)), shape = gammaFit$estimate, rate = gammaFit$estimate)
logNormData <- qlnorm(ppoints(length(data)), meanlog = logNormFit$estimate, sdlog = logNormFit$estimate)
normalData <- qnorm(ppoints(length(data)), mean = normalFit$estimate, sd = normalFit$estimate)

 #now plot
old.par <- par(mfrow=c(2,2))#place 4 plots in the same page
qqplot(data, expData)
qqplot(data, gammaData)
qqplot(data, logNormData)
qqplot(data, normalData)
par(old.par)
#part c: use AIC to choose best fitting model
resultsAIC <- AIC(expFit, gammaFit, logNormFit, normalFit)
