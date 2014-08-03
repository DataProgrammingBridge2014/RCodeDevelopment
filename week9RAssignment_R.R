
#Part 1 import data from csv file on github

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
fileurl= "https://raw.githubusercontent.com/DataProgrammingBridge2014/RCodeDevelopment/master/cuny9.csv"
data <- getURL(fileurl)
cuny9 <- read.csv(text = data)

# split data into data frames for Bretts code
Idf <- data.frame(x = cuny9$x1, y = cuny9$y1)
IIdf <- data.frame(x = cuny9$x2, y = cuny9$y2)
IIIdf <- data.frame(x = cuny9$x3, y = cuny9$y3)
IVdf <- data.frame(x = cuny9$x4, y = cuny9$y4)

#Initial plots:
par(mfrow=c(2,2))
plot(Idf, main="I")
abline(lm(Idf$y ~ Idf$x))
plot(IIdf, main="II")
abline(lm(IIdf$y ~ IIdf$x))
plot(IIIdf, main="III")
abline(lm(IIIdf$y ~ IIIdf$x))
plot(IVdf, main="IV")
abline(lm(IVdf$y ~ IVdf$x))

#Summaries:
summary(Idf)
summary(IIdf)
summary(IIIdf)
summary(IVdf)

#Now let's make a more accurate predition model: (each lm makes 4 pictures)
par(mfrow=c(2,2))
Ilm <- lm(formula = y ~ x, data = Idf)
plot(Ilm)
#II
IIlm <- lm(formula = y ~ x, data = IIdf)
plot(IIlm)
#III
IIIlm <- lm(formula = y ~ x, data = IIIdf)
plot(IIIlm)
#IV
IVlm <- lm(formula = y ~ x, data = IVdf)
plot(IVlm)
#correlation of all data
cor(cuny9)
#plot using ggplot2

library(ggplot2)
library(grid)
library(gridExtra)

a<-ggplot(cuny9, aes(x=x1, y=y1)) + geom_point(shape=1) + geom_smooth(method=lm)
b<-ggplot(cuny9, aes(x=x2, y=y2)) + geom_point(shape=1) + geom_smooth(method=lm)
c<-ggplot(cuny9, aes(x=x3, y=y3)) + geom_point(shape=1) + geom_smooth(method=lm)
d<-ggplot(cuny9, aes(x=x4, y=y4)) + geom_point(shape=1) + geom_smooth(method=lm)

grid.arrange(a,b,c,d, ncol=2, main = "data sets I - IV with ggplot2")

fit1 <- lm(y1 ~ x1, cuny9)
summary(fit1)
fit2 <- lm(y2 ~ x2, cuny9)
summary(fit2)
fit3 <- lm(y3 ~ x3, cuny9)
summary(fit3)
fit4 <- lm(y4 ~ x4, cuny9)
summary(fit4)

#Coefficient plots and residuals
library(coefplot)

multiplot(fit1, fit2, fit3, fit4)

#Fitted and residuals
plotfit1 <- ggplot(aes(x=.fitted, y=.resid), data=fit1)+geom_point()+geom_hline(yintercept=0)+geom_smooth(method = lm, se=F)+labs(x="Fitted Values", y="Residuals")
plotfit2 <- ggplot(aes(x=.fitted, y=.resid), data=fit2)+geom_point()+geom_hline(yintercept=0)+geom_smooth(method = lm, se=F)+labs(x="Fitted Values", y="Residuals")
plotfit3 <- ggplot(aes(x=.fitted, y=.resid), data=fit3)+geom_point()+geom_hline(yintercept=0)+geom_smooth(method = lm, se=F)+labs(x="Fitted Values", y="Residuals")
plotfit4 <- ggplot(aes(x=.fitted, y=.resid), data=fit4)+geom_point()+geom_hline(yintercept=0)+geom_smooth(method = lm, se=F)+labs(x="Fitted Values", y="Residuals")

suppressWarnings(
  suppressMessages(
  grid.arrange(plotfit1, plotfit2,plotfit3,plotfit4, 
                              ncol=2, main = "Fitted Values vs. Residuals")))

#QQ
qqdat1 <- ggplot(fit1, aes(sample=.stdresid))+stat_qq()+geom_abline()+ggtitle("I")
qqdat2 <- ggplot(fit2, aes(sample=.stdresid))+stat_qq()+geom_abline()+ggtitle("II")
qqdat3 <- ggplot(fit3, aes(sample=.stdresid))+stat_qq()+geom_abline()+ggtitle("III")
qqdat4 <- ggplot(fit4, aes(sample=.stdresid))+stat_qq()+geom_abline()+ggtitle("IV")

suppressWarnings(
  suppressMessages(
    grid.arrange(qqdat1, qqdat2, qqdat3, qqdat4, 
                 ncol=2, main = "Theoretical Quantile Plots")))
  


#Influence Measures
influence.measures(Ilm)
influence.measures(IIlm)
influence.measures(IIIlm)
influence.measures(IVlm)


#Alternate Methods - whole model

obs <- length(cuny9$x1)  # get number of obs. for each data set

#create vector that indicates the data source for the x and y values 
dataset <- as.vector(c(rep("I",obs),rep("II",obs), rep("III",obs), rep("IV",obs))) 
cunyy <- as.vector(c(cuny9$y1, cuny9$y2, cuny9$y3, cuny9$y4))  #combine all y's
cunyx <- as.vector(c(cuny9$x1, cuny9$x2, cuny9$x3, cuny9$x4))  #combine all x's
#Create data frame
cunyxy <- data.frame(cunyx, cunyy, dataset)

#plot all 4 sets together
e <- ggplot(data = cunyxy, aes(x=cunyx, y= cunyy))
e + geom_point(aes(color = dataset, size = 6))+geom_smooth(method = lm)+ labs(x="X values", y = "Y values")


#model summary


lm4 <- lm(cunyx ~ cunyy,data = cunyxy)
summary(lm4)

# multivariate with y1, y2, y3 as predictors of x

# combine data sets, sum of y as predictors of x value
cunyy <- as.vector(cuny9$y1 + cuny9$y2 + cuny9$y3) #adding y's for I, II, and III
cunyx <- as.vector(cuny9$x1)  #x1 = x2 = x3 = x
cunyxy <- data.frame(cunyx, cunyy)

#graph model
e <- ggplot(data = cunyxy, aes(y=cunyx, x= cunyy))
e + geom_point(size = 4, color ="green3")+geom_smooth(method = lm)+labs(x="X values", y = "Y values")
#summary of model
lm3 <- lm(cunyx ~ cunyy, data = cunyxy)
summary(lm3)

