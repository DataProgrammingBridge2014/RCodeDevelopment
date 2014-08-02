#Part 1
library(RSQLite)
setwd('C:/Users/Brett/Downloads')
# Importing the data from SQLite
sqlite <- dbDriver("SQLite")
workingdb <- dbConnect(sqlite, "cunyweek9.sqlite")
dbListTables(workingdb)
I <- dbSendQuery(workingdb, "SELECT * FROM I")
Idf <- data.frame(fetch(I, -1))
II <- dbSendQuery(workingdb, "SELECT * FROM II")
IIdf <- data.frame(fetch(II, -1))
III <- dbSendQuery(workingdb, "SELECT * FROM III")
IIIdf <- data.frame(fetch(III, -1))
IV <- dbSendQuery(workingdb, "SELECT * FROM IV")
IVdf <- data.frame(fetch(IV, -1))
dbDisconnect(workingdb)
remove(sqlite)
remove(workingdb)
remove(I, II, III, IV)
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

#Combining the data in order to make using others code easier
cuny9 <- cbind(Idf, IIdf, IIIdf, IVdf)
colnames(cuny9) <- c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")
summary(cuny9)
cor(cuny9)
library(ggplot2)
ggplot(cuny9, aes(x=x1, y=y1)) + geom_point(shape=1) + geom_smooth(method=lm)
ggplot(cuny9, aes(x=x2, y=y2)) + geom_point(shape=1) + geom_smooth(method=lm)
ggplot(cuny9, aes(x=x3, y=y3)) + geom_point(shape=1) + geom_smooth(method=lm)
ggplot(cuny9, aes(x=x4, y=y4)) + geom_point(shape=1) + geom_smooth(method=lm)
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
library(gridExtra)
multiplot(fit1, fit2, fit3, fit4)
#Fitted and residuals
plotfit1 <- ggplot(aes(x=.fitted, y=.resid), data=fit1)+geom_point()+geom_hline(yintercept=0)+geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
plotfit2 <- ggplot(aes(x=.fitted, y=.resid), data=fit2)+geom_point()+geom_hline(yintercept=0)+geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
plotfit3 <- ggplot(aes(x=.fitted, y=.resid), data=fit3)+geom_point()+geom_hline(yintercept=0)+geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
plotfit4 <- ggplot(aes(x=.fitted, y=.resid), data=fit4)+geom_point()+geom_hline(yintercept=0)+geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
plotfit1
plotfit2
plotfit3
plotfit4
#QQ
qqdat1 <- ggplot(fit1, aes(sample=.stdresid))+stat_qq()+geom_abline()
qqdat2 <- ggplot(fit2, aes(sample=.stdresid))+stat_qq()+geom_abline()
qqdat3 <- ggplot(fit3, aes(sample=.stdresid))+stat_qq()+geom_abline()
qqdat4 <- ggplot(fit4, aes(sample=.stdresid))+stat_qq()+geom_abline()
qqdat1
qqdat2
qqdat3
qqdat4
#Influence Measures
influence.measures(Ilm)
influence.measures(IIlm)
influence.measures(IIIlm)
influence.measures(IVlm)
