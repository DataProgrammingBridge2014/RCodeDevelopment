Week 9 - Group Submission - R
==================================================================================

## Conclusions
This is where the general summary of the findings should go.

## Section Categories
Here we should break up our documentation into sections with our code and supporting visualizations.

## Workspace
Below is all of the code previously submitted through the discussion forums.

**Brett's initial code:**
``` {r echo=TRUE}

library(RSQLite)
setwd('C:/Users/Brett/Downloads')
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

```

**Brett's initial plots**

``` {r echo=TRUE}
par(mfrow=c(2,2))
plot(Idf, main="I")
abline(lm(Idf$y ~ Idf$x))
plot(IIdf, main="II")
abline(lm(IIdf$y ~ IIdf$x))
plot(IIIdf, main="III")
abline(lm(IIIdf$y ~ IIIdf$x))
plot(IVdf, main="IV")
abline(lm(IVdf$y ~ IVdf$x))

# We can see that all four have a very similar regression line, but obviously those regression lines don't tell us very much about II, III, or IV.

```

**Sonya's summaries:**

``` {r echo=TRUE}
summary(Idf)
summary(IIdf)
summary(IIIdf)
summary(IVdf)
```

**Tom's code using ggplot:**

``` {r echo=TRUE}
db <- dbConnect(SQLite(), dbname="cunyweek9.sqlite") ## connect to db
dbListTables(db)dbListFields(db, "I") ##just looking

cI <- as.data.frame(dbReadTable(db, "I")) ## import first table, etc.
cII <- as.data.frame(dbReadTable(db, "II"))
cIII <- as.data.frame(dbReadTable(db, "III"))
cIV <- as.data.frame(dbReadTable(db, "IV"))
week9df <- as.data.frame(cbind(cI, cII, cIII, cIV))
str(week9df) ## just taking a look
cuny9 <- week9df ## renaming to a simpler name
head(cuny9) ## just looking at the data
names(cuny9) <- (c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")) ## new colnames to indicate 1-4 soure
pairs(cuny9) ## exploratory plots using base graphics
library(ggplot2) ## load ggplot2
with(cuny9, qplot(x1,y1)) ## first of 4 individual plots 
with(cuny9, qplot(x2,y2))
with(cuny9, qplot(x3,y3))
with(cuny9, qplot(x4,y4))
```

**Igor's box plots:**
``` {r echo=TRUE}
library("ggplot2", lib.loc="C:/R/R-3.1.0/library")
source("http://peterhaschke.com/Code/multiplot.R")
file_name <- "C:\\CUNY\\cuny9.csv"
cuny9 <- read.table(file=file_name, header=TRUE, sep=",")
p1 <- ggplot(cuny9, aes(y=y1,x=x1))+geom_boxplot()
p2 <- ggplot(cuny9, aes(y=y2,x=x2))+geom_boxplot()
p3 <- ggplot(cuny9, aes(y=y3,x=x3))+geom_boxplot()
p4 <- ggplot(cuny9, aes(y=y4,x=x4))+geom_boxplot()
multiplot(p1, p2, p3, p4, cols=2)
```

**Brett's linear modeling**
``` {r echo=TRUE}
par(mfrow=c(2,2))
Ilm <- lm(formula = y ~ x, data = Idf)
plot(Ilm)

par(mfrow=c(2,2))
IIlm <- lm(formula = y ~ x, data = IIdf)
plot(IIlm)

par(mfrow=c(2,2))
IIIlm <- lm(formula = y ~ x, data = IIIdf)
plot(IIIlm)

par(mfrow=c(2,2))
IVlm <- lm(formula = y ~ x, data = IVdf)
plot(IVlm)

```

**Xingjia's linear regression modeling summaries**

``` {r echo=TRUE}
Ilmfit <- lm(Idf$y ~ Idf$x)
summary(Ilmfit)
IIlmfit <- lm(IIdf$y ~ IIdf$x)
summary(IIlmfit)
IIIlmfit <- lm(IIIdf$y ~ IIIdf$x)
summary(IIIlmfit)
IVlmfit <- lm(IVdf$y ~ IVdf$x
summary(IVlmfit)
```

**Riguel's influence measures**

```{r echo=TRUE}

# Influence Measures gives several measures of influence for each observation (Cook's Distance, et) and actually flags observations that it determines are influential by any of the measure

influence.measures(Ilm)

influence.measures(IIlm)

influence.measures(IIIlm)

influence.measures(IVlm)
```

**Lara's Durbin-Watson tests:**

```{r echo=TRUE}
library(lmtest)
dwtest(Ilm)
dwtest(IIlm)
dwtest(IIIlm)
dwtest(IVlm)
```
