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


**Cheryl's Imaginary Models:** 
*all of cheryl's rather verbose commentary is included below. Please edit/trim/move/delete what isn't useful*

If we assume this is real data from some mysterious processes, we would notice that there seems to be a relationship between the different data sets, most obvious in I - III, where the X values are identical.

Two alternate approaches of looking at the data: 
1.  Combine all x,y data points into a single data set 
2.  Create a multivariate model with x as the dependent variable

```{r}
# read data form csv to dataframe
# wil need to modify data source to be consistent with the rest of the document, this points to the CSV file on my compute
library(plotting2)
cuny9 = read.csv("C:/Users/Cheryl/Documents/R/cuny9.csv", stringsAsFactors = FALSE)

```
##### 1.  Combine data points into single data set

Notes about combining all 4 data sets: 
Looking at all of the data as a single x,y linear model gives a fair P value, better than the individual models, even with data set IV included. If we use this as a model the outliers aren't so impactful. It is nice to be able to leave all the data in the model because given the small number of observations within each data set, it is really difficult to know if the outliers are valid and should be included, sometimes the skewing is entirely appropriate.

```{r}
obs <- length(cuny9$x1)  # get number of obs. for each data set
#create vector  that indicates the data source for the x and y values 
dataset <- as.vector(c(rep("I",obs),rep("II",obs), rep("III",obs), rep("IV",obs))) 
cunyy <- as.vector(c(cuny9$y1, cuny9$y2, cuny9$y3, cuny9$y4))  #combine all y's
cunyx <- as.vector(c(cuny9$x1, cuny9$x2, cuny9$x3, cuny9$x4))  #combine all x's
#Create data frame
cunyxy <- data.frame(cunyx, cunyy, dataset)
```

plot all 4 sets together
```{r fig.width=7, fig.height=6}
e <- ggplot(data = cunyxy, aes(x=cunyx, y= cunyy))
e + geom_point(aes(color = dataset), size = 6)+geom_smooth(method = lm)+ labs(x="X values", y = "Y values")
```

model summary
```{r}
lm4 <- lm(cunyx ~ cunyy,data = cunyxy)
summary(lm4)

```
##### 2.  multivariate with y1, y2, y3 as predictors of x

Notes about a multivariate model:
What if this is a model of 11 observations, and the y1,y2,y3,y4 are supposed to be attributes of x?. Since x1=x2 = x3,  perhaps the data collection for y1, y2, y3 functioned correctly, but something went terribly wrong with data collection in data set IV. In this case it might be a good idea to try and model the x  with just the first 3 sets of 'good' data. That gives some entertaining results, nice R, and pvalue

```{r}
# combine data sets, sum of y as predictors of x value
cunyy <- as.vector(cuny9$y1 + cuny9$y2 + cuny9$y3) #adding y's for I, II, and III
cunyx <- as.vector(cuny9$x1)  #x1 = x2 = x3 = x
cunyxy <- data.frame(cunyx, cunyy)

```
graph model
```{r fig.width=7, fig.height=6}
e <- ggplot(data = cunyxy, aes(y=cunyx, x= cunyy))
e + geom_point(size = 4, color ="green3")+geom_smooth(method = lm)+labs(x="X values", y = "Y values")
```
summary of model
```{r}
lm3 <- lm(cunyx ~ cunyy, data = cunyxy)
summary(lm3)
