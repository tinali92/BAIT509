library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps #eps = error
  tibble(x1=x1, x2=x2, y=y)
  # tibble: a fancy version of a data frame with columns named x1, x2, and y
}
genreg(10)
#1
dat <- genreg(1000)
#2
dat <- mutate(dat,
       yhat = 0,
       yhat1 = 5-x1,
       yhat2 = 5+2*x2,
       yhat12 = 5-x1+2*x2)
#3
mse <- mean((dat$yhat - dat$y)^2)
mse1 <- mean((dat$yhat1 - dat$y)^2)
mse2 <- mean((dat$yhat2 - dat$y)^2)
mse12 <- mean((dat$yhat12 - dat$y)^2)
msecombine <- cbind(mse,mse1,mse2,mse12)
msecombine #x2 is a better predictor than x1 for y

#classification
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}
#1
  #x = 1
(pB1 <- 0.8/(1+exp(-1)))#0.5848469
(pC1 <- 1 - pB1 - 0.2)#0.2151531
  #predict y in category level B when x = 1
  #x = -2
(pB2 <- 0.8/(1+exp(2)))#0.09536234
(pC2 <- 1 - pB2 - 0.2)#0.7046377
  #predict y in category level C when x = 2

#plot x = 1
df <- data.frame(Y=c("A", "B", "C"),
                 Probability=c(0.2, 0.5848469, 0.2151531))
library(ggplot2)
# Basic barplot
ggplot(data=df, aes(x=Y, y=Probability)) +
  geom_bar(stat="identity") + labs(title="probability of x = 1")

#plot x = -2
df2 <- data.frame(Y=c("A", "B", "C"),
                 Probability=c(0.2, 0.09536234, 0.7046377))
library(ggplot2)
# Basic barplot
ggplot(data=df2, aes(x=Y, y=Probability)) +
  geom_bar(stat="identity") + labs(title="probability of x = -2")

#2
#if x < 0, predict y in C
#if x > 0, predict y in B
#if x = 0, predict either B or C

#3
dat2 <- gencla(1000)

#4
dat2 <- mutate(dat2,
               yhat = sapply(x, function(x_)
                 if (x_<0) "C" else "B"))
#5
1-mean(dat2$yhat == dat2$y)
