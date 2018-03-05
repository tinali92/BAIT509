library(tidyverse)
install.packages("knitr")
library(knitr)

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

###################
#Exercise 3.1
###################

#1 
#a reasonable estimate for y near x=0 is a little bit above 2.7, due to the location of nearest several points

#2
dat$d <- abs(dat$x-0)
dat$d
dat.sub <- arrange(dat,d)
dat.subset <- dat.sub[1:8,] 

(y_predict <- mean(dat.subset$y))

dat.lowess <- filter(dat,d<1)
mean(dat.lowess$y)

#3 when r = 0.01, no prediction exists
dat.lowess.0.01 <- filter(dat,d<0.01)

#4 
#small values: small bias with high variance
#large values: small variance but high bias

###################
#Exercise 3.2
###################

x.range <- seq(-5, 4, length.out=1000)

kNN_estimates <- map_dbl(x.range, function(x){
  dat$d <- abs(dat$x-x)
  dat.knn <- arrange(dat,d)
  dat.knnsub <- dat.knn[1:8,] 
  y.hat1 <- mean(dat.knnsub$y)
  return(y.hat1)
})

loess_estimates <- map_dbl(x.range, function(x){
  dat$d <- abs(dat$x-x)
  dat$d
  dat.lowess <- arrange(dat,d)
  dat.lowesssub <- filter(dat.lowess, d<0.1) #d<1,10,100,...
  y.hat2 <- mean(dat.lowesssub$y)
})
est <- tibble(x=x.range, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()

#lowess is over-fitting here: the model captures pieces that are not present in the data






