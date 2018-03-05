library(tidyverse)
install.packages("knitr")

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









