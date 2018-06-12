library(tidyverse)
library(rsample)

get_tmean <- function(x){
  map_dbl(x,
      function(x)
        mean(analysis(x)[["Sepal.Width"]]), trim = 0.3)
}

get_tmean(bt$splits[[1]])

set.seed(64)
bt <- bootstraps(iris, apparent=TRUE) %>%
  mutate(tmean = get_tmean(splits))
