library(rsample)
library(tidyverse)

get_mean <- function(split, ...) {
  bt_samp <- analysis(split)
  theta_i <- mean(bt_samp[["mpg"]], ...)
  # theta.i = mean(boot.sample)
  # T.b = (theta.i-theta.obs)/(sd(boot.sample)/sqrt(length(boot.sample)))
  # return(T.b)
  return(theta_i)
}

get_median <- function(split, ...){
  bt_samp <- analysis(split)
  theta_i <- median(bt_samp[["mpg"]], ...)
}

get_diff_means <- function(split, ...){
  bt_samp <- analysis(split)
  theta_i <- x_1 - x_2
}

get_diff_median <- function(splits, ...) {
  bt_samp <- analysis(splits)
  theta_i <- median(bt_samp$MonthlyIncome[bt_samp$Gender == "Female"]) -
    median(bt_samp$MonthlyIncome[x$Gender == "Male"])
  return(theta_i)
}

get_sd <- function(split, ...){
  bt_samp <- analysis(split)
  theta_sd <- sd(bt_samp[["mpg"]])
  return(theta_sd)
}


get_length <- function(split, ...){
  bt_samp <- analysis(split)
  bt_sample_size <- length(bt_samp[["mpg"]])
  return(bt_sample_size)
}


# example 1
set.seed(1)
bt_resamples <- bootstraps(mtcars, times = 4000) %>%
  mutate(theta.i = map_dbl(splits, get_mean, trim = .3)) %>%
  # // TO-DO don't make the user calculate the bootstrap estimate by hand here
  mutate(theta.obs = mean(mtcars$mpg))

# example 2
# set.seed(353)
# bt_resamples2 <- bootstraps(attrition, times = 500) %>%
#   mutate(theta.i = map_dbl(splits, get_diff_median) %>%
#   mutate(theta.obs = median(attrition$MonthlyIncome[x$Gender == "Female"]) -
#   median(x$MonthlyIncome[attrition$Gender == "Male"])))
# bt_resamples$wage_diff <- map_dbl(bt_resamples2$splits, get_diff_median) %>%


#' @param x An `rset` object.
#' @param level = 1 - alpha to match the `base::confint()` syntax.
#' Contrary to base R, `alpha=0.5` seems more intuitive.
#'
# high-level api
boot_ci <- function(x, method = "percentile", level = 0.95, ...) {
  alpha = 1-level
  if(method == "percentile") {
    # don't do detailed computations in this block, just call
    results <- boot_ci_perc(x, alpha, ...)
    return(results)
  } # and so on
  if(method == "pivot-t"){
    results <- boot_ci_t(x, alpha, ...)
    return(results)
  }
  if(method == "bca"){
    results <- boot_ci_bca(x, alpha, ...)
    return(results)
  }
  if(method == "abc"){
    results <- boot_ci_abc(x, alpha, ...)
    return(results)
  }


}

# low-level api's
boot_ci_perc <- function(x, alpha) {
  ci.perc = quantile(x$theta.i, probs = c(alpha/2, 1-alpha/2))
  return(ci.perc)
}

boot_ci_t <- function(x, alpha) {
  theta.sd = map_dbl(x$splits, get_sd)
  bt.sample.size = map_dbl(x$splits, get_length)
  theta.obs = x$theta.obs[1]
  theta.b = (x$theta.i-theta.obs)/theta.sd/sqrt(bt.sample.size)
  bootstrap.t = quantile(theta.b, probs = c(alpha/2, 1-alpha/2))
  # // TO-DO don't reference mtcars$mpg & mtcars locally here
  ci.t = theta.obs + bootstrap.t * sd(mtcars$mpg) / sqrt(nrow(mtcars))
  return (ci.t)
}


boot_ci_bca <- function(x, alpha){
  theta.hat = mean(x$theta.i)

  ### Estimating Z0:
  po = mean(x$theta.i <= theta.hat)
  Z0 = qnorm(po)
  Za = qnorm(1-alpha/2)

  # // TO-DO don't reference mtcars locally
  ### Estimating a:
  the.data=mtcars

  # // TO-DO I'm sure you can do LOO sampling using rsample()...akin to LOO CV
  leave.one.out.theta = sapply(1:nrow(the.data),function(i){
    # // TO-DO don't reference $mpg locally
    leave.out.data = the.data$mpg[-i] #leave out the ith observation
    theta.i = mean(leave.out.data)
    return(theta.i)
  })

  theta.minus.one = mean(leave.one.out.theta)
  a = sum( (theta.minus.one - leave.one.out.theta)^3)/( 6 *(sum( (theta.minus.one - leave.one.out.theta)^2))^(3/2) )

  Zu = (Z0+Za)/(1-a*(Z0+Za)) + Z0 #upper limit for Z
  Zl = (Z0-Za)/(1-a*(Z0-Za)) + Z0 #Lower limit for Z
  lower.percentile = pnorm(Zl,lower.tail = TRUE) #percentile for Z
  upper.percentile = pnorm(Zu,lower.tail = TRUE) #percentile for Z
  ci.bca = as.numeric(quantile(x$theta.i,c(lower.percentile,upper.percentile))) #putting those percentiles in place of alpha/2, 1-alpha/
  return(ci.bca)
}



boot_ci_abc <- function(x, alpha){
  paste("Approximate Bootstrap CI Results \n a method of apporximating the BCa Intervals
        \n analytically, without using any Monte Carlo replications at all. \n
        The S function is called: abcnon(x, tt)")
}


# finally call the boot functions
percentile_results <- boot_ci(bt_resamples, method = "percentile", level = 0.95)
pivot_t_results <- boot_ci(bt_resamples, method = "pivot-t", level = 0.95)
bca_results <- boot_ci(bt_resamples, method = "bca", level = 0.95)
all_results <- rbind(percentile_results, pivot_t_results, bca_results)
print(all_results)

# finally call the boot functions
#######
# TO-DO this won't work unless you get rid of the calls to mtcars & mtcars$mpg
# within the confidence interval methods
####
# percentile_results <- boot_ci(bt_resamples2, method = "percentile", level = 0.05)
# pivot_t_results <- boot_ci(bt_resamples2, method = "pivot-t", level=0.05)
# bca_results <- boot_ci(bt_resamples2, method = "bca", level=0.05)
# all_results <- rbind(percentile_results, pivot_t_results, bca_results)
# print(all_results)






# check compared to boot package
library(boot)
theta.fun = function(mtcars, random.indices){
  boot.data = mtcars$mpg[random.indices]
  theta.i = mean(boot.data)
  return(theta.i)
}

boot_resamples = boot(mtcars, theta.fun, R = 4000)
boot_package_results <- boot.ci(boot_resamples)
print(boot_package_results)

