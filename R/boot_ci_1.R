#' Bootstrap Confidence Intervals
#'
#' @description 
#' `boot_ci()` can be used to take the results of a resampling procedure 
#' created by `rsample` and construct a confidence interval for a statistic
#' of interest.
#'
#' @details 
#' Statistic(s) of interest must be defined by the user. Parameter estimates 
#' and their corresponding confidence intervals can be bootstrapped, such 
#' as trimmed mean, difference of medians between two groups, regression 
#' coefficients, etc. 
#'

### -- Library Calls (take out later) ----------------------------
library(rsample)
library(tidyverse)
library(roxygen2)
library(boot)



  #' @name boot_ci()
  #' @param x An `rset` object.
  #' @param method confidence intervals
  #'  Should I follow the `boot:boot.ci()` names? Like `boot:boot.ci(stud)` is
  #'   the name for pivot-t / studentized confidence intervals
  #' @param level = 1 - alpha to match the `base::confint()` syntax.
  #'   Contrary to base R, `alpha=0.5` seems more intuitive.
  #'
  # high-level api
  boot_ci() <- function(x, method = "percentile", level = 0.95, the.data...) {
    
  alpha <- 1 - level
  
  # loop over method & call the boot_ci
  # name the low-level function to be consisetent with the method arguemtn
  # percentile_ci()
  f = paste0(method, "_ci") 
  
  # construct list of arguments
  mean(x,trim = 0.5)
  
  # invoke function via do.call
  do.call(f)
  do.call("mean", list(of_arguments, na.rm=TRUE), trim=0.5)
    
  if (method == "percentile") {
    # don't do detailed computations in this block, just call
    results <- boot_ci_perc(x, alpha, ...)
    return(results)
  }
  if (method == "pivot-t") {
    results <- boot_ci_t(x, alpha, ...)
    return(results)
  }
  if (method == "bca") {
    results <- boot_ci_bca(x, alpha, ...)
    return(results)
  }
  if (method == "abc") {
    results <- boot_ci_abc(x, alpha, ...)
    return(results)
  }
  }
  
  
bt_resamples <- 
  
  

# low-level api's
boot_ci_perc <- function(x, alpha, data) {
  ci.perc <- quantile(x$theta.i, probs = c(alpha / 2, 1 - alpha / 2))
  return(ci.perc)
}

boot_ci_t <- function(x, alpha, data) {
  data <- mtcars
  theta_sd <- map_dbl(x$splits, get_sd)
  bt.sample.size <- map_dbl(x$splits, get_length)
  theta.obs <- x$theta.obs[1]
  theta.b <- (x$theta.i - theta.obs) / theta.sd / sqrt(bt.sample.size)
  bootstrap.t <- quantile(theta.b, probs = c(alpha / 2, 1 - alpha / 2))
  # // TO-DO don't reference mtcars$mpg & mtcars locally here
  ci.t <- theta.obs + bootstrap.t * sd(mtcars$mpg) / sqrt(nrow(mtcars))
  return(ci.t)
}


#' @source Efron, Tibshirani 1994. Page 186.
boot_ci_bca <- function(x, alpha, the.data, var.name) {
  theta.hat <- mean(x$theta.i)

  ### Estimating Z0:
  po <- mean(x$theta.i <= theta.hat)
  Z0 <- qnorm(po)
  Za <- qnorm(1 - alpha / 2)

  # // TO-DO don't reference mtcars locally
  ### Estimating a:
  the.data <- mtcars
  the.data[-var.name]

  # // TO-DO I'm sure you can do LOO sampling using rsample()...akin to LOO CV
  # similar to call to boostraps
  loo_cv(the.data)
  
  leave.one.out.theta <- sapply(1:nrow(the.data), function(i) {
    # // TO-DO don't reference $mpg locally
    leave.out.data <- the.data$mpg[-i] # leave out the ith observation
    theta.i <- mean(leave.out.data)
    return(theta.i)
  })

  theta.minus.one <- mean(leave.one.out.theta)
  a <- sum((theta.minus.one - leave.one.out.theta)^3) / (6 * (sum((theta.minus.one - leave.one.out.theta)^2))^(3 / 2))

  Zu <- (Z0 + Za) / (1 - a * (Z0 + Za)) + Z0 # upper limit for Z
  Zl <- (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0 # Lower limit for Z
  lower.percentile <- pnorm(Zl, lower.tail = TRUE) # percentile for Z
  upper.percentile <- pnorm(Zu, lower.tail = TRUE) # percentile for Z
  ci.bca <- as.numeric(quantile(x$theta.i, c(lower.percentile, upper.percentile))) # putting those percentiles in place of alpha/2, 1-alpha/
  return(ci.bca)
}



boot_ci_abc <- function(x, alpha) {
  paste("Approxima






te Bootstrap CI Results \n a method of apporximating the BCa Intervals
        \n analytically, without using any Monte Carlo replications at all. \n
        The S function is called: abcnon(x, tt)")
}








### -- Example 1 Results -------------------------------------------
percentile_results <- boot_ci(bt_resamples, method = "percentile", level = 0.95)
pivot_t_results <- boot_ci(bt_resamples, method = "pivot-t", level = 0.95)
bca_results <- boot_ci(bt_resamples, method = "bca", level = 0.95)
all_results <- rbind(percentile_results, pivot_t_results, bca_results)
print(all_results)

### -- Example 2 Results ------------------------------------------
# TO-DO ===========================================================================
# This won't work unless you get rid of the calls to mtcars & mtcars$mpg
# within the confidence interval methods
# =================================================================================
# percentile_results <- boot_ci(bt_resamples2, method = "percentile", level = 0.05)
# pivot_t_results <- boot_ci(bt_resamples2, method = "pivot-t", level=0.05)
# bca_results <- boot_ci(bt_resamples2, method = "bca", level=0.05)
# all_results <- rbind(percentile_results, pivot_t_results, bca_results)
# print(all_results)



### -- boot::boot() Equivalents ------------------------------- 
set.seed(1)
theta.fun <- function(mtcars, random.indices) {
  boot.data <- mtcars$mpg[random.indices]
  theta.i <- mean(boot.data)
  return(theta.i)
}

boot_resamples <- boot(mtcars, theta.fun, c("stud", "percentile", R = 4000)
boot_package_results <- boot.ci(boot_resamples)
print(boot_package_results)
