#' Bootstrap Confidence Intervals

# Libraries ---------------------------------------------------------------
library(rsample)
library(tidyverse)

# Statistics of Interest --------------------------------------------------
get_mean <- function(split, ...) {
  bt_samp <- analysis(split)
  theta_i <- mean(bt_samp[["mpg"]])
  return(theta_i)
}

get_trimmed_mean <- function(split, trim, ...) {
  bt_samp <- analysis(split)
  theta_i <- mean(bt_samp[["mpg"]], trim = 0.3)
  return(theta_i)
}

get_median <- function(split, ...){
  bt_samp <- analysis(split)
  theta_i <- median(bt_samp[["mpg"]], ...)
  return(theta_i)
}

# get_diff_means <- function(split, ...){
  # bt_samp <- analysis(split)
  # theta_i <- btsamp[[- x_2
# }

get_diff_median <- function(splits, ...) {
  boot_sample <- analysis(splits)
  theta_i <- median(boot_sample$MonthlyIncome[boot_sample$Gender == "Female"]) -
    median(boot_sample$MonthlyIncome[boot_sample$Gender == "Male"])
  return(theta_i)
}

# first_resample <- bt_resamples2$splits[[1]]
# head(as.data.frame(first_resample))
# nrow(as.data.frame(first_resample))
# many_splits <- analysis(first_resample)
# theta_i <- median(many_splits$MonthlyIncome[many_splits$Gender == "Female"]) -
#   median(many_splits$MonthlyIncome[many_splits$Gender == "Male"])
# theta_i


# Helper Functions --------------------------------------------------------
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


# Example 1 ---------------------------------------------------------------
set.seed(1)
bt_resamples1 <- bootstraps(mtcars, times = 4000, apparent = TRUE) %>%
  mutate(theta_i = map_dbl(splits, get_mean))


# Example 2 ---------------------------------------------------------------
set.seed(1)
bt_resamples2 <- bootstraps(mtcars, times = 4000, apparent = TRUE) %>%
  mutate(theta_i = map_dbl(splits, get_median))


# Example 3 ---------------------------------------------------------------
# set.seed(353)
# bt_resamples3 <- bootstraps(attrition, times = 4000, apparent = TRUE) %>%
  # mutate(theta_i = map_dbl(splits, get_diff_median)) %>%
  # mutate(call_me_maybe = call("get_diff_median"))
  # mutate(theta_i = map(splits, median(attrition$MonthlyIncome[attrition$Gender == "Female"]) -
           # median(attrition$MonthlyIncome[attrition$Gender == "Male"])))
# bt_resamples$wage_diff <- map_dbl(bt_resamples2$splits, get_diff_median) %>%


# High-Level API ----------------------------------------------------------
boot_ci <- function(bt_resamples, statistic, variable, method = "percentile", level = 0.95, ...) {
  alpha = 1 - level
  variables = variable
  apparent_resample <- bt_resamples$splits[[length(bt_resamples$splits)]]
  apparent_df <- as.data.frame(apparent_resample)
  apparent_vals <- apparent_df[[variables]]

  # TO-DO
  # Sure, here I care abut the mean. But how do I generalize for cases
  # where the mean is NOT theta_obs (the statistic of interest)?
  #
  # use invoke() function at the expense of making boot_ci() call
  # annoying with yet ANOTHER added parameter to write in the call
  theta_obs <- invoke(statistic, apparent_vals)
  # theta_obs <- mean(apparent_vals)

  if (method == "percentile") {
    # don't do detailed computations in this block, just call
    results <- boot_ci_perc(bt_resamples, alpha, apparent_vals, ...)
    return(results)
  } # interesting side-effect..doesn't get called if not possible to calculate?
  # or is it just a bug?
  if (method == "pivot-t"){
    results <- boot_ci_t(bt_resamples, alpha, apparent_vals, theta_obs, ...)
    return(results)
  }
  if (method == "bca"){
    results <- boot_ci_bca(bt_resamples, alpha, apparent_vals, ...)
    return(results)
  }
  if (method == "abc"){
    results <- boot_ci_abc(bt_resamples, alpha, apparent_vals, ...)
    return(results)
  }
}


# Low-Level API -----------------------------------------------------------
boot_ci_perc <- function(bt_resamples, alpha, data) {
  ci_perc <-  quantile(bt_resamples$theta_i, probs = c(alpha/2, 1-alpha/2))
  return(ci_perc)
}


boot_ci_t <- function(bt_resamples, alpha, data, theta_obs) {
  theta_sd <- map_dbl(bt_resamples$splits, get_sd)
  bt_sample_size <- map_dbl(bt_resamples$splits, get_length)
  # theta_obs = bt_resamples$theta_obs[1]
  theta_b <- (bt_resamples$theta_i-theta_obs)/theta_sd/sqrt(bt_sample_size)
  bootstrap_t <-quantile(theta_b, probs = c(alpha/2, 1-alpha/2))
  ci_t <-  theta_obs + bootstrap_t * sd(data) / sqrt(length(data))
  return (ci_t)
}


boot_ci_bca <- function(bt_resamples, alpha, data){
  theta_hat = mean(bt_resamples$theta_i)

  ### Estimating Z0:
  po = mean(bt_resamples$theta_i <= theta_hat)
  Z0 = qnorm(po)
  Za = qnorm(1-alpha/2)

  # // TO-DO I'm sure you can do LOO sampling using rsample()...akin to LOO CV
  # loo_rsets <- loo_cv(as_tibble(data))
  # loo_df <- loo_rsets %>%
  #   mutate(theta_i =  mean())
  # Error: C stack usage  7970176 is too close to the limit

  leave_one_out_theta = sapply(1:length(data), function(i){
    leave_out_data = data[-i] # leave out the ith observation
    theta_i = mean(leave_out_data)
    return(theta_i)
  })

  theta_minus_one = mean(leave_one_out_theta)
  a = sum( (theta_minus_one - leave_one_out_theta)^3)/( 6 *(sum( (theta_minus_one - leave_one_out_theta)^2))^(3/2) )

  Zu = (Z0+Za)/(1-a*(Z0+Za)) + Z0 # upper limit for Z
  Zl = (Z0-Za)/(1-a*(Z0-Za)) + Z0 # Lower limit for Z
  lower_percentile = pnorm(Zl,lower.tail = TRUE) # percentile for Z
  upper_percentile = pnorm(Zu,lower.tail = TRUE) # percentile for Z
  ci_bca = as.numeric(quantile(bt_resamples$theta_i, c(lower_percentile,upper_percentile))) # putting those percentiles in place of alpha/2, 1-alpha/
  return(ci_bca)
}


boot_ci_abc <- function(bt_resamples, alpha, data){
  paste("Approximate Bootstrap CI Results \n a method of approximating the BCa Intervals
        \n analytically, without using any Monte Carlo replications at all. \n
        The S function is called: abcnon(x, tt)")
}


# Example 1 Results -------------------------------------------------------
# percentile_results <- boot_ci(bt_resamples1, statistic = "get_mean", variable = "mpg", method = "percentile", level = 0.95)
# pivot_t_results <- boot_ci(bt_resamples1, statistic = "get_mean", variable = "mpg", method = "pivot-t", level = 0.95)
# bca_results <- boot_ci(bt_resamples1, statistic = "get_mean", variable = "mpg", method = "bca", level = 0.95)
# all_results <- rbind(percentile_results, pivot_t_results, bca_results)
# cat("--- BOOT_CI() --- \n")
# print(all_results)


# Example 2 Results -------------------------------------------------------
percentile_results <- boot_ci(bt_resamples2, statistic = "get_median", variable = "mpg", method = "percentile", level = 0.95)
pivot_t_results <- boot_ci(bt_resamples2, statistic = "get_median", variable = "mpg", method = "pivot-t", level = 0.95)
bca_results <- boot_ci(bt_resamples2, statistic = "get_median", variable = "mpg", method = "bca", level = 0.95)
all_results <- rbind(percentile_results, pivot_t_results, bca_results)
cat("--- BOOT_CI() --- \n")
print(all_results)



# Example 3 Results -------------------------------------------------------
#######
# TO-DO this won't work unless you get rid of the calls to mtcars & mtcars$mpg
# within the confidence interval methods
####
# percentile_results <- boot_ci(bt_resamples3, method = "percentile", level = 0.05)
# pivot_t_results <- boot_ci(bt_resamples3, method = "pivot-t", level=0.05)
# bca_results <- boot_ci(bt_resamples3, method = "bca", level=0.05)
# all_results <- rbind(percentile_results, pivot_t_results, bca_results)
# print(all_results)



# Boot() Package ----------------------------------------------------------
library(boot)
theta_fun = function(mtcars, random_indices){
  boot_data = mtcars$mpg[random_indices]
  theta_i = mean(boot_data)
  return(theta_i)
}
boot_resamples = boot(mtcars, theta_fun, R = 4000)
boot_package_results <- boot.ci(boot_resamples)
cat("\n\n---BOOT PACKAGE---\n")
print(boot_package_results)

