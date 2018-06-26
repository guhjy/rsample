#' Bootstrap Confidence Intervals
#'
#' @details
#' Calculate boostrap confidence intervals for a statistic of interest
#'
#'  @importFrom dplyr mutate
#'  @importFrom stats sd
#'  @export
boot_ci_t <- function(bt_resamples, stat, alpha, data = NULL, theta_obs) {

  theta_obs <- theta_obs[[stat]]

  if (all(is.na(theta_obs)))
    stop("All statistics (theta_obs) are missing values.", call. = FALSE)

  theta_se <- sd(bt_resamples[[stat]], na.rm = TRUE) / sqrt(sum(!is.na((bt_resamples[[stat]]))))

  if (theta_se == 0 | theta_se == Inf)
    stop("Your standard error (theta_se) is 0 or infinity.", call. = FALSE)

  z_dist <- (bt_resamples[[stat]] - theta_obs) / theta_se
  z_pntl <- quantile(z_dist, probs = c(alpha / 2, 1 - (alpha) / 2), na.rm = TRUE)
  ci <- theta_obs + z_pntl * theta_se

  tibble(
    lower = ci[1],
    upper = ci[2],
    alpha = alpha,
    method = "bootstrap-t"
  )
}

boot_ci_perc <- function(bt_resamples, stat, alpha, data = NULL, theta_obs) {
  z_dist <- bt_resamples[[stat]]

  if (all(is.na(z_dist)))
  stop("All statistics (z_dist) are missing values.", call. = FALSE)

  if (0 < alpha && alpha > 1)
  stop("Your significance level (alpha) is unreasonable.", call. = FALSE)

  ci <- quantile(z_dist, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  tibble(
    lower = ci[1],
    upper = ci[2],
    alpha = alpha,
    method = "percentile"
  )
}


boot_ci_bca <- function(bt_resamples, stat, stat_func, alpha, var, data = NULL, theta_obs){

  if (nrow(bt_resamples) < 1000)
    stop("Recommend at least 1000 bootstrap resamples.", call. = FALSE)

  # throw an error if apaprent != TRUE
  # TODO then write a test case for that
  # if(apparent != TRUE)
  #   stop("Please set apparent = TRUE in boostraps()")

  # Process apparent resample
  apparent_sample <- theta_obs$splits[[1]]
  dat <- analysis(apparent_sample)

  # # What is the monthly income for females?
  # dat$MonthlyIncome[dat$Gender == "Female"]
  #
  # # What is the monthly income for females?
  # dat$MonthlyIncome[dat$Gender == "Male"]


  # wow this is assuming that the 2 vectors are the same length
  # That is, you're assuming that each data point is 1:1 mapping of male vs female ???
  # That actually makes no sense
  # median_diffs <- median(dat$MonthlyIncome[dat$Gender == "Female"] - dat$MonthlyIncome[dat$Gender == "Male"])


  median_difference <- median(dat$MonthlyIncome[dat$Gender == "Female"]) - median(dat$MonthlyIncome[dat$Gender == "Male"])

  mean(median_difference)


  # median(x$MonthlyIncome[x$Gender == "Female"]) -
  #   median(x$MonthlyIncome[x$Gender == "Male"])
  #
  data <- as.data.frame(dat[[var]])

  # hist(bt_resamples[[stat]])
  # hist(attrition$MonthlyIncome)
  #
  # is it mean(median(x1) - median(x2)) ???
  theta_hat <- mean(bt_resamples[[stat]])

  ### Estimating Z0 bias-correction
  po <- mean(bt_resamples[[stat]] <= theta_hat)
  Z0 <- qnorm(po)
  Za <- qnorm(1 - alpha / 2)

  # TODO clock loo_cv() performance against sapply implementation
  #             using loo_cv() appears significantly slower (at least for k = 1)
  # TODO Time for k > 1, e.g. bootstrap for regression coefficients & compare

  # leave_one_out_theta = sapply(1:length(data), function(i){
  #   leave_out_data = data[-i] # leave out the ith observation
  #   theta_i = mean(leave_out_data)
  # return(theta_i)    # returns a vector of means. mean of each bootstrap resample.
  # })

  # dat

  get_theta_i <- function(x)
     map_dbl(x,
             function(x)
               median(analysis(x)[["dat[[var]]"]]))


  # get_theta_i <- do.call(stat_func, x)

  # # TODO replace mean with median
  # or rather stat of interest
  # use some `do.call` magique to call `median_diff` or `get_tmean` functions


  loo_df <- loo_cv(data)

  leave_one_out_theta <- loo_cv(data) %>%
    mutate(theta_i = get_theta_i(splits))

  theta_minus_one <- mean(leave_one_out_theta$theta_i)
  a <- sum( (theta_minus_one - leave_one_out_theta$theta_i) ^ 3) / ( 6 * (sum( (theta_minus_one - leave_one_out_theta$theta_i) ^ 2)) ^ (3 / 2) )

  Zu <- (Z0 + Za) / ( 1 - a * (Z0 + Za)) + Z0 # upper limit for Z
  Zl <- (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0 # Lower limit for Z
  lower_percentile <-  pnorm(Zl, lower.tail = TRUE) # percentile for Z
  upper_percentile <-  pnorm(Zu, lower.tail = TRUE) # percentile for Z
  ci_bca <- as.numeric(quantile(bt_resamples[[stat]], c(lower_percentile, upper_percentile))) # putting those percentiles in place of alpha/2, 1-alpha/

  tibble(
  lower = ci_bca[1],
  upper = ci_bca[2],
  alpha = alpha,
  method = "BCa"
  )
}


# can't write issues on a fork.
# TODO how to handle multiple `var` in boot_ci_bca
          # one var of interest (ie Sepal.Width) but multiple (ie gender & income)
# TODO throw error if `apparent` = TRUE for bootstrap_ci function calls
# TODO concerned about numerical precision. Is default 2 sigfigs inadequate?
          # increase num of digits in tibbles returned
          # Is 2 enough? Is 3 enough? Is 4 enough?
# TODO concerned about speed of loo_cv() compared to sapply() -- this week's sunk cost?
# TODO consequently concerned about increasing test duration. 18.7s thus far.
# TODO write desc
# TODO keep updating API drafts
          # visualizations (how does getting a hist look like?)
          # parameters (? how does getting bias y std error look like)

