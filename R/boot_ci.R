#' Bootstrap Confidence Intervals
#'
#' @importFrom stats sd
#' @export
boot_ci_t <- function(bt_resamples, var, alpha, data = NULL, theta_obs) {

  theta_obs <- theta_obs[[var]]

  if (all(is.na(theta_obs)))
    stop("All statistics (theta_obs) are missing values.", call. = FALSE)

  theta_se <- sd(bt_resamples[[var]], na.rm = TRUE) / sqrt(sum(!is.na((bt_resamples[[var]]))))

  # then write a test case for that
  if (theta_se == (0 | Inf))
    stop("Your standard error (theta_se) is 0 or infinity.", call. = FALSE)

  z_dist <- (bt_resamples[[var]] - theta_obs) / theta_se
  z_pntl <- quantile(z_dist, probs = c(alpha/2, 1 - (alpha)/2), na.rm = TRUE)
  ci <- theta_obs + z_pntl * theta_se

  tibble(
    lower = ci[1],
    upper = ci[2],
    alpha = alpha,
    method = "bootstrap-t"
  )
}

boot_ci_perc <- function(bt_resamples, var, alpha, data = NULL, theta_obs) {
  z_dist <- bt_resamples[[var]]

  if (all(is.na(z_dist)))
  stop("All statistics (z_dist) are missing values.", call. = FALSE)

  if (0<alpha && alpha>1)
  stop("Your significance level (alpha) is unreasonable.", call. = FALSE)

  ci <- quantile(z_dist, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
  tibble(
    lower = ci[1],
    upper = ci[2],
    alpha = alpha,
    method = "percentile"
  )
}


# TO-DO return tibble with upper lower alpha
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
  ci_bca = as.numeric(quantile(bt_resamples$theta_i, c(lower_percentil, upper_percentile))) # putting those percentiles in place of alpha/2, 1-alpha/
  return(ci_bca)
}










