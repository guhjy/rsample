# Bootstrap Confidence Intervals

#' @importFrom stats sd
#' @export
boot_ci_t <- function(bt_resamples, var, alpha, data = NULL, theta_obs) {

  theta_obs <- theta_obs[[var]]

  if (all(is.na(theta_obs)))
    stop("All statistics (theta_obs) are missing values.", call. = FALSE)

  theta_se <- sd(bt_resamples[[var]], na.rm = TRUE)/
    sqrt(sum(!is.na((bt_resamples[[var]]))))

  # then write a test case for that
  if (theta_se == (0 | Inf))
    stop("Your standard error (theta_se) is 0 or infinity.", call. = FALSE)
  # rlang::abort()

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










