# Bootstrap Confidence Intervals

# things to check for:
# missing data
# all same estimates
# make sure that bt_resamples is a boostrap object (inherits)
# make sure theta_obs is not NA
# make sure that z_pntl has two unique values
# check against rand normal data and standard CI
#' @importFrom stats sd
#' @export
boot_ci_t <- function(bt_resamples, var, alpha, data = NULL, theta_obs) {
  theta_obs <- theta_obs[[var]]
  theta_se <- sd(bt_resamples[[var]], na.rm = TRUE)/
    sqrt(sum(!is.na((bt_resamples[[var]]))))
  z_dist <- (bt_resamples[[var]] - theta_obs)/theta_se
  z_pntl <- quantile(z_dist, probs = c(alpha/2, 1 - (alpha)/2), na.rm = TRUE)
  ci <- theta_obs + z_pntl * theta_se
  tibble(
    lower = ci[1],
    upper = ci[2],
    alpha = alpha,
    method = "bootstrap-t"
  )
}











