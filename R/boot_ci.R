#' Bootstrap Confidence Intervals
#'
#' A bootstrap sample is a sample that is the same size as the original data set that is made using replacement.  This results in analysis samples that have multiple replicates of some of the original rows of the data. The assessment set is defined as the rows of the original data that were not included in the bootstrap sample. This is often referred to as the "out-of-bag" (OOB) sample.

#' @details
#' The argument `bt_resamples` is the data.frame comprised of B bootstrap resamples and an extra apparent resample (copy of the original data set) appended at the end to calculate estimators.
#'
#' The `var` argument is a character string of the variable(s) of interest.
#'
#' The `alpha`
#' @return  An tibble with the `lower` end of the confidence interval, `upper` end of the confidence interval, `alpha` level of significance, and `method` of constructing the confidence interval.


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










