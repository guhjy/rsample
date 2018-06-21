#' Bootstrap Confidence Intervals
#'
#' @importFrom stats sd
#' @export
boot_ci_t <- function(bt_resamples, stat, alpha, data = NULL, theta_obs) {

  theta_obs <- theta_obs[[stat]]

  if (all(is.na(theta_obs)))
    stop("All statistics (theta_obs) are missing values.", call. = FALSE)

  theta_se <- sd(bt_resamples[[stat]], na.rm = TRUE) / sqrt(sum(!is.na((bt_resamples[[stat]]))))

  # then write a test case for that
  if (theta_se == 0 | theta_se == Inf)
    stop("Your standard error (theta_se) is 0 or infinity.", call. = FALSE)

  z_dist <- (bt_resamples[[stat]] - theta_obs) / theta_se
  z_pntl <- quantile(z_dist, probs = c(alpha/2, 1 - (alpha)/2), na.rm = TRUE)
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



boot_ci_bca <- function(bt_resamples, stat, alpha, var, data = NULL, theta_obs){

  # Process apparent resample
  apparent_sample <- theta_obs$splits[[1]]
  dat <- analysis(apparent_sample)
  data <- as.data.frame(dat[[var]])

  theta_hat <- mean(bt_resamples[[stat]])

  ### Estimating Z0 bias-correction
  po <- mean(bt_resamples[[stat]] <= theta_hat)
  Z0 <-  qnorm(po)
  Za <-  qnorm(1-alpha/2)

  # TODO try loo(), clock the performance against sapply implementation
  leave_one_out_theta = sapply(1:length(data), function(i){
    leave_out_data = data[-i] # leave out the ith observation
    theta_i = mean(leave_out_data)
  return(theta_i)    # returns a vector of means. mean of each bootstrap resample.
  })


# Yet another abysmal start -----------------------------------------------
  # start over!!

  # run this again
  #bt <- bootstraps(iris, apparent = TRUE, times = 500) %>%
  #dplyr::mutate(tmean = get_tmean(splits))

  # run parts inside the function
 # results_bca <- rsample:::boot_ci_bca(
  bt_resamples = bt %>% dplyr::filter(id != "Apparent")
  stat = "tmean"
  alpha = 0.05
  var = "Sepal.Width"
  theta_obs = bt %>% dplyr::filter(id == "Apparent")
#)

  # Process apparent resample
  apparent_sample <- theta_obs$splits[[1]]
  dat <- analysis(apparent_sample)
  data <- as.data.frame(dat[[var]])
    # now data looks like a df with a single var called dat[[var]]

  # pass the original data into loo_cv to generate LOO resamples
  loo_resamples <- loo_cv(data)



  # GENERATE ONE RESAMPLE FIRST
  loo_one_resample <- loo_resamples$splits[[1]]
  loo_one_resample_data <- analysis(loo_one_resample)

  nrow(loo_one_resample_data)  # 150 original data entries - 1 = 149
  str(loo_one_resample_data) # data.frame object means you have to subset out var of interest
  theta_one_resample_data <- mean(loo_one_resample_data[["dat[[var]]"]])

  # compile all the steps above without intermediate variables
  loo_results <- loo_cv(data) %>%
    mutate(theta_i = get_theta_i(splits))

  loo_results

  get_theta_i <- function(x)
    map_dbl(x,
            function(x)
              mean(analysis(x)[["dat[[var]]"]]))



    #analysis(pluck('splits'))
  class(loo_results)
  str(loo_results)
    # mutate(stuff = map_data(analysis(splits)))

# inspo
bt_sample_size <- map_dbl(bt_resamples$splits, get_mean)

# inspo
get_mean <- function(split, ...) {
  bt_samp <- analysis(split)
  theta_i <- mean(bt_samp[["data"]])
  return(theta_i)
}



  # now repeat the above process for all the data


  # theta_hat <- mean(bt_resamples[[stat]])



    #mutate(loo_analysis_resample = map(analysis(splits))

# mutate(theta_i = map_dbl(splits, get_mean))



#  many_analysis <- lapply(loo_resamples$splits, analysis)


  loo_analysis <- analysis(loo_resamples$splits[[1]])

  str(loo_analysis)
  str(data)
  colnames(data)
  colnames(loo_analysis)

  loo_analysis[[]]

  mean(loo_analysis)
  mean(loo_analysis[["data"]])

  loo_mean <- map_dbl(loo_resamples$splits, get_mean)


  leave_one_out_theta <- loo_cv(data) %>% mutate(mean(analysis(splits)))

bt_sample_size <- map_dbl(bt_resamples$splits, get_mean)

get_mean <- function(split, ...) {
  bt_samp <- analysis(split)
  theta_i <- mean(bt_samp[["data"]])
  return(theta_i)
}



  theta_minus_one = mean(leave_one_out_theta)
  a = sum( (theta_minus_one - leave_one_out_theta)^3)/( 6 *(sum( (theta_minus_one - leave_one_out_theta)^2))^(3/2) )

  Zu = (Z0+Za)/(1-a*(Z0+Za)) + Z0 # upper limit for Z
  Zl = (Z0-Za)/(1-a*(Z0-Za)) + Z0 # Lower limit for Z
  lower_percentile = pnorm(Zl,lower.tail = TRUE) # percentile for Z
  upper_percentile = pnorm(Zu,lower.tail = TRUE) # percentile for Z
  ci_bca = as.numeric(quantile(bt_resamples[[stat]], c(lower_percentile, upper_percentile))) # putting those percentiles in place of alpha/2, 1-alpha/

  tibble(
  lower = ci_bca[1],
  upper = ci_bca[2],
  alpha = alpha,
  method = "bca"
  )
}










