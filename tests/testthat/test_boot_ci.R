context("boot_ci")
library(testthat)
library(rsample)
library(purrr)

# things to check for:

# ? all same estimates
# median with 10 data points
# if they didn't generate enough B
# num of unique datapoints
# only one unique === FAILURE
# stop divide by 0

  # bug
  # 2 tibbles -- look all counts the same but underlying is different


get_tmean <- function(x)
  map_dbl(x,
          function(x)
            mean(analysis(x)[["Sepal.Width"]], trim = 0.1))

set.seed(888)
bt <- bootstraps(iris, apparent = TRUE, times = 10000) %>%
  dplyr::mutate(tmean = get_tmean(splits))
results <- rsample:::boot_ci_t(
  bt_resamples = bt %>% dplyr::filter(id != "Apparent"),
  var = "tmean",
  alpha = 0.05,
  theta_obs = bt %>% dplyr::filter(id == "Apparent")
)


# impute some missing values in iris
test_that('upper & lower confidence interval does not contain NA', {
  iris_na<- iris
  iris_na$Sepal.Width[c(1, 51, 101)] <- NA

  set.seed(888)
  bt_na <- bootstraps(iris_na, apparent = TRUE, times = 10000) %>%
    dplyr::mutate(tmean = rep(NA_real_, 10001))

  expect_error(
    rsample:::boot_ci_t(
      bt_resamples = bt_na %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt_na %>% dplyr::filter(id == "Apparent")
    )
  )
})


test_that('z_pntl has two unique values', {
  expect_false(results$lower == results$upper)
})


test_that('bt_resamples is a bootstrap object', {
  expect_equal(class(bt)[1], "bootstraps")
})


test_that('theta_obs is not NA', {
  expect_equal(sum(is.na(bt$tmean)), 0)
})


# check against random normal data and standard CI
test_that(
  'Normally generated mean is in the ball-park of the bootstrap
  confidence interval or parameter estimate',
  {
    set.seed(888)
    x <- rnorm(n = 500, mean = 10, sd = 1)
    ttest <- t.test(x)
    results_tdist<- ttest$conf.int[1:2]

    bt_norm <- bootstraps(x, times = 10000, apparent = TRUE) %>%
      dplyr::mutate(tmean = get_tmean(splits))

    results_norm <- rsample:::boot_ci_t(
      bt_resamples = bt_norm %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt_norm %>% dplyr::filter(id == "Apparent")
    )
  }
)

# t.test
# the mean
# get p
# expects_equal
# within another tolerance
# double precision
# lower numerical tolearnce

# expect_equivalent
# looks at the values not the properties like rownames or colnames


#
# test_that('all same estimates' , {
#
# })
