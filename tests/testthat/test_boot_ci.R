context("boot_ci")
library(rsample)
library(testthat)
library(purrr)
library(tibble)


# Example Code --------------------------------------------------------
get_tmean <- function(x)
  map_dbl(x,
          function(x)
            mean(analysis(x)[["Sepal.Width"]], trim = 0.1))

set.seed(888)
bt <- bootstraps(iris, apparent = TRUE, times = 200) %>%
  dplyr::mutate(tmean = get_tmean(splits))

results <- rsample:::boot_ci_t(
  bt_resamples = bt %>% dplyr::filter(id != "Apparent"),
  var = "tmean",
  alpha = 0.05,
  theta_obs = bt %>% dplyr::filter(id == "Apparent")
)

results_percentile <- rsample:::boot_ci_perc(
  bt_resamples = bt %>% dplyr::filter(id != "Apparent"),
  var = "tmean",
  alpha = 0.05,
  theta_obs = bt %>% dplyr::filter(id == "Apparent")
)


context("boot_ci: Sufficient Number of Bootstrap Resamples")
test_that("throw warning if theta_se equals 0 or infinity", {
  set.seed(888)
  bt <- bootstraps(iris, apparent = TRUE, times = 1) %>%
    dplyr::mutate(tmean = get_tmean(splits))

  expect_error(
    rsample:::boot_ci_t(
      bt_resamples = bt %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt %>% dplyr::filter(id == "Apparent")
    )
  )
})

test_that('z_pntl has two unique values', {
  expect_false(results$lower == results$upper)
  expect_false(results_percentile$lower == results_percentile$upper)
})

test_that('bootstrap resample estimates are unique',{
  times <- 1
  bt_same <- bootstraps(iris, apparent = TRUE, times = times) %>%
    dplyr::mutate(tmean = rep(3, times + 1))
  expect_error(
    rsample:::boot_ci_t(
      bt_resamples = bt_same %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt_same %>% dplyr::filter(id == "Apparent")
    )
  )
})


context("boot_ci Prompt Errors: Too Many Missing Values")
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
  expect_error(
    rsample:::boot_ci_perc(
      bt_resamples = bt_na %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt_na %>% dplyr::filter(id == "Apparent")
    )
  )

})

test_that('theta_obs is not NA', {
  expect_equal(sum(is.na(bt$tmean)), 0)
})

test_that('bt_resamples is a bootstrap object', {
  expect_equal(class(bt)[1], "bootstraps")
})


test_that('alpha is a reasonable level of significance', {
  expect_error(
    rsample:::boot_ci_perc(
      bt_resamples = bt_na %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 5,
      theta_obs = bt_na %>% dplyr::filter(id == "Apparent")
    )
  )
})


context("boot_ci Check Against Standard Confidence Interval")
test_that(
  'Boostrap estimate of mean is close to estimate of mean from normal distribution',
  {
    set.seed(888)
    n <- 10000
    mean <- 10
    sd <- 1

    rand_nums <- rnorm(n, mean, sd)
    x <- as.data.frame(rand_nums)

    ttest <- t.test(x)
    results_ttest <- tibble(
      lower = ttest$conf.int[1],
      upper = ttest$conf.int[2],
      alpha = 0.05,
      method = "bootstrap-t"
    )

    get_mean <- function(y)
      map_dbl(y,
              function(y)
                mean(analysis(y)[["rand_nums"]], na.rm = TRUE))

    bt_norm <- bootstraps(x, times = 500, apparent = TRUE) %>%
      dplyr::mutate(tmean = get_mean(splits))

    results_boot <- rsample:::boot_ci_t(
      bt_resamples = bt_norm %>% dplyr::filter(id != "Apparent"),
      var = "tmean",
      alpha = 0.05,
      theta_obs = bt_norm %>% dplyr::filter(id == "Apparent")
    )

      expect_equal(results_ttest$lower, results_boot$lower, tolerance = 0.01)
      expect_equal(results_ttest$upper, results_boot$upper, tolerance = 0.01)
  }
)
