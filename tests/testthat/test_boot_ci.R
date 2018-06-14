context("boot_ci")
library(testthat)
library(rsample)


# things to check for:
# missing data
# all same estimates
# make sure that bt_resamples is a boostrap object (inherits)
# make sure theta_obs is not NA
# make sure that z_pntl has two unique values
# check against rand normal data and standard CI



get_tmean <- function(x)
  purrr::map_dbl(x,
          function(x)
            mean(analysis(x)[["Sepal.Width"]], trim = 0.1)
  )
set.seed(646)
bt <- bootstraps(iris, apparent = TRUE, times = 10000) %>%
  dplyr::mutate(tmean = get_tmean(splits))

results <- rsample:::boot_ci_t(
  bt_resamples = bt %>% dplyr::filter(id != "Apparent"),
  var = "tmean",
  alpha = 0.05,
  theta_obs = bt %>% dplyr::filter(id == "Apparent")
)



test_that('z_pntl has two unique values', {
  expect_false(results$lower == results$upper)
})


test_that('bt_resamples is a bootstrap object', {
  expect_equal(class(bt)[1], "bootstraps")
})


test_that('theta_obs is not NA', {
  expect_equal(sum(is.na(bt$tmean)), 0)
})



#
# test_that('missing data', {
#
# })
#
# test_that('all same estimates' , {
#
# })


