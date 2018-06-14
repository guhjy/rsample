context("boot_ci")
library(testthat)
library(rsample)


# things to check for:

# ? missing data
# Where do I care about missing data? bt?
# original dataset has Nas

# ? all same estimates
# median with 10 data points
# if they didn't generate enough B
# num of unique datapoints
# only one unique === FAILURE
# stop divide by 0
# hey all of your bootstrap estimates are the same
# (as original boot package? that means the user has
# to dowload it which is annoying --too bad for now I guess. I need the
# straightforward benchmark that it provides.)

# X make sure that bt_resamples is a boostrap object (inherits)
# X make sure theta_obs is not NA
# X make sure that z_pntl has two unique values

# ? check against rand normal data and standard CI
  # how does this comparison look like?
  # EX
# b=10000
  # rnorm 500
  # analyhtically tractically , mean
  # t.test
  # the mean
  # get p\
  # expects_equal
  # within another tolerance
  # diable doub,e precision
  # lower numerical tolearnce

  expect_equivalent
  # looks at the values not the properties like rownames or colnames

  # bug
  # 2 tibbles -- look all counts the same but underlying is different


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


# which dataframe is ocmplete?
test_that('there is no missing data?', {

})

test_that('no missing values', {
  expect_identical(testing_data, na.omit(testing_data))
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



#
# test_that('missing data', {
#
# })
#
# test_that('all same estimates' , {
#
# })


