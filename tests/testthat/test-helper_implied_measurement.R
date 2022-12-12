context("helper_implied_measurement works")

#
plot_data =   prepare_measurement_data(small)
plot_data_2 = prepare_measurement_data(small, small_mis)
test_that("prepare_measurement_data works", {
  expect_equal(plot_data$Variable%>%unique, c("x1", "x2", "x3", "y1", "y2", "y3", "z"))
  expect_equal(plot_data_2$model%>%unique, c("small", "small_mis"))
})

test_that("compute_latent_residual works", {
  # make sure residuals are computed correctly
  residual = with(plot_data, f1-slope_f1*Observed + intercept_f1)
  expect_equal(residual%>%as.numeric, compute_latent_residual("f1", plot_data)$residual_f1)
})

test_that("order_flexdata_by_slopes works", {
  a = order_flexdata_by_slopes(plot_data, "f1", F)
  b = order_flexdata_by_slopes(plot_data, "f1", T)
  expect_equal(a$Variable, unique(plot_data$Variable))
  expect_equal(b$Variable[1], "x3")
  expect_equal(sort(a$Variable), sort(b$Variable))
  expect_equal(order_flexdata_by_slopes(plot_data_2, "f1", T)$Diff[1], .833, tol=.001)
})

test_that("standardized_observed works", {
  expect_equal(sum(standardize_observed(small)), 0, tol=.001)
  expect_equal(unique(standardize_observed(small, small_mis)$model), c("model", "model2"))
})

test_that("check_for_latent works", {
  expect_null(check_for_latent(small, "f1"))
  expect_error(check_for_latent(small, "f12"))
})

test_that("return_actual_slope works", {
  expect_equal(return_actual_slope("x1", "f1", plot_data) %>% as.numeric, .787, tol=.01)
})

test_that("get_slopes works", {
  expect_equal(get_slopes(small)[1,1], .7457, .001)
})

test_that("get_intercepts works", {
  expect_equal(sum(get_intercepts(get_slopes(small), small)),0,tol=.001)
})

test_that("latent_observed_implied works", {
  expect_equal(dim(latent_observed_implied(small)), c(7,2))
})

test_that("find_common_latent works", {
  expect_equal(find_common_latent(small, small_diflat), "f1")
  expect_error(find_common_latent(force_fit$lavaan, force_exp$lavaan))
  expect_equal(find_common_latent(small, NULL), "f2")
  expect_equal(find_common_latent(small_uni, NULL), "f1")
})

test_that("rank_worst_fitting_latent works", {
  expect_equal(rank_worst_fitting_latents(small_mis)[1], "f1")
  expect_equal(rank_worst_fitting_latents(small_uni)[1], "f1")
})






#