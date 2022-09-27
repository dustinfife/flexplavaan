context("helper_plot_modifiers works")

p = visualize(small, plot="trail", subset=1:3) 
test_that("remove_and_add_gg_element works", {
  np = remove_and_add_gg_element(p, geom_abline(slope=0, intercept=0))  
  expect_true(class(np[2,1]$layers[[2]]$geom)[1] == "GeomAbline")
})

test_that("remove_geom works", {
  # make sure original layer has geomsmooth
  all_layers_o = lapply(p[1,2]$layers, function(x) class(x$geom)) %>% unlist
  expect_true("GeomSmooth" %in% all_layers_o)
  
  # and make sure it's now removed
  np = remove_geom(p[1,2], "GeomSmooth")  
  all_layers = lapply(np$layers, function(x) class(x$geom)) %>% unlist
  expect_false("GeomSmooth" %in% all_layers)
  
})

test_that("smooth_method_check works", {
  expect_match(smooth_method_check("lm"), "lm")
  expect_match(smooth_method_check("poisson"), "poisson")
  expect_match(smooth_method_check("Gamma"), "Gamma")
  expect_match(smooth_method_check("quadratic"), "x, 2")
  expect_match(smooth_method_check("cubic"), "x, 3")
  expect_match(smooth_method_check("rlm"), "rlm")
  expect_match(smooth_method_check(), "loess")
})
