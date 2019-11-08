class(dat_vent) <- c("data.frame", "vent")
fig <- autoplot.vent(dat_vent, fsave = FALSE)
fig_exp <- "/Users/heverz/Documents/R_projects/rvent/tests/testthat/fig_exp"


test_that("autoplot.vent", {
  expect_known_output(fig, fig_exp, print = TRUE)
})
