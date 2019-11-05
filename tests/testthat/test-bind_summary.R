bsummary <- bind_summary("/Users/heverz/Documents/R_projects/rvent/inst/extdata/summary", inter = FALSE)
bsummary_exp <- "/Users/heverz/Documents/R_projects/rvent/tests/testthat/bsummary_exp"

test_that("bind_summary output",{
  expect_known_output(bsummary, bsummary_exp, print = TRUE)
})


