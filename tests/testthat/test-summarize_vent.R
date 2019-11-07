vsummary <- summarize_vent(sess1, inter = FALSE)
vsummary_exp <- "/Users/heverz/Documents/R_projects/rvent/tests/testthat/vsummary_exp"

test_that("summary_vent output", {
  expect_known_output(vsummary, vsummary_exp, print = TRUE)
})
