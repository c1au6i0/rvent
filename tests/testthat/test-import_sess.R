sess1_exp <- "/Users/heverz/Documents/R_projects/rvent/tests/testthat/sess1_exp"
sess1 <- import_session(
  inter = FALSE, iox_folder = "/Users/heverz/Documents/R_projects/rvent/inst/extdata/data1",
  comments_tsd = c("1 heroin 600 ug/kg", "9 heroin NA NA"),
  tofill = c(1:2)
)

test_that("import_session output", {
  expect_known_output(sess1, sess1_exp, print = TRUE)
})
