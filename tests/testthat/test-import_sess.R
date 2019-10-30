sess1_exp <- "/Users/heverz/Documents/R_projects/rvent/tests/testthat/sess1_exp"
sess1 <- import_session(inter = FALSE, iox_folder = "/Users/heverz/Documents/R_projects/rvent/inst/extdata",
               comments_tsd = comments_tofill$comments_tsd,
               tofill = comments_tofill$tofill)

test_that("import_session output",{
  expect_known_output(sess1, sess1_exp, print = TRUE)
})



