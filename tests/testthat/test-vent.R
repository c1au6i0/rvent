test_that("new_vent_ok", {
  expect_success(expect_s3_class(new_vent(dat_vent), c("data.frame", "vent")))

  expect_error(new_vent(as.list(dat_vent)))
})


test_that("vent_validator", {
  dat_vent2 <- new_vent(dat_vent)

  expect_s3_class(validate_vent(dat_vent2), c("data.frame", "vent"))
  expect_error(validate_vent(dat_vent[, 1:4]))
})
