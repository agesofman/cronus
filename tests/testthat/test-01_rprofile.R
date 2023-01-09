test_that("set_path_demeter() works", {
  expect_identical(get_path_demeter(), NULL)
  set_path_demeter(getwd(), prof = "user")
  expect_identical(get_path_demeter(), getwd())
})
