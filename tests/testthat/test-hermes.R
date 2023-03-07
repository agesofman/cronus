test_that("hermes download works", {

  # Set path
  path_hermes <- file.path(tempdir(), "hermes-main")
  path_hermes_zip <- file.path(tempdir(), "hermes-main.zip")

  # Delete project
  unlink(path_hermes, recursive = TRUE, force = TRUE)
  unlink(path_hermes_zip, recursive = TRUE, force = TRUE)

  # Download project
  expect_no_error(download_hermes(tempdir()))

  # Delete project
  unlink(path_hermes, recursive = TRUE, force = TRUE)
  unlink(path_hermes_zip, recursive = TRUE, force = TRUE)

})

test_that("hermes download works", {

  path_hermes <- tempdir()

  # Test path
  expect_no_error(get_path_hermes_obj("objectname", "projectname", path_hermes))

  # Save and load object
  x <- 1:3
  expect_no_error(save_obj(x, "objectname", "projectname", path_hermes))
  expect_no_error(read_obj("objectname", "projectname", path_hermes))

})
