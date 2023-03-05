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
