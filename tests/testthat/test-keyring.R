test_that("keyring works", {

  ringname <- "my_ringname"
  password <- "my_password"
  create_keyring(ringname, password)

  log_in(ringname, password)
  check_keyring(ringname)

  add_key(ringname = ringname,
          provider = "usgs",
          username = "usgs_username",
          password = "usgs_password")

  expect_identical(get_username(ringname, "usgs"), "usgs_username")
  expect_identical(get_password(ringname, "usgs"), "usgs_password")

  delete_key(ringname, "usgs")
  delete_keyring(ringname)

})
