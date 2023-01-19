test_that(".Renviron editing functions work", {

  # Initialize
  newline1 <- "my_variable=1"
  newline2 <- "my_variable=2"
  type <- "environ"

  # Get path
  path_startup <- get_startup_path(type)

  # Read file
  file_startup <- read_startup(type)

  # Add line to file
  add_startup_line(newline1, type)
  file_startup_1 <- read_startup(type)

  # Add line to file
  add_startup_line(newline2, oldline = 'my_variable=', type)
  file_startup_2 <- read_startup(type)

  # Add same line to file
  add_startup_line(newline1, oldline = 'my_variable=', type)
  file_startup_3 <- read_startup(type)

  # TEST: Expect that the two files are identical
  expect_identical(file_startup_1, file_startup_3)

  # Write file
  writeLines(file_startup, con = path_startup)

})

test_that(".Rprofile editing functions work", {

  # Initialize
  newline1 <- "my_variable=1"
  newline2 <- "my_variable=2"
  type <- "profile"

  # Get path
  path_startup <- get_startup_path(type)

  # Read file
  file_startup <- read_startup(type)

  # Add line to file
  add_startup_line(newline1, type)
  file_startup_1 <- read_startup(type)

  # Add line to file
  add_startup_line(newline2, type)
  file_startup_2 <- read_startup(type)

  # Add same line to file
  add_startup_line(newline1, type)
  file_startup_3 <- read_startup(type)

  # TEST: Expect that the two files are identical
  expect_identical(file_startup_1, file_startup_3)

  # Write file
  writeLines(file_startup, con = path_startup)

})


test_that("set_path_demeter() works", {

  # Initialize
  type <- "environ"
  path1 <- getwd()
  path2 <- NULL
  newline1 <- paste0("path_demeter='", path1, "'")
  newline2 <- paste0("path_demeter='", path2, "'")

  # Get path
  path_startup <- get_startup_path(type)

  # Read file
  file_startup <- read_startup(type)

  # Add the path to .Renviron
  set_path_demeter(path1)
  file_startup_1 <- read_startup(type)

  # TEST Expect to find newline1 in the file
  expect_true(newline1 %in% file_startup_1)

  # Add line to .Renviron
  set_path_demeter(path2)
  file_startup_2 <- read_startup(type)

  # TEST Expect not to find newline1 in the file
  expect_false(newline1 %in% file_startup_2)

  # Write file
  writeLines(file_startup, con = path_startup)

})
