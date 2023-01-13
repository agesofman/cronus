test_that("Cropmaps CDL and Daymet functions work", {

  # Define required variables
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))
  date <- as.Date("2020-07-15")
  dir <- tempdir()

  # Create the object
  x <- new("Cropmaps", region = region, date = date, dir = dir)
  y <- new("Daymet", region = region, date = date, dir = dir)
  z <- new("Parameters", region = region, date = date, dir = dir)

  # Download the data
  expect_no_error(download(x, "cdl"))
  expect_no_error(download(y, c("tmin", "tmax")))

  # Load the data
  rast_cdl <- load_map(x, "cdl")
  expect_s4_class(rast_cdl, "SpatRaster")
  rast_tmin <- load_map(y, "tmin")
  expect_s4_class(rast_tmin, "SpatRaster")

  # Project the rasters
  expect_no_error(project(x, y, variablex = "cdl", variabley = "tmin", newvarname = "cdl_projected"))

  # Load the data
  rast_cdl_projected <- load_map(x, "cdl_projected")
  expect_s4_class(rast_cdl, "SpatRaster")

  # Get the current categories data.frame
  df_cat <- terra::cats(rast_cdl_projected)[[1]]

  # Define a transformation table
  tb_rcl <- cbind(a = c(1:4, 121:124, 141:143), b = c(1:4, rep(82, 4), rep(63, 3)))

  # Write the metadata
  expect_no_error(write_metadata(x, name = "my_metadata", df_cat = df_cat, tb_rcl = tb_rcl))

  # Read the metadata
  list_md <- read(x, name = "my_metadata")
  expect_type(list_md, "list")

  # Recode the raster
  expect_no_error(recode(x, variable = "cdl_projected", mdname = "my_metadata", newvarname = "cdl_recoded"))

  # Load the data
  rast_cdl_projected <- load_map(x, "cdl_recoded")
  expect_s4_class(rast_cdl, "SpatRaster")

  # Summarize the raster
  df_summary <- summarize(x, "cdl_recoded")
  expect_type(df_summary, "list")

  # Define the cardinal temperatures metadata
  tb_ct <- rbind('Dry Beans' = c(Tb = 4.0,  To = 23.0, Tc = 32.0),
                 'Corn'      = c(Tb = 10.0, To = 30.0, Tc = 50.0),
                 'Soybeans'  = c(Tb = 6.0,  To = 26.0, Tc = 39.0))

  # Write the metadata
  write_metadata(z, name = "default", tb_ct = tb_ct)

  # Read the metadata
  tb_ct <- read(z, "default")$tb_ct

  # Derive GDD
  expect_no_error(derive(y, x, "gdd", varxy = c("tmin", "tmax", "cdl_recoded"), tb_ct = tb_ct))
  rast_gdd <- load_map(y, "gdd")
  expect_s4_class(rast_gdd, "SpatRaster")

  # Compose the rasters into a time-series
  a <- compose(y, x, variablex = "gdd", variabley = "cdl_recoded", fun = "mean")
  expect_type(a, "list")

})
