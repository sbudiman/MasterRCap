context("Test functions in the MasterRCap package")

filename <- system.file("extdata/results", package = "MasterRCap")
MajorEarthquakes <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns data frame", {
  expect_is(eq_clean_data(MajorEarthquakes), "data.frame")
})
