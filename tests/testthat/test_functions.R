context("Test functions in the MasterRCap package")

filename <- system.file("extdata/results", package = "MasterRCap")
MajorEarthquakes <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns data frame", {
  expect_is(eq_clean_data(MajorEarthquakes), "data.frame")
})

test_that("eq_clean_data$DATE is Date type", {
  expect_is(eq_clean_data(MajorEarthquakes)$DATE, "Date")
})

test_that("eq_clean_data returns LATITUDE and LONGITUDE as numeric values", {
  expect_is(eq_clean_data(MajorEarthquakes)$LATITUDE, "numeric")
  expect_is(eq_clean_data(MajorEarthquakes)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a data frame", {
  expect_is(eq_location_clean(MajorEarthquakes), "data.frame")
})

test_that("geom_timeline returns ggplot object", {
  g <- MajorEarthquakes %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("CHINA", "JAPAN"), YEAR > 2010) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("geom_timeline_label returns ggplot object", {
  g <- MajorEarthquakes %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("CHINA", "JAPAN"), YEAR > 2010) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline_label(aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})

test_that("theme_timeline returns ggplot object", {
  g <- MajorEarthquakes %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("CHINA", "JAPAN"), YEAR > 2010) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    theme_timeline()
  expect_is(g, "ggplot")
})

test_that("eq_map returns leaflet object", {
  l <- MajorEarthquakes %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "INDONESIA" & YEAR >= 2010) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(l, "leaflet")
})

test_that("eq_create_label returns character vector", {
  expect_is(eq_create_label(MajorEarthquakes), "character")
})
