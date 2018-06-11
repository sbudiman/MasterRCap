require(readr)
require(magrittr)
require(tidyr)
require(dplyr)
require(stringr)
require(lubridate)
require(grid)
require(ggplot2)

MajorEarthquakes <- readr::read_delim(file=file.path("data","results"), delim='\t', quote = "\"",
  escape_backslash = FALSE,escape_double = TRUE, col_names = TRUE,
  col_types = NULL, na = c('\t',"", "NA"), quoted_na = TRUE, comment = "",
  trim_ws = FALSE, skip = 0, n_max = Inf)

MajorEarthquakes <- MajorEarthquakes %>%
#  dplyr::filter(YEAR >= 0) %>%
  dplyr::mutate(DATE = ISOdate(YEAR,
                                tidyr::replace_na(MONTH,1),
                                tidyr::replace_na(DAY,1)),
         LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE))

#' Cleans earthquake location data
#'
#' @description \code{eq_location_clean} transforms NOAA data frame LOCATION_NAME
#' column by trimming the country name when applicable.
#'
#' @param data A data frame with raw data obtained from NOAA website (see details)
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#' @details The function requires raw date obtained from NOAA site
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_trim
#'
#' @export
#'
#' @examples
#' \dontrun{readr::read_delim(file=file.path("data","results"), delim='\t') %>%
#' eq_location_clean()}
eq_location_clean <- function(){
    for (locindex in 1:length(MajorEarthquakes$LOCATION_NAME)) {
      loc_list <- MajorEarthquakes$LOCATION_NAME[locindex] %>% str_split('\\:') %>% unlist() %>% str_trim(side = "both")
      if (length(loc_list) == 1) {
        MajorEarthquakes$LOCATION_NAME[locindex] <<- loc_list[1]
      } else if (length(loc_list) > 1) {
        MajorEarthquakes$LOCATION_NAME[locindex] <<- paste(loc_list[-1],separator = '', collapse='')
      }
    }
}

#' Cleans earthquake data from NOAA
#'
#' @description \code{eq_clean_data} cleans date, latitude and longitude, and location name
#' from the source NOAA data
#'
#' @param data A data frame with raw data obtained from NOAA website (see details)
#'
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#'
#' @details The function requires raw date obtained from NOAA site
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. It adds
#' a column DATE with cleaned date (Date format), transforms LATITUDE and
#' LONGITUDE columns as numeric objects and transforms LOCATION_NAME by removing
#' the country.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_trim
#'
#' @export
#'
#' @examples
#' \dontrun{readr::read_delim(file=file.path("data","results"), delim='\t') %>%
#' eq_clean_data()}
eq_clean_data <- function(RawMEdf = MajorEarthquakesAD){
  if (!is.data.frame(RawMEdf)) stop("RawMEdf must be a data frame")
  if (!("LOCATION_NAME" %in% colnames(RawMEdf))) stop("RawMEdf data frame must have LOCATION_NAME")
  RawMEdf <- RawMEdf %>%
                  dplyr::mutate(DATE = ISOdate(YEAR,
                                 tidyr::replace_na(MONTH,1),
                                 tidyr::replace_na(DAY,1)),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE))
  for (locindex in 1:length(RawMEdf$LOCATION_NAME)) {
    loc_list <- RawMEdf$LOCATION_NAME[locindex] %>% stringer::str_split('\\:') %>% unlist() %>% stringr::str_trim(side = "both")
    if (length(loc_list) == 1) {
      RawMEdf$LOCATION_NAME[locindex] <- loc_list[1]
    } else if (length(loc_list) > 1) {
      RawMEdf$LOCATION_NAME[locindex] <- paste(loc_list[-1],separator = '', collapse='')
    }
  }
  return(RawMEdf)
}

MajorEarthquakes2 <- readr::read_delim(file=file.path("data","results"), delim='\t') %>%
  eq_clean_data()

readr::read_delim(file=file.path("data","results"), delim='\t') %>%
  eq_clean_data()%>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)

MajorEarthquakesAD %>%
  filter(COUNTRY==c("MEXICO","CHINA"), YEAR>=2010) %>%
  ggplot(aes(x=DATE, y=COUNTRY, size=as.numeric(EQ_PRIMARY))) + geom_timeline()

MajorEarthquakesAD %>%
  filter(COUNTRY==c("MEXICO","CHINA"), YEAR>=2014) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE,
                             y = COUNTRY,
                             color = as.numeric(TOTAL_DEATHS),
                             size = as.numeric(EQ_PRIMARY)
  )) + geom_timeline()

MajorEarthquakesAD %>%
  filter(COUNTRY==c("MEXICO","CHINA"), YEAR>=2010) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE,
                             y = COUNTRY,
                             color = as.numeric(TOTAL_DEATHS),
                             size = as.numeric(EQ_PRIMARY)
  )) + geom_timeline() + geom_timeline_label(aes(label = LOCATION_NAME),n_max=3)

MajorEarthquakesAD %>%
  filter(COUNTRY==c("MEXICO","CHINA"), YEAR>=2010) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE,
                               y = COUNTRY,
                               color = as.numeric(TOTAL_DEATHS),
                               size = as.numeric(EQ_PRIMARY)
  )) +
  geom_timeline() +
  geom_timeline_label(aes(label = LOCATION_NAME),n_max=3) +
  theme_timeline() +
  labs(size = "Earthquake Magnitude", color = "Fatalities")


MajorEarthquakesAD %>%
  filter(COUNTRY==c("CHINA"), YEAR>=2014) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE,
                               color = as.numeric(TOTAL_DEATHS),
                               size = as.numeric(EQ_PRIMARY)
  )) + geom_timeline()


MajorEarthquakesAD %>%
  filter(COUNTRY==c("MEXICO","CHINA"), YEAR>=2010) %>%
  select(DATE,COUNTRY,EQ_PRIMARY,TOTAL_DEATHS,LOCATION_NAME)

readr::read_delim(file=file.path("data","results"), delim='\t') %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "JAPAN" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")

MajorEarthquakesAD %>%
  dplyr::filter(COUNTRY == "INDONESIA" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
