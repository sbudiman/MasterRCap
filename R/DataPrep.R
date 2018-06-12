#' Cleans earthquake location data
#'
#' @description \code{eq_location_clean} transforms NOAA data frame LOCATION_NAME
#' column by trimming the country name when applicable.
#'
#' @param RawMEdf A data frame with raw data obtained from NOAA website (see details).
#' This defaults to sample data frame delivered in package MajorEarthquakes.
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
eq_location_clean <- function(RawMEdf = MajorEarthquakes){
    for (locindex in 1:length(RawMEdf$LOCATION_NAME)) {
      loc_list <- RawMEdf$LOCATION_NAME[locindex] %>% str_split('\\:') %>% unlist() %>% str_trim(side = "both")
      if (length(loc_list) == 1) {
        RawMEdf$LOCATION_NAME[locindex] <- loc_list[1]
      } else if (length(loc_list) > 1) {
        RawMEdf$LOCATION_NAME[locindex] <- paste(loc_list[-1],separator = '', collapse='')
      }
    }
    return(RawMEdf)
}

#' Cleans earthquake data from NOAA
#'
#' @description \code{eq_clean_data} cleans date, latitude and longitude, and location name
#' from the source NOAA data
#'
#' @param RawMEdf A data frame with raw data obtained from NOAA website (see details).
#' This defaults to sample data frame delivered in package MajorEarthquakes.
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
eq_clean_data <- function(RawMEdf = MajorEarthquakes){
  if (!is.data.frame(RawMEdf)) stop("RawMEdf must be a data frame")
  if (!("LOCATION_NAME" %in% colnames(RawMEdf))) stop("RawMEdf data frame must have LOCATION_NAME")
  RawMEdf <- RawMEdf %>%
                  dplyr::mutate(DATE = ISOdate(YEAR,
                                 tidyr::replace_na(MONTH,1),
                                 tidyr::replace_na(DAY,1)),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE))
  for (locindex in 1:length(RawMEdf$LOCATION_NAME)) {
    loc_list <- RawMEdf$LOCATION_NAME[locindex] %>% stringr::str_split('\\:') %>% unlist() %>% stringr::str_trim(side = "both")
    if (length(loc_list) == 1) {
      RawMEdf$LOCATION_NAME[locindex] <- loc_list[1]
    } else if (length(loc_list) > 1) {
      RawMEdf$LOCATION_NAME[locindex] <- paste(loc_list[-1],separator = '', collapse='')
    }
  }
  return(RawMEdf)
}

