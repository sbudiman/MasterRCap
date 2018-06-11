#' Earthquake Timeline Visualization
#'
#' @description This is a geom function plots earthquake in a timeline. It has the options
#' to take additional plotting input like country, magnitude of earthquakes and
#' number of fatalities.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @details This geom function relies on the new Geom class called GeomTimeline
#' that is created using \code{ggplot2::ggproto}. This new class specifies a number 
#' of attributes and functions that describe how data should be drawn on a plot.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point layer
#' @importFrom grid pointsGrob gpar unit polylineGrob gList
#' @importFrom scales alpha
#'
#' @export
#'
#' @examples
#'    \dontrun{readr::read_delim(file=file.path("data","results"), delim='\t') %>% 
#'             eq_clean_data() %>% 
#'             dplyr::filter(COUNTRY == c("CHINA","JAPAN"), YEAR >= 2000) %>%
#'             ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                             y = COUNTRY,
#'                             color = as.numeric(TOTAL_DEATHS),
#'                             size = as.numeric(EQ_PRIMARY))) +
#'               geom_timeline()}
GeomTimeline <-
  ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   required_aes = c("x"),
                   default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                                              shape = 19, fill = "grey", stroke = 0.5),
                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {
                     
                   if (!("y" %in% colnames(data))) {
                     data$y <- 0.15
                   }
                     
                   coords <- coord$transform(data, panel_scales)
                     
                   points <- grid::pointsGrob(
                     coords$x, coords$y,
                     pch = coords$shape, size = grid::unit(coords$size / 4, "char"),
                     gp = grid::gpar(
                       col = scales::alpha(coords$colour, coords$alpha),
                       fill = scales::alpha(coords$colour, coords$alpha)
                     )
                   )
                   
                   y_lines <- unique(coords$y)
                     
                   lines <- grid::polylineGrob(
                     x = grid::unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                     y = grid::unit(c(y_lines, y_lines), "npc"),
                     id = rep(seq_along(y_lines), 2),
                     gp = grid::gpar(col = "grey",
                                     lwd = .pt)
                   )
                     
                   grid::gList(points, lines)
                   }
  )

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Earthquake Timeline Label
#'
#' @description This geom plots timeline labels of earthquakes, and assumes that
#' \code{geom_timeline} was used to create the timelines
#'
#' @inheritParams ggplot2::geom_text
#' 
#' @param n_max An integer. If used, it only plots the labels for the
#' \code{n_max} largest earthquakes in the selected group in the timeline. Also,
#' the timeline must have size as input parameter.
#'
#' @details This geom function relies on the new Geom class called GeomTimelineLabel
#' that is created using \code{ggplot2::ggproto}. The required aesthetics for this 
#' geom is \code{label} that should contain string for labeling each data point.
#'
#' @importFrom ggplot2 ggproto Geom draw_key_blank layer
#' @importFrom dplyr group_by_ top_n ungroup
#' @importFrom grid polylineGrob textGrob gList
#'
#' @export
#'
#' @examples
#'    \dontrun{readr::read_delim(file=file.path("data","results"), delim='\t') %>% 
#'             eq_clean_data() %>% 
#'             dplyr::filter(COUNTRY == c("CHINA","JAPAN"), YEAR >= 2000) %>%
#'             ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                             y = COUNTRY,
#'                             color = as.numeric(TOTAL_DEATHS),
#'                             size = as.numeric(EQ_PRIMARY))) +
#'               geom_timeline() +
#'               geom_timeline_label(aes(label = LOCATION_NAME),n_max=3)}
GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label"),
    draw_key = ggplot2::draw_key_blank,
    setup_data = function(data, params) {
      if (!is.null(params$n_max)) {
        if (!("size" %in% colnames(data))) {
          stop(paste("'size' aesthetics needs to be",
                     "provided when 'n_max' is defined."))
        }
        data <- data %>%
          dplyr::group_by_("group") %>%
          dplyr::top_n(params$n_max, size) %>%
          dplyr::ungroup()
      }
      data
    },
    draw_panel = function(data, panel_scales, coord, n_max) {
      
      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }
      
      coords <- coord$transform(data, panel_scales)
      n_grp <- length(unique(data$group))
      offset <- 0.2 / n_grp
      
      lines <- grid::polylineGrob(
        x = unit(c(coords$x, coords$x), "npc"),
        y = unit(c(coords$y, coords$y + offset), "npc"),
        id = rep(1:dim(coords)[1], 2),
        gp = grid::gpar(
          col = "grey"
        )
      )
      
      names <- grid::textGrob(
        label = coords$label,
        x = unit(coords$x, "npc"),
        y = unit(coords$y + offset, "npc"),
        just = c("left", "bottom"),
        rot = 45,
        check.overlap = TRUE
      )
      
      grid::gList(lines, names)
    }
  )

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' Theme for better timeline visualization in ggplot2
#'
#' @description  This theme function provides better rendering for 
#' \code{\link{geom_timeline}}
#'
#' @importFrom ggplot2 theme element_blank element_line
#'
#' @export
#'
#' @examples
#'    \dontrun{readr::read_delim(file=file.path("data","results"), delim='\t') %>% 
#'             eq_clean_data() %>% 
#'             dplyr::filter(COUNTRY == c("CHINA","JAPAN"), YEAR >= 2000) %>%
#'             ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                             y = COUNTRY,
#'                             color = as.numeric(TOTAL_DEATHS),
#'                             size = as.numeric(EQ_PRIMARY))) +
#'               geom_timeline() +
#'               geom_timeline_label(aes(label = LOCATION_NAME),n_max=3) +
#'               theme_timeline() + 
#'               labs(size = "Earthquake Magnitude", color = "Fatalities")}
theme_timeline <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 1),
    axis.ticks.y = ggplot2::element_blank(),
    legend.position = "bottom"
  )
}
