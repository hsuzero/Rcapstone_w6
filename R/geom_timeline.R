#' Plot Timeline
#'
#' This method acts behind de scenes creating a layer to plot the output of GeomTimeline function.
#'
#' @param mapping A set of aesthetic mappings created by aes()
#' @param data A dataframe object containing the earthquake data points
#'   from the \code{eq_location_clean} function that is to be displayed
#'   in this layer of the plot
#' @param stat A string representing the statistical transformation for
#'   this layer of the plot
#' @param position A string representing the positional adjustment
#' @param na.rm A logical value to indicate whether to raise warning when
#'   missing values are removed for the plot
#' @param xmin An integer representing the minimum date value using the
#'   \code{lubridate::dmy("dd/mm/yyyy")} function, inclusive
#' @param xmax An integer representing the maximum date value using the
#'   \code{lubridate::dmy("dd/mm/yyyy")} function, exclusive
#' @param ctry A string representing the country filter for the plot
#' @param show.legend A logical value to indicate whether this layer of
#'   the plot should be included in the legends
#' @param inherit.aes A logical value to indicate whether to override the
#'   default aesthetics or to combine with them
#' @param ... A list representing other aesthetics to be passed on to this
#'   layer of the plot
#'
#' @import ggplot2
#' @importFrom lubridate dmy
#' @importFrom dplyr filter arrange
#'
#' @return Creates a layer to plot the GeomTimeline output function. Updates the
#'         graphic inserting new content/visuals.
#' @export
#'
#' @examples
#' \dontrun{ggplot(data = y, aes(x = DATE, country = COUNTRY, label = LOCATION, 
#'                      magnitude = MAGNITUDE)) + 
#'   geom_timeline(ctry = "USA", xmin = dmy("01/01/2010"), xmax = dmy("01/01/2016")) +
#'   theme_classic()}

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          xmin = lubridate::dmy("01/01/2010"), 
                          xmax = lubridate::dmy("01/01/2016"),
                          ctry = "JAPAN",
                          show.legend = NA, inherit.aes = TRUE,...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ctry = ctry,...)
  )
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x", "country"),
                                 default_aes = ggplot2::aes(shape = 21, 
                                                            colour = "black", 
                                                            alpha = 0.5),
                                 setup_data = function(data, params){
                                   data <- data %>%
                                     dplyr::filter(data$x >= params$xmin, 
                                                   data$x < params$xmax,
                                                   data$country == params$ctry)
                                 },
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord, 
                                                       xmin, xmax, ctry){
                                   
                                   coords <- coord$transform(data, panel_scales)
                                   
                                   pos_vector <- c(rep(0.1, length(coords$x)))
                                   
                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = pos_vector,
                                     pch = coords$shape,
                                     gp = grid::gpar(fill = coords$colour,
                                                     alpha = coords$alpha)
                                   )
                                 })