#' Plot Timeline (Label)
#' 
#' This method acts behind de scenes creating a layer to plot the output of GeomTimelinelabel function.
#'
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
#' @param n_max An integer representing the maximum number of data
#'   labels to draw based on the magnitude of the earthquakes
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
#' @return Creates a layer to plot the GeomTimelineLabel output function. Updates the
#'         graphic inserting new content/visuals.
#' @export
#'
#' @examples
#' \dontrun{ggplot(data = y, aes(x = DATE, country = COUNTRY, label = LOCATION, 
#'                      magnitude = MAGNITUDE)) + 
#'   geom_timeline(ctry = "USA", xmin = dmy("01/01/2010"), xmax = dmy("01/01/2016")) + 
#'   geom_timeline_label(n_max = 5, ctry = "USA", 
#'                       xmin = dmy("01/01/2010"), 
#'                       xmax = dmy("01/01/2016")) + 
#'   theme_classic()}

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, n_max = 5,
                                xmin = lubridate::dmy("01/01/2010"), 
                                xmax = lubridate::dmy("01/01/2016"),
                                ctry = "JAPAN",
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max,
                  xmin = xmin, xmax = xmax, ctry = ctry,...)
  )
}

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("label", "magnitude"),
                                      draw_key = ggplot2::draw_key_blank,
                                      setup_data = function(data, params){

                                        data <- data %>%
                                          dplyr::filter(data$x >= params$xmin, 
                                                        data$x < params$xmax,
                                                        data$country == params$ctry) %>%
                                          dplyr::arrange(desc(magnitude)) %>%
                                          head(params$n_max)
                                      },
                                      draw_panel = function(data, panel_scales, coord, 
                                                            n_max, xmin, xmax, ctry){
                                        
                                        coords <- coord$transform(data, panel_scales)
                                        
                                        pos_vector <- c(rep(0.4, length(coords$x)))
                                        
                                        lines <- grid::polylineGrob(
                                          x = unit(c(coords$x, coords$x), "npc"),
                                          y = unit(c(pos_vector - 0.28, pos_vector), "npc"),
                                          id = rep(1:length(coords$x), 2),
                                          gp = grid::gpar(col = "black")
                                        )
                                        
                                        names <- grid::textGrob(
                                          label = coords$label,
                                          x = unit(coords$x, "npc"),
                                          y = unit(pos_vector + 0.01, "npc"),
                                          gp = grid::gpar(fontsize = 8),
                                          just = c("left", "bottom"),
                                          rot = 45
                                        )
                                        
                                        ctry_label <- grid::textGrob(
                                          label = ctry,
                                          x = unit(0.5, "npc"),
                                          y = unit(0.95, "npc"),
                                          gp = grid::gpar(fontsize = 18, 
                                                          fontface = "bold"),
                                          just = c("center", "center")
                                        )
                                        
                                        grid::gList(lines, names, ctry_label)
                                        
                                      })


