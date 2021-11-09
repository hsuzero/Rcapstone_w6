#' Create Map
#' 
#' This function plots a straightforward map using circles to determine the eartquake location. The radius
#' of the circle is according to the magnitude (in Richter Scale) of the earthquake.
#'
#' @param filtered_data A DataFrame with DATE, LONGITUDE, LATITUDE, and EQ_PRIMARY columns.
#' 
#' @param annot_col The information to be displayed inside of the popup.
#' 
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' 
#' @import dplyr
#' 
#' @return A leaflet object representing the map of earthquake points
#' 
#' @export
#'
#' @examples
#' \dontrun{readr::read_delim("./inst/extdata/dataset.txt", delim = "\t") %>% 
#'   eq_clean_data() %>% 
#'   eq_location_clean() %>%
#'   filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#'   mutate(popup_text = eq_create_label(.)) %>% 
#'   eq_map(annot_col = "popup_text")}

eq_map <- function(filtered_data, annot_col = "DATE"){
  map = leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = filtered_data,
                              radius = ~ MAGNITUDE,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              popup = ~ filtered_data[[annot_col]],
                              weight = 2)
  return(map)
}