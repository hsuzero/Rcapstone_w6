#' Create Map Label
#'
#' This function creates content to be displayed inside of the popups plotted by the eq_map function.
#'
#' @param filtered_data DataFrame with information about earthquakes, must have LOCATION,
#'                      EQ_PRIMARY, and TOTAL_DEATHS columns
#'                      
#' @return Vector with content to be used by eq_map function.
#'
#' @examples
#'
#' @examples
#' \dontrun{eq_create_label(.)}


eq_create_label <- function(filtered_data){
  
  popup_text = ""
  popup_text = paste0(popup_text, ifelse(!is.na(filtered_data$LOCATION),
                                         paste0("<b>Location: </b>", 
                                                filtered_data$LOCATION, 
                                                "<br>"), ""))
  popup_text = paste0(popup_text, ifelse(!is.na(filtered_data$MAGNITUDE),
                                         paste0("<b>Magnitude: </b>", 
                                                filtered_data$MAGNITUDE, 
                                                "<br>"), ""))
  popup_text = paste0(popup_text, ifelse(!is.na(filtered_data$DEATH_COUNT),
                                         paste0("<b>Total Deaths: </b>", 
                                                filtered_data$DEATH_COUNT, 
                                                "<br>"), ""))
  
  return(popup_text)
}