#' Clean Data (Location)
#' 
#' This function is a intermediate step to clean the raw data. The `eq_clean_data` will use it behind
#' the scenes to insert the column LOCATION.
#' 
#' @param clean_data A dataframe object returned by the 
#'   \code{eq_clean_data} function
#'
#' @return Adds a new column to the DataFrame called LOCATION, following the specifications:
#'         Title Case and without country name.
#' 
#' @importFrom dplyr select mutate_
#' @importFrom lubridate dmy
#' @importFrom stringr str_replace str_to_title
#' 
#' @export
#'
#' @examples
#' \dontrun{eq_location_clean(clean_data)}

eq_location_clean <- function(clean_data){
  if(is.data.frame(clean_data)){
    clean_data <- tryCatch({
      clean_data %>%
        dplyr::mutate_(LOCATION = ~LOCATION %>%
                         stringr::str_replace(paste0(COUNTRY, ": "), "") %>%
                         stringr::str_to_title())
    }, error = function(e){
      print(paste("Invalid dataframe object. Columns should include",
                  "DATE, LATITUDE, LONGITUTE, COUNTRY, LOCATION,",
                  "MAGNITUDE, INTENSITY, DEATH_COUNT"))
      return(NULL)
    })
  }else{
    print("Invalid variable type. Should be dataframe object.")
    return(NULL)
  }
  
  return(clean_data)
}
