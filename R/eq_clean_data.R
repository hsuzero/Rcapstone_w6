#' eq_clean_data
#'
#' You can use this function in two ways. Assigning the file_name (e.g. ../path/my_file.txt) with path or piping it
#' as sequence of a read_delim.
#'
#' @param file You can insert the file_name and path to reach the tsv file or pipe it with a read_delim.
#'
#' @return A DataFrame version of file_name. Have in mind, I have only cleaning few columns:
#'         EQ_PRIMARY, LOCATION_NAME, DATE, and TOTAL_DEATHS.
#'
#' @importFrom readr read_delim
#' @importFrom dplyr select mutate rename filter
#' @importFrom lubridate dmy
#'
#' @examples
#'
#' \dontrun{
#' # Loading using a third party function and then cleaning teh DataFrame.
#' readr::read_delim("inst/extdata/signif.txt",
#'                   delim = "\t") %>%
#'                           eq_clean_data()
#'
#' # Piping a DataFrame.
#' any_dataframe %>% eq_clean_data()
#'
#' # Assigning a file_name
#' eq_clean_data(file_name = "my_folder/signif.txt")}
#'
#' @export

eq_clean_data <- function(file){
  raw_data <- tryCatch({
    readr::read_delim(file, delim = "\t")
  }, error = function(e){
    print(paste("File", file, "does not exist. Please check."))
    return(NULL)
  })
  
  if(!is.null(raw_data)){
    clean_data <- raw_data %>% 
      dplyr::select(YEAR, MONTH, DAY, LATITUDE, LONGITUDE, COUNTRY, LOCATION_NAME,
                    EQ_PRIMARY, INTENSITY, TOTAL_DEATHS) %>%
      dplyr::filter(YEAR >= 1900, MONTH >= 1, DAY >= 1,
                    !is.na(EQ_PRIMARY), !is.na(INTENSITY)) %>%
      dplyr::mutate(DATE = lubridate::dmy(paste(DAY, MONTH, YEAR, sep = "/")),
                    LATITUDE = as.numeric(LATITUDE),
                    LONGITUDE = as.numeric(LONGITUDE),
                    EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                    INTENSITY = as.numeric(INTENSITY),
                    TOTAL_DEATHS = ifelse(is.na(TOTAL_DEATHS), 0, TOTAL_DEATHS),
                    TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)) %>%
      dplyr::rename(LOCATION = LOCATION_NAME, 
                    MAGNITUDE = EQ_PRIMARY,
                    DEATH_COUNT = TOTAL_DEATHS) %>%
      dplyr::select(DATE, LATITUDE, LONGITUDE, COUNTRY, LOCATION, 
                    MAGNITUDE, INTENSITY, DEATH_COUNT)
  }else{
    clean_data <- raw_data
  }
  
  return(clean_data)
}