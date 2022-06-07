#' Get factor data
#' 
#' This function collects data from Fama and French's five factor database. It retrieves daily factors for developed markets.
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @examples
get_factor_data <- function() {
  
  url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_5_Factors_Daily_CSV.zip"
  csv_file <- "Developed_5_Factors_Daily.csv"
  
  temp <- tempfile()
  download.file(url, temp, quiet = TRUE)
  
  factors <- 
    readr::read_csv(unz(temp, csv_file), skip = 6) %>% 
    rename(date = ...1, mkt = `Mkt-RF`) %>%
    rename_at(c("SMB", "HML", "RMW", "CMA", "RF"), .funs = tolower) %>% 
    mutate(date = lubridate::ymd(date)) %>%
    mutate_if(is.numeric, funs(. / 100)) 
  
  return(factors)
}
