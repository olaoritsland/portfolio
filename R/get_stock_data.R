
#' Get stock data
#' 
#' This function collects market data for the specified tickers and computes both simple and logarithmic returns.
#'
#' @param tickers 
#'
#' @return
#' @export
#' @importFrom tidyquant tq_get
#' @import dplyr
#'
#' @examples
#' \dontrun {
#' get_stock_data("APPL")
#' }
#' 
get_stock_data <- function(tickers){
  
  tickers %>% 
    tidyquant::tq_get() %>% 
    select(symbol, date, close) %>%
    group_by(symbol) %>% 
    arrange(date) %>% 
    mutate(returns_simple = close - lag(close),
           returns_log = log(close) - log(lag(close))) %>% 
    ungroup()
  
}
