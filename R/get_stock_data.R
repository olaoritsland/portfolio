
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
#' 
get_stock_data <- function(tickers) {
  
  tickers %>% 
    tidyquant::tq_get() %>% 
    select(symbol, date, adjusted) %>%
    group_by(symbol) %>% 
    arrange(date) %>% 
    mutate(returns_simple = adjusted - lag(adjusted),
           returns_log = log(adjusted) - log(lag(adjusted))) %>% 
    ungroup()
  
}
