#' Plot stock volatility
#' 
#' This function plots the daily stock returns 
#'
#' @param .data 
#' @param ticker 
#' @param return_var
#' @param all_tickers boolean
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#'
#' @examples
plot_stock_volatility <- function(.data, ticker, return_var, all_tickers = FALSE) {
  
  if (all_tickers) {
    
    .data %>% 
      ggplot(aes(date, {{return_var}})) +
      geom_bar(stat = 'identity') +
      facet_grid(symbol ~ ., scales = "free")
    
    
  } else {
  
  .data %>% 
    filter(symbol == {{ticker}}) %>% 
    ggplot(aes(date, {{return_var}})) +
    geom_bar(stat = 'identity')
    
  }
  
}