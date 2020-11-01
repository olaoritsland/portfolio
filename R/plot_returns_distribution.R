#' Plot returns distribution
#' 
#' This function plots the distribution of all symbols in data.
#'
#' @param .data 
#' @param ticker
#' @param return_var 
#' @param all_tickers boolean
#'
#' @return
#' @export
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' 
plot_returns_distribution <- function(.data, ticker, return_var, all_tickers = FALSE) {
  
  if (all_tickers) {
    
    .data %>% 
      ggplot(aes({{return_var}})) + 
      geom_density(fill = 'forestgreen', alpha = .2) +
      facet_wrap(. ~ symbol, scales = "free")
    
  } else {
  
  mean <- .data %>% 
    filter(symbol == {{ticker}}) %>% 
    summarise(mean = mean({{return_var}}, na.rm = T)) %>% 
    pull(mean)
  
  .data %>% 
    filter(symbol == {{ticker}}) %>% 
    ggplot(aes({{return_var}})) + 
    geom_density(fill = 'forestgreen', alpha = .2) + 
    geom_vline(xintercept = mean, linetype = "dashed") 
  #geom_vline(xintercept = sd(returns$returns, na.rm = T)) 
  }
  
}