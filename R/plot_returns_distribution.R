#' Plot returns distribution
#' 
#' This function plots the distribution of all symbols in data.
#' 
#' TODO: add theoretical normal distribution
#'
#' @param .data 
#' @param ticker if missing, all tickers are plotted
#' @param return_var 
#'
#' @return
#' @export
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' 
plot_returns_distribution <- function(.data, ticker, return_var) {
  
  if (missing(ticker)) {
    
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