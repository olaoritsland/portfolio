
#' Plot factor returns cumulative
#' 
#' This function plots daily cumulative returns of market factors.
#'
#' @param .data 
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
plot_factor_returns_cum <- function(.data) {
  
  .data %>% 
    mutate_if(is.numeric, cumsum) %>% 
    tidyr::pivot_longer(-date) %>%  
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line()
  
}