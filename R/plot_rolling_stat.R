
#' Plot rolling stat
#' 
#' This function plot returns with rolling mean and median 
#' 
#' TODO: add labels for easier use with purrr::map
#'
#' @param .data 
#' @param ticker 
#' @param width length (days) of rolling stat
#'
#' @return
#' @export
#' @import tidyquant
#' @improt dplyr
#' @import tidyr pivot_longer
#' @import ggplot2
#' @importFrom ggthemes scale_color_ptol
#' 
#'
#' @examples
plot_rolling_stat <- function(.data, ticker, width = 200) {
  
  .data <- .data %>% 
    filter(symbol == {{ticker}}) %>% 
    ungroup() %>% 
    select(date, close)
  
  roll_mean_name <- paste0("roll_mean_", width)
  roll_sd_name <- paste0("roll_sd_", width)
  
  plot_data <- .data %>%
    tidyquant::tq_mutate(select = close, 
                         mutate_fun = rollapply, 
                         width = width, 
                         FUN = mean,
                         na.rm = TRUE, 
                         col_rename = roll_mean_name) %>% 
    tidyquant::tq_mutate(select = close, 
                         mutate_fun = rollapply, 
                         width = width, 
                         FUN = sd,
                         na.rm = TRUE, 
                         col_rename = roll_sd_name)
  
  
  plot_data %>% 
    tidyr::pivot_longer(-date) %>% 
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    ggthemes::scale_color_ptol()
}