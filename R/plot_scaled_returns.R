
#' Plot scaled returns
#' 
#' Todo: create tests
#'
#' @param data 
#' @param id 
#' @param price_var 
#' @param start_date 
#' @param date_var 
#'
#' @return
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
plot_scaled_returns <- function(data, 
                                id = symbol,
                                price_var = adjusted, 
                                start_date, 
                                date_var = date) {
  
  min_date <- min(data %>% pull({{date_var}}))
  
  if (start_date < min_date) {
    stop('start_date' < 'min_date')
  }
  
  data %>% 
    filter({{date_var}} >= start_date) %>% 
    group_by({{id}}) %>% 
    arrange({{date_var}}) %>% 
    mutate(
      scaled_price = if_else({{date_var}} == start_date, 
                             100, 
                             {{price_var}} / lag({{price_var}})) %>% cumprod) %>% 
    ungroup() %>% 
  
  
  ggplot() +
  aes(x = {{date_var}}, y = scaled_price, colour = {{id}}) +
  geom_line()
  
  
}
