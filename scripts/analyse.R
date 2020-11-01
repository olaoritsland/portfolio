# last inn pakke
devtools::load_all()


# get data ---------------------------------------------------------------------

# stocks
tickers <- c("FB",
             "AMZN",
             "AAPL",
             "NFLX",
             "GOOGL")


# get stock data
stocks <- get_stock_data(tickers)

# get factor data
factors <- get_factor_data()

# join data
df <- stocks %>%
  left_join(factors, by = "date") %>% 
  na.omit()


# eda --------------------------------------------------------------------------

# plot factor data
plot_factor_returns_cum(factors)

# plot distribution of returns
plot_returns_distribution(.data = stocks, return_var = returns_log, all_tickers = TRUE)

# Plot distribution of a single stock
plot_returns_distribution(stocks, "AMZN", returns_log)

# plot rolling mean and sd of a single stock
plot_rolling_stat(stocks, "NFLX")


# plot volatility
plot_stock_volatility(stocks, ticker = "FB", return_var = returns_simple)
plot_stock_volatility(stocks, return_var = returns_simple, all_tickers = TRUE)

