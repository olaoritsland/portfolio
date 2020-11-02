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


# eda --------------------------------------------------------------------------

# plot factor data
plot_factor_returns_cum(factors)

# plot distribution of returns
plot_returns_distribution(.data = stocks, return_var = returns_log)

# Plot distribution of a single stock
plot_returns_distribution(stocks, "AMZN", returns_log)

# plot rolling mean and sd of a single stock
plot_rolling_stat(stocks, "NFLX")


# plot volatility
plot_stock_volatility(stocks, return_var = returns_simple)

# plot volatility of single stock
plot_stock_volatility(stocks, ticker = "FB", return_var = returns_simple)
