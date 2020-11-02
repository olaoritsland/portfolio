# last inn pakke
devtools::load_all()

library(PortfolioAnalytics)
library(ROI)


# get data ---------------------------------------------------------------------

# stocks
tickers <- c("FB",
             "AMZN",
             "AAPL",
             "NFLX",
             "GOOGL")


# get stock data
stocks <- get_stock_data(tickers) %>% 
  select(date, symbol, returns_log) %>% 
  tidyr::pivot_wider(id_cols = "date",
                     names_from = "symbol", 
                     values_from = "returns_log") %>% 
  na.omit() %>% 
  timetk::tk_xts() %>% 
  mutate_all(replace_na(0))


# define portfolio -------------------------------------------------------------

p <- portfolio.spec(assets = colnames(stocks)) %>% 
  add.objective(type = "risk", name = "var") %>%              # minimise risk
  add.constraint(type = "full_investment") %>%                # sum(weights) = 1
  add.constraint(type = "return",     return_target= 0.0035) %>% 
  add.constraint(type="long_only") %>% 
  add.objective(type = "return", name = "mean")

# optimise
portfolio <- optimize.portfolio(R = stocks, 
                                portfolio = p, 
                                optimize_method = "ROI", 
                                trace = TRUE)

opt_rebal <- optimize.portfolio.rebalancing(
  R = stocks, 
  portfolio = p, 
  optimize_method = "random", 
  rp = rp, 
  trace = TRUE, 
  search_size = 1000, 
  rebalance_on = "quarters", 
  training_period = 60, 
  rolling_window = 60)

extractObjectiveMeasures(portfolio)

