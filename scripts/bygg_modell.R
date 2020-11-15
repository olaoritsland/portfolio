
# Market cap (SMB) Small minus Big
# Book-to-market (HML) high minus low (Value vs. growth)
# Operating profitability (RMW) Robust minus weak
# (Annual change in gross PP&E + annual change in inventories) / Book value (CMA) conservative minus aggressive

devtools::load_all()


# Get financials ---------------------------------------------------------------

tickers <- c("FB",
             "AMZN",
             "AAPL",
             "NFLX",
             "GOOGL",
             "EQNR",
             "ZM")

metrics <- yahooQF(c("Ticker",
                     "Price/Book",
                     "Market Capitalization", 
                     "Book Value",
                     "Shares Outstanding",
                     "Earnings/Share"))

metrics <- getQuote(paste(tickers, sep="", collapse=";"), what = metrics)


metrics <- metrics %>% 
  tibble::rownames_to_column("ticker") %>% 
  rename(
    market_cap = `Market Capitalization`,
    book_value = `Book Value`,
    shares_outstanding = `Shares Outstanding`,
    eps = `Earnings/Share`
  ) %>% 
  mutate(
    book_value = book_value * shares_outstanding,
    book_to_market = book_value / market_cap
  )


# get factors ------------------------------------------------------------------

ant_aksjer = length(tickers)
weights = 1/ant_aksjer

# get stock data
stocks <- get_stock_data(tickers) %>% 
  select(date, symbol, returns_simple) %>% 
  tidyr::pivot_wider(names_from = "symbol",
                     values_from = "returns_simple") %>% 
  mutate(uniform_portfolio = rowMeans(select(., -date), na.rm = TRUE) / 100) %>% 
  select(date, uniform_portfolio)

# get factor data
factors <- get_factor_data()

# join data
df <- stocks %>%
  left_join(factors, by = "date") %>% 
  na.omit()


# select strategy --------------------------------------------------------------

# Split into train and test
df_split     <- initial_split(df)
df_train_raw <- training(df_split)
df_test_raw  <- testing(df_split)

# Create recipe
recipe <- 
  recipe(uniform_portfolio ~ mkt + smb + hml + rmw + cma, data = df_train_raw) %>% 
  prep()

# Bake
df_train <- bake(recipe, df_train_raw)
df_test <- bake(recipe, df_test_raw)

# Train model
model <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    uniform_portfolio ~ 
      mkt
    + smb
    + hml
    + rmw
    + cma,
    data = df_train
  )


# extract factors
model_factors <- broom::tidy(model) %>% 
  select(term, estimate) %>% 
  tidyr::pivot_wider(names_from = "term", 
                     values_from = "estimate") %>% 
  rename(intercept = `(Intercept)`)


# join factors
df_score <- metrics %>% 
  bind_cols(model_factors) %>% 
  mutate(
    score = intercept + (smb * market_cap) + (hml * book_to_market)
  ) %>% 
  arrange(score)
