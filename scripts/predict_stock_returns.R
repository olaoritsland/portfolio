# last inn pakke
devtools::load_all()

library(tidymodels)

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


#  --------------------------------------------------------------

# Split into train and test
df_split     <- initial_split(df)
df_train_raw <- training(df_split)
df_test_raw  <- testing(df_split)

# Create recipe
recipe <- 
  recipe(returns_log ~ mkt + smb + hml + rmw + cma, data = df_train_raw) %>% 
  prep()

# Bake
df_train <- bake(recipe, df_train_raw)
df_test <- bake(recipe, df_test_raw)


# Train model
rf_mod <- rand_forest(mode = "regression", trees = 200, mtry = 2) %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(
    returns_log ~ 
      mkt
    + smb
    + hml
    + rmw
    + cma,
    data = df_train
  )

# summary
rf_mod$fit

# predictions
prediction <- predict(rf_mod, df_test) %>% 
  bind_cols(df_test) %>% 
  rename(estimate     = .pred, 
         truth        = returns_log)


# variable importance
rf_mod %>% 
  purrr::pluck("fit") %>% 
  purrr::pluck("variable.importance") %>% 
  enframe() %>% 
  ggplot() +
  aes(x = reorder(name, value), y = value) +
  geom_bar(fill = "forestgreen",
           color = "black",
           stat = "identity",
           alpha = .5) +
  coord_flip()

