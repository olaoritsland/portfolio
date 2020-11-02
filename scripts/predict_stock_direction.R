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

# Select stock
df_filtered <- df %>% 
  filter(symbol == "NFLX") %>% 
  mutate(direction = case_when(
    returns_simple > 0 ~ "up",
    returns_simple == 0 ~ "flat",
    returns_simple < 0 ~ "down"
    )
  ) %>% 
  select(-c(returns_simple, returns_log))

# Split into train and test
df_split     <- initial_split(df_filtered)
df_train_raw <- training(df_split)
df_test_raw  <- testing(df_split)

# Create recipe
recipe <- 
  recipe(direction ~ mkt + smb + hml + rmw + cma, data = df_train_raw) %>% 
  prep()

# Bake
df_train <- bake(recipe, df_train_raw)
df_test <- bake(recipe, df_test_raw)


# Train model
rf_mod <- rand_forest(mode = "classification", trees = 200, mtry = 2) %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(
    direction ~ 
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
  rename(estimate     = .pred_class, 
         truth        = direction)

# correct
prediction %>% mutate(is_equal = estimate == truth) %>% filter(is_equal) %>% nrow / nrow(prediction)

table(prediction$estimate, prediction$truth)

# variable importance


