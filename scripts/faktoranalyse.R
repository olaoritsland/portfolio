# last inn pakken
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


# select strategy --------------------------------------------------------------

# Select stock
df_filtered <- df %>% 
  filter(symbol == "FB")

# Split into train and test
df_split     <- initial_split(df_filtered)
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
model <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    returns_log ~ 
      mkt
    + smb
    + hml
    + rmw
    + cma,
    data = df_train
  )

# Test model
summary(model$fit)

prediction <- predict(model, df_test) %>%
  bind_cols(df_test_raw) %>%
  rename(estimate     = .pred,
         truth        = returns_log) %>%
  mutate(
    dev = truth - estimate,
    abs_dev = abs(truth - estimate),
    abs_dev_perc = abs_dev / truth
  )

# Evaluate model
multi_metric <- yardstick::metric_set(mape, rmse, mae, rsq)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)

# Pdp plot
model$fit %>%
  pdp::partial(pred.var = "mkt", train = df_train) %>%
  autoplot()

# Plot predictions vs. truth
plot_pred_truth_dist(.data = prediction)

plot_pred_truth_dist <- function(.data = prediction) {
  
  .data %>% 
    select(truth, estimate, date) %>% 
    pivot_longer(-date) %>% 
    ggplot(aes(value, fill = name)) +
    geom_density(alpha = .3) 
  
}
