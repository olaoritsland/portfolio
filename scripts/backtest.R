library(tidyverse)
library(portfolioBacktest)
​
theme_set(theme_bw())
​
# Last ned aksjepriser ----------------------------------------------------
​
# StockDataDownload er en robust wrapper rundt quantmod sin getSymbols, og 
# anbefales å bruke heller enn quantmod. 
​
# Last ned data, f.eks. SP500 de siste 3 år
# Tok meg ~2 min på jobbnettet
SP500 <- stockDataDownload(stock_symbols = SP500_symbols,
                           from = "2016-12-19", 
                           to   = "2019-12-19", 
                           local_file_path = NULL)
​
# Portfolio backtesting ---------------------------------------------------
​
# Lag liste med 10 dataset med 50 tilfeldige aksjer
my_dataset_list <- stockDataResample(SP500, 
                                     N_sample = 50, 
                                     T_sample = 252*2, 
                                     num_datasets = 10)
​
​
# Lag trading strategi (en funksjon som tar inn data og returnerer 
# investeringsvekter), her benytter jeg en fra et eksempel på nettsiden
​
library(CVXR) # for konveks optimering i Markowitz-strategien
​
Markowitz_portfolio_fun <- function(dataset) {
  
  x <- diff(log(dataset$adjusted))[-1]  # compute log returns
  
  mu <- colMeans(x)  # compute mean vector
  
  Sigma <- cov(x)  # compute the SCM
  
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  
  result <- solve(prob)
  
  return(as.vector(result$getValue(w)))
}
​
# Selve backtesting, her kan man legge til alle strategiene man vil teste i 
# portfolio_funs, og alle benchmarks man vil teste mot
​
backtest <- portfolioBacktest(portfolio_funs = list("Markowitz" = Markowitz_portfolio_fun), 
                              dataset_list = my_dataset_list, 
                              benchmark = c("uniform") # en enkel 1/n-strategi
)
​
# Vurder performance ------------------------------------------------------
​
results_summary <- backtestSummary(backtest)
​
# Lag tabell med summary
summaryTable(results_summary, 
             type = "DT", 
             order_col = "Sharpe ratio", 
             order_dir = "desc")
​
# Sammenlign strategier
summaryBarPlot(results_summary, 
               measures = c(
                 "Sharpe ratio", 
                 "Sterling ratio",
                 "max drawdown", 
                 "annual volatility", 
                 "annual return"
               ))
​
backtestBoxPlot(backtest, measure = "Sharpe ratio")
​
backtestChartCumReturns(backtest) 
​
backtestChartDrawdown(backtest)