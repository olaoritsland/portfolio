context("Test Plot Factor Returns Cum")

p <- plot_factor_returns_cum(portfolio::factors)

test_that("function returns a ggplot object", {
  expect_is(p, c("gg", "ggplot"))
})

# throws error if ...
