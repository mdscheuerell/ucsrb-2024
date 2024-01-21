## script to create plots for Columbia River spending evaluation

#### setup ####

library(dplyr)


#### load & munge data ####

## get data file
data_raw <- read.csv(here::here("data", "columbia_spending.csv"))

## adult returns
returns <- data_raw %>%
  select(year, ends_with("returns")) %>%
  mutate(chin_returns = chin_returns / 1000,
         coho_returns = coho_returns / 1000,
         sock_returns = sock_returns / 1000,
         sthd_returns = sthd_returns / 1000)

## annual & cumulative spending
spending <- data_raw %>%
  select(year, spending) %>%
  mutate(cum_spending = cumsum(spending))

## annual & cumulative spending
releases <- data_raw %>%
  select(year, ends_with("rel")) %>%
  mutate(chin_rel = chin_rel / 1e6,
         coho_rel = coho_rel / 1e6,
         sock_rel = sock_rel / 1e6,
         sthd_rel = sthd_rel / 1e6)


#### make plots ####

## adult returns

png(file = here::here("presentation", "figs", "adult_returns.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
matplot(returns[,"year"], returns[,-1],
        type = "o", lty = "solid", pch = 16, lwd = 2,
        col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
        yaxt = "n",
        xlab = "Year", ylab = "Returns (1000s)",
        cex.axis = 1.2, cex.lab = 1.5)
axis(side = 2, at = c(0, 400, 800, 1200), cex.axis = 1.2)
legend(x = 1975, y = 1200, xjust = 0.3, yjust = 0.7,
       legend = c("Chinook", "Coho", "Sockeye", "Steelhead"),
       col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       text.col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       bty = "n", lty = "solid", pch = 16, lwd = 2,
       y.intersp = 1.2)

dev.off()

## annual & cumulative spending



## annual & cumulative spending

 