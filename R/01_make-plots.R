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
  mutate(cum_spending = cumsum(spending) / 1000)

## hatchery releases
releases <- data_raw %>%
  select(year, ends_with("rel")) %>%
  mutate(chin_rel = chin_rel / 1e6,
         coho_rel = coho_rel / 1e6,
         sock_rel = sock_rel / 1e6,
         sthd_rel = sthd_rel / 1e6)

## get model fits
model_fits <- read.csv(here::here("data", "model_fits.csv")) %>%
  filter(year >= 1980) %>%
  mutate(fitted = fitted / 1000,
         fitted_s = fitted_s / 1000,
         actual = actual / 1000) %>%
  select(year, actual, fitted, fitted_s)


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
        cex.axis = 1.4, cex.lab = 1.6)
axis(side = 2, at = c(0, 400, 800, 1200), cex.axis = 1.4)
legend(x = 1975, y = 1200, xjust = 0.3, yjust = 0.7,
       legend = c("Chinook", "Coho", "Sockeye", "Steelhead"),
       col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       text.col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       bty = "n", lty = "solid", pch = 16, lwd = 2,
       y.intersp = 1.2)

dev.off()


## annual & cumulative spending

## annual
png(file = here::here("presentation", "figs", "annual_spending.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
plot(spending[,"year"], spending[,"spending"],
        type = "o", lty = "solid", pch = 16, lwd = 2,
        col = "#008837",
        xlab = "Year", ylab = "Spending (millions)",
        cex.axis = 1.4, cex.lab = 1.6)

dev.off()

## cumulative
png(file = here::here("presentation", "figs", "cum_spending.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
plot(spending[,"year"], spending[,"cum_spending"],
     type = "o", lty = "solid", pch = 16, lwd = 2,
     col = "#008837",
     xlab = "Year", ylab = "Cumulative spending (billions)",
     cex.axis = 1.4, cex.lab = 1.6)

dev.off()


## hatchery releases

png(file = here::here("presentation", "figs", "releases.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
matplot(releases[,"year"], releases[,-1],
        type = "o", lty = "solid", pch = 16, lwd = 2,
        col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
        xlab = "Year", ylab = "Releases (millions)",
        cex.axis = 1.4, cex.lab = 1.6)
legend(x = 2010, y = 30, xjust = 0.5, yjust = 0.5,
       legend = c("Chinook", "Coho", "Sockeye", "Steelhead"),
       col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       text.col = c("#a6611a", "#2b83ba", "#ca0020", "#5e3c99"),
       bty = "n", lty = "solid", pch = 16, lwd = 2,
       y.intersp = 1.2)

dev.off()


## model fits 

png(file = here::here("presentation", "figs", "model_fits.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
matplot(model_fits[,"year"], model_fits[,-1],
        type = "o", lty = "solid", pch = 16, lwd = 2,
        col = c("#5e3c99", "#2b83ba", "#ca0020"),
        xlab = "Year", ylab = "Returns (1000s)",
        ylim = range(model_fits[,-1], na.rm = TRUE),
        cex.axis = 1.4, cex.lab = 1.6)
legend(x = 1985, y = 2000, xjust = 0.3, yjust = 0.5,
       legend = c("Actual", "Model fit", "Model fit - releases"),
       col = c("#5e3c99", "#2b83ba", "#ca0020"),
       text.col = c("#5e3c99", "#2b83ba", "#ca0020"),
       bty = "n", lty = "solid", pch = 16, lwd = 2,
       y.intersp = 1.2)

dev.off()


## model fits minus releases

png(file = here::here("presentation", "figs", "model_fits_h.png"),
    width = 8, height = 5, units = "in", res = 300)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0.1, 0.1, 0.1, 0.1))
matplot(model_fits[,"year"], model_fits[,c("actual", "fitted")],
        type = "o", lty = "solid", pch = 16, lwd = 2,
        col = c("#5e3c99", "#2b83ba"),
        xlab = "Year", ylab = "Returns (1000s)",
        ylim = range(model_fits[,-1], na.rm = TRUE),
        cex.axis = 1.4, cex.lab = 1.6)
legend(x = 1985, y = 2000, xjust = 0.3, yjust = 0.5,
       legend = c("Actual", "Model fit", "Model fit - releases"),
       col = c("#5e3c99", "#2b83ba", "white"),
       text.col = c("#5e3c99", "#2b83ba", "white"),
       bty = "n", lty = "solid", pch = 16, lwd = 2,
       y.intersp = 1.2)

dev.off()
