library(quantreg)
library(tidyverse)

f <- function(x) ((x+4) * (x+1) * (x-1) * (x-3)) / 14 + 0.5
f_prime <- function(x) (4 * x**3 + 3 * x**2 - 26 * x - 1) / 14
f_prime_prime <- function(x) (6 * x**2 + 3 * x - 13) / 7


p <- tibble(
         x = seq(-4, 4, 0.01),
         y = f(x)
     ) %>%
     ggplot(mapping = aes(x = x, y = y)) +
        geom_line() +
        theme_minimal() + xlab("x") + ylab("y")

ggsave('local_vs_global_minima.pdf', p)


p <- tibble(
         x = seq(-4, 4, 0.01),
         y = f(x)
     ) %>%
     ggplot(mapping = aes(x = x, y = y)) +
        geom_line() +
        geom_segment(mapping = aes(x = -1, y = f(-1),
                                   xend = -2, yend = f(-1) - f_prime(-1)),
                     arrow = arrow(), color = "red") +
        theme_minimal() + xlab("x") + ylab("y")

ggsave('derivative.pdf', p)


p <- tibble(
         x = seq(-4, 4, 0.01)
     ) %>%
     ggplot(mapping = aes(x = x)) +
        geom_line(mapping = aes(y = x**2)) +
        geom_line(mapping = aes(y = x + 2), color = "red") +
        theme_minimal() + xlab("x") + ylab("y")

ggsave('convex_function.pdf', p)


p <- tibble(
         x = seq(-2, 8, 0.01)
     ) %>%
     ggplot(mapping = aes(x = x)) +
        geom_polygon(data = tibble(x = c(2, -1, 6), y = c(6, -3, 4)),
                     mapping = aes(x = x, y = y), fill = "#CFD8DC") +
        geom_line(mapping = aes(y = 3*x)) +
        geom_line(mapping = aes(y = -x/2 + 7)) +
        geom_line(mapping = aes(y = x - 2)) +
        geom_line(mapping = aes(y = (-3*x + 18) / 4), color = "#FFCDD2") +
        geom_line(mapping = aes(y = (-3*x + 22) / 4), color = "#E57373") +
        geom_line(mapping = aes(y = (-3*x + 26) / 4), color = "#F44336") +
        geom_line(mapping = aes(y = (-3*x + 30) / 4), color = "#D32F2F") +
        geom_line(mapping = aes(y = (-3*x + 34) / 4), color = "#B71C1C") +
        geom_point(data = tibble(x = 6, y = 4),
                   mapping = aes(x = x, y = y), color = "#F44336") +
        theme_minimal() + xlab("x") + ylab("y")

ggsave('lp.pdf', p)


set.seed(42)
p <- tibble(
         x = seq(0, 100, 0.1),
         y = 5 + 0.1 * x + rnorm(length(x), 0, 0.1 + 0.05 * x)
     ) %>%
     ggplot(mapping = aes(x = x, y = y)) +
        geom_point(color = "#9E9E9E") +
        geom_quantile(quantiles = seq(0.1, 0.9, 0.1), color = "#F44336") +
        theme_minimal() + xlab("x") + ylab("y")

ggsave('quantile_regression.pdf', p)

