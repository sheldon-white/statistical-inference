library(ggplot2)
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
    x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                       nosim), 1, cfunc, 10)
    ),
    size = factor(rep(c(10), rep(nosim, 1))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
print(g)