# Example script
library(httpgd)
httpgd::hgd()
options(device = httpgd::hgd)


x <- rnorm(100)
y <- rnorm(100)
plot(x, y, main = "Test Plot")