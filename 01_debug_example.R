set.seed(0)

a <- rnorm(100, 2, 1)
b <- NULL

for (i in 1:length(a)) {
  if (a[i] < 0) browser()
  b <- c(b, a[i])
}