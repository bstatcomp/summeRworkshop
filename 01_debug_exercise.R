get_primes <- function (n_max) {
  if (n_max == 0) {
    return (NULL)
  } else if (n_max == 1) {
    return (NULL)
  } else if (n_max == 2) {
    return (2) 
  } else {
    all_primes <- c(2)
    for (i in 3:n_max) {
      is_prime <- TRUE
      for (j in 2:i - 1) {
        tmp <- i / j
        if (round(tmp) == tmp)
          is_prime <- FALSE
      }
      if (is_prime)
        all_primes <- c(all_primes, i)
    }
    return (all_primes)
  }
}
get_primes(10)