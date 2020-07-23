#' Generate a single group of metanalyses with the maximum number of participants
#'
#' @param k number of studies in the metanalysis
#' @param delta true effect of the studies
#' @param tau the SD around the true effect
#'
#' @export

genMA <- function(k, delta, tau, fixed.n = NULL) {
  if(is.null(fixed.n)){
    ns <- getN(k)
  }else{
    ns <- rep(fixed.n, k)
  }

  t <- tibble(ns=ns, opt_delta=delta, opt_tau=tau, emp_tau = rnorm(k, 0, tau), emp_delta = opt_delta + emp_tau)
  t <- mutate(t, s = mapply(function(x,emp_delta){rnorm(x, mean=emp_delta)}, x=ns, emp_delta=emp_delta, SIMPLIFY = F))
  return(t)
}

#' Simulate a sample size
#'
#' Legacy Meta-Showdown
#'
#' get a simulated per-group sample size that follows the distribution of empirical sample sizes
# (see folder "Empirical sample size distributions")
# min = minimum sample size, max = maximum sample size
# max = 1905 corresponds to the largest observed per-group sample size in Marszalek et al.
#' @export


getN <- function(k = 1, min.n = 5, max.n = 1905, shape = 1.15326986, scale = 0.04622745) {
  # library(invgamma)
  ns <- round(rtrunc(n = k, spec = "invgamma", a = min.n, b = max.n, shape = shape, scale = scale))
  return(ns)
}
