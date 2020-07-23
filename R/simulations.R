#' Main simulation function
#'
#'
#'
#'


simulate <- function(k, delta, tau,
                     qrpEnv = qrpEnv = c("none", "low", "medium", "high"),
                     censorFunc = c("none", "medium", "high"),
                     verbose = FALSE,
                     fixed.n = NULL,
                     seq_t = list(pr, seq_param)
                     ) {

  this_MA = genMA(k, delta, tau, fixed.n)

  #TODO Generate sample
  #TODO Calculate stats, tvalue, seq_tvalue, bayesian
  #TODO Add Questionable Research Practices
  #TODO Add Publication Bias


}
